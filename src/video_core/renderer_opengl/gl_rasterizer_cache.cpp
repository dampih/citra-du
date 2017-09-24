// Copyright 2015 Citra Emulator Project
// Licensed under GPLv2 or any later version
// Refer to the license.txt file included.

#include <algorithm>
#include <atomic>
#include <cstring>
#include <iterator>
#include <unordered_set>
#include <utility>
#include <vector>
#include <boost/range.hpp>
#include <glad/glad.h>
#include "common/bit_field.h"
#include "common/color.h"
#include "common/logging/log.h"
#include "common/math_util.h"
#include "common/microprofile.h"
#include "common/scope_exit.h"
#include "common/vector_math.h"
#include "core/frontend/emu_window.h"
#include "core/memory.h"
#include "core/settings.h"
#include "video_core/pica_state.h"
#include "video_core/renderer_opengl/gl_rasterizer_cache.h"
#include "video_core/renderer_opengl/gl_state.h"
#include "video_core/texture/texture_decode.h"
#include "video_core/utils.h"
#include "video_core/video_core.h"

using SurfaceType = SurfaceParams::SurfaceType;
using PixelFormat = SurfaceParams::PixelFormat;

static std::array<OGLFramebuffer, 2> transfer_framebuffers;

struct FormatTuple {
    GLint internal_format;
    GLenum format;
    GLenum type;
};

static constexpr std::array<FormatTuple, 5> fb_format_tuples = {{
    {GL_RGBA8, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8},     // RGBA8
    {GL_RGB8, GL_BGR, GL_UNSIGNED_BYTE},              // RGB8
    {GL_RGB5_A1, GL_RGBA, GL_UNSIGNED_SHORT_5_5_5_1}, // RGB5A1
    {GL_RGB565, GL_RGB, GL_UNSIGNED_SHORT_5_6_5},     // RGB565
    {GL_RGBA4, GL_RGBA, GL_UNSIGNED_SHORT_4_4_4_4},   // RGBA4
}};

static constexpr std::array<FormatTuple, 4> depth_format_tuples = {{
    {GL_DEPTH_COMPONENT16, GL_DEPTH_COMPONENT, GL_UNSIGNED_SHORT}, // D16
    {},
    {GL_DEPTH_COMPONENT24, GL_DEPTH_COMPONENT, GL_UNSIGNED_INT},   // D24
    {GL_DEPTH24_STENCIL8, GL_DEPTH_STENCIL, GL_UNSIGNED_INT_24_8}, // D24S8
}};

static constexpr FormatTuple tex_tuple = { GL_RGBA8, GL_RGBA, GL_UNSIGNED_BYTE };

static const FormatTuple& GetFormatTuple(PixelFormat pixel_format) {
    const SurfaceType type = SurfaceParams::GetFormatType(pixel_format);
    if (type == SurfaceType::Color) {
        ASSERT((size_t)pixel_format < fb_format_tuples.size());
        return fb_format_tuples[(unsigned int)pixel_format];
    }
    else if (type == SurfaceType::Depth || type == SurfaceType::DepthStencil) {
        size_t tuple_idx = (size_t)pixel_format - 14;
        ASSERT(tuple_idx < depth_format_tuples.size());
        return depth_format_tuples[tuple_idx];
    }
    else {
        return tex_tuple;
    }
}

template <typename Map, typename Interval>
constexpr auto RangeFromInterval(Map& map, const Interval& interval) {
    return boost::make_iterator_range(map.equal_range(interval));
}

enum MortonCopyFlags : int {
    MortonToGl = (1 << 0),
    CheckRange = (1 << 1),
    D24S8Format = (1 << 2),
    BytesPerPixelBits = 3, // bits 3-4
    GLBytesPerPixelBits = 5, // bits 5-6
    MaxValue = (1 << 7) - 1,
};

template <int flags>
static void MortonCopyPixels(u32 width, u32 height, const u8* in_data, u8* out_data, PAddr base, PAddr start, PAddr end) {
    constexpr bool check_range = (flags & MortonCopyFlags::CheckRange) ? true : false;
    constexpr bool morton_to_gl = (flags & MortonCopyFlags::MortonToGl) ? true : false;

    constexpr bool D24S8format = (flags & MortonCopyFlags::D24S8Format) ? true : false;

    constexpr u32 bytes_per_pixel = u32(((flags) >> MortonCopyFlags::BytesPerPixelBits) & 0x3) + 1; // 2bits, starting with value 1
    constexpr u32 gl_bytes_per_pixel = u32(((flags) >> MortonCopyFlags::GLBytesPerPixelBits) & 0x3) + 1; // 2bits, starting with value 1

    if (check_range)
        ASSERT(start >= base && end <= (base + (width * height * bytes_per_pixel)));
    const u32 start_offset = start - base;
    const u32 end_offset = end - base;

    for (u32 x = 0; x < width; ++x) {
        for (u32 y = 0; y < height; ++y) {
            const u32 coarse_x = x & ~7;
            const u32 coarse_y = y & ~7;
            u32 morton_offset = VideoCore::GetMortonOffset(x, y, bytes_per_pixel) + coarse_y * width * bytes_per_pixel;
            u32 gl_pixel_index = (x + (height - 1 - y) * width) * gl_bytes_per_pixel;

            if (check_range) {
                if (morton_offset >= end_offset && coarse_x == 0 && coarse_y == 0) // Out of range and new tile
                    return;
                if (morton_offset < start_offset || morton_offset >= end_offset) // Out of range
                    continue;
            }

            const size_t copy_bytes = check_range ? std::min(end_offset - morton_offset, bytes_per_pixel) : bytes_per_pixel;

            const u8* const in_ptr = &in_data[morton_to_gl ? morton_offset : gl_pixel_index];
            u8* const out_ptr = &out_data[morton_to_gl ? gl_pixel_index : morton_offset];

            if (D24S8format) {
                // Swap depth and stencil value ordering since 3DS does not match OpenGL
                constexpr size_t swap_offset = morton_to_gl ? 3 : 1;
                std::array<u8, 4> swap_buf;
                std::memcpy(&swap_buf[4 - swap_offset], &in_ptr[0], swap_offset);
                std::memcpy(&swap_buf[0], &in_ptr[swap_offset], 4 - swap_offset);
                std::memcpy(out_ptr, &swap_buf[0], copy_bytes);
            }
            else {
                std::memcpy(out_ptr, in_ptr, copy_bytes);
            }
        }
    }
}

template <size_t size>
class FunctionTable {
public:
    FunctionTable() {
        FillArray<size>();
    }
    const auto& operator [](size_t pos) const {
        return table[pos];
    }
private:
    template <size_t P>
    void FillArray() {
        table[P - 1] = &MortonCopyPixels<P - 1>;
        FillArray<P - 1>();
    }
    template <>
    void FillArray<0>() {}
    std::array<decltype(&MortonCopyPixels<0>), size> table;
};
static const FunctionTable<MortonCopyFlags::MaxValue + 1> MortonCopyFnTable;

// Allocate an uninitialized texture of appropriate size and format for the surface
static void AllocateSurfaceTexture(GLuint texture, const FormatTuple& format_tuple, u32 width, u32 height) {
    OpenGLState cur_state = OpenGLState::GetCurState();

    // Keep track of previous texture bindings
    GLuint old_tex = cur_state.texture_units[0].texture_2d;
    cur_state.texture_units[0].texture_2d = texture;
    cur_state.Apply();
    glActiveTexture(GL_TEXTURE0);

    glTexImage2D(GL_TEXTURE_2D, 0, format_tuple.internal_format, width, height, 0,
        format_tuple.format, format_tuple.type, nullptr);

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAX_LEVEL, 0);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

    // Restore previous texture bindings
    cur_state.texture_units[0].texture_2d = old_tex;
    cur_state.Apply();
}

static bool BlitTextures(GLuint src_tex, const MathUtil::Rectangle<int>& src_rect,
                         GLuint dst_tex, const MathUtil::Rectangle<int>& dst_rect,
                         SurfaceType type) {
    OpenGLState cur_state = OpenGLState::GetCurState();

    OpenGLState prev_state = cur_state;
    SCOPE_EXIT({ prev_state.Apply(); });

    // Make sure textures aren't bound to texture units, since going to bind them to framebuffer
    // components
    OpenGLState::ResetTexture(src_tex);
    OpenGLState::ResetTexture(dst_tex);

    // Keep track of previous framebuffer bindings
    cur_state.draw.read_framebuffer = transfer_framebuffers[0].handle;
    cur_state.draw.draw_framebuffer = transfer_framebuffers[1].handle;
    cur_state.Apply();

    u32 buffers = 0;

    if (type == SurfaceType::Color || type == SurfaceType::Texture) {
        glFramebufferTexture2D(GL_READ_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, src_tex,
                               0);
        glFramebufferTexture2D(GL_READ_FRAMEBUFFER, GL_DEPTH_STENCIL_ATTACHMENT, GL_TEXTURE_2D, 0,
                               0);

        glFramebufferTexture2D(GL_DRAW_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, dst_tex,
                               0);
        glFramebufferTexture2D(GL_DRAW_FRAMEBUFFER, GL_DEPTH_STENCIL_ATTACHMENT, GL_TEXTURE_2D, 0,
                               0);

        buffers = GL_COLOR_BUFFER_BIT;
    } else if (type == SurfaceType::Depth) {
        glFramebufferTexture2D(GL_READ_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, 0, 0);
        glFramebufferTexture2D(GL_READ_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_TEXTURE_2D, src_tex, 0);
        glFramebufferTexture2D(GL_READ_FRAMEBUFFER, GL_STENCIL_ATTACHMENT, GL_TEXTURE_2D, 0, 0);

        glFramebufferTexture2D(GL_DRAW_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, 0, 0);
        glFramebufferTexture2D(GL_DRAW_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_TEXTURE_2D, dst_tex, 0);
        glFramebufferTexture2D(GL_DRAW_FRAMEBUFFER, GL_STENCIL_ATTACHMENT, GL_TEXTURE_2D, 0, 0);

        buffers = GL_DEPTH_BUFFER_BIT;
    } else if (type == SurfaceType::DepthStencil) {
        glFramebufferTexture2D(GL_READ_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, 0, 0);
        glFramebufferTexture2D(GL_READ_FRAMEBUFFER, GL_DEPTH_STENCIL_ATTACHMENT, GL_TEXTURE_2D,
                               src_tex, 0);

        glFramebufferTexture2D(GL_DRAW_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, 0, 0);
        glFramebufferTexture2D(GL_DRAW_FRAMEBUFFER, GL_DEPTH_STENCIL_ATTACHMENT, GL_TEXTURE_2D,
                               dst_tex, 0);

        buffers = GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT;
    }

    glBlitFramebuffer(src_rect.left, src_rect.top, src_rect.right, src_rect.bottom, dst_rect.left,
                      dst_rect.top, dst_rect.right, dst_rect.bottom, buffers,
                      buffers == GL_COLOR_BUFFER_BIT ? GL_LINEAR : GL_NEAREST);

    return true;
}

static bool FillSurface(const Surface& surface, const u8* fill_data) {
    OpenGLState cur_state = OpenGLState::GetCurState();

    OpenGLState prev_state = cur_state;
    SCOPE_EXIT({ prev_state.Apply(); });

    OpenGLState::ResetTexture(surface->texture.handle);

    cur_state.draw.draw_framebuffer = transfer_framebuffers[1].handle;
    cur_state.Apply();

    if (surface->type == SurfaceType::Color || surface->type == SurfaceType::Texture) {
        glFramebufferTexture2D(GL_DRAW_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, surface->texture.handle, 0);
        glFramebufferTexture2D(GL_DRAW_FRAMEBUFFER, GL_DEPTH_STENCIL_ATTACHMENT, GL_TEXTURE_2D, 0, 0);

        Pica::Texture::TextureInfo tex_info{};
        tex_info.format = static_cast<Pica::TexturingRegs::TextureFormat>(surface->pixel_format);
        Math::Vec4<u8> color = Pica::Texture::LookupTexture(fill_data, 0, 0, tex_info);

        std::array<GLfloat, 4> color_values = {
            color.x / 255.f,
            color.y / 255.f,
            color.z / 255.f,
            color.w / 255.f
        };

        cur_state.color_mask.red_enabled = GL_TRUE;
        cur_state.color_mask.green_enabled = GL_TRUE;
        cur_state.color_mask.blue_enabled = GL_TRUE;
        cur_state.color_mask.alpha_enabled = GL_TRUE;
        cur_state.Apply();
        glClearBufferfv(GL_COLOR, 0, &color_values[0]);
    }
    else if (surface->type == SurfaceType::Depth) {
        glFramebufferTexture2D(GL_DRAW_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, 0, 0);
        glFramebufferTexture2D(GL_DRAW_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_TEXTURE_2D, surface->texture.handle, 0);
        glFramebufferTexture2D(GL_DRAW_FRAMEBUFFER, GL_STENCIL_ATTACHMENT, GL_TEXTURE_2D, 0, 0);

        u32 value_32bit = 0;
        GLfloat value_float;

        if (surface->pixel_format == SurfaceParams::PixelFormat::D16) {
            std::memcpy(&value_32bit, fill_data, 2);
            value_float = value_32bit / 65535.0f; // 2^16 - 1
        }
        else if (surface->pixel_format == SurfaceParams::PixelFormat::D24) {
            std::memcpy(&value_32bit, fill_data, 3);
            value_float = value_32bit / 16777215.0f; // 2^24 - 1
        }

        cur_state.depth.write_mask = GL_TRUE;
        cur_state.Apply();
        glClearBufferfv(GL_DEPTH, 0, &value_float);
    }
    else if (surface->type == SurfaceType::DepthStencil) {
        glFramebufferTexture2D(GL_DRAW_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, 0, 0);
        glFramebufferTexture2D(GL_DRAW_FRAMEBUFFER, GL_DEPTH_STENCIL_ATTACHMENT, GL_TEXTURE_2D, surface->texture.handle, 0);

        u32 value_32bit;
        std::memcpy(&value_32bit, fill_data, 4);

        GLfloat value_float = (value_32bit & 0xFFFFFF) / 16777215.0f; // 2^24 - 1
        GLint value_int = (value_32bit >> 24);

        cur_state.depth.write_mask = GL_TRUE;
        cur_state.stencil.write_mask = -1;
        cur_state.Apply();
        glClearBufferfi(GL_DEPTH_STENCIL, 0, value_float, value_int);
    }
    return true;
}

SurfaceParams::SurfaceParams() {
    // Set the internal resolution, assume the same scaling factor for top and bottom screens
    float resolution_scale_factor = Settings::values.resolution_factor;
    if (resolution_scale_factor == 0.0f) {
        // Auto - scale resolution to the window size
        resolution_scale_factor = VideoCore::g_emu_window->GetFramebufferLayout().GetScalingRatio();
    }
    res_scale_width = resolution_scale_factor;
    res_scale_height = resolution_scale_factor;
}

MathUtil::Rectangle<int> CachedSurface::GetSubRect(const SurfaceParams& sub_surface) const {
    const u32 begin_pixel_index = PixelsInBytes(sub_surface.addr - addr);
    const int x0 = begin_pixel_index % width;
    const int y0 = begin_pixel_index / width;

    if (is_tiled)
        return MathUtil::Rectangle<int>(x0, height - y0 - sub_surface.height, x0 + sub_surface.width, height - y0); // Bottom to top

    return MathUtil::Rectangle<int>(x0, y0, x0 + sub_surface.width, y0 + sub_surface.height); // Top to bottom
}

MathUtil::Rectangle<int> CachedSurface::GetScaledSubRect(const SurfaceParams& sub_surface) const {
    auto rect = GetSubRect(sub_surface);
    rect.left = static_cast<int>(rect.left * res_scale_width);
    rect.right = static_cast<int>(rect.right * res_scale_width);
    rect.top = static_cast<int>(rect.top * res_scale_height);
    rect.bottom = static_cast<int>(rect.bottom * res_scale_height);
    return rect;
}

bool CachedSurface::ExactMatch(const SurfaceParams& other_surface) const {
    return (other_surface.addr == addr &&
        other_surface.width == width &&
        other_surface.height == height &&
        other_surface.stride == stride &&
        other_surface.pixel_format == pixel_format &&
        other_surface.is_tiled == is_tiled);
}

bool CachedSurface::CanSubRect(const SurfaceParams& sub_surface) const {
    if (sub_surface.addr < addr || sub_surface.end > end || sub_surface.stride != stride ||
        sub_surface.pixel_format != pixel_format || sub_surface.is_tiled != is_tiled)
        return false;

    auto rect = GetSubRect(sub_surface);

    if (rect.left + sub_surface.width > stride)
        return false;

    if (is_tiled)
        return ((height - rect.bottom) % 8 == 0 && rect.left % 8 == 0);

    return true;
}

bool CachedSurface::CanCopy(const SurfaceParams& dest_surface) const {
    if (type == SurfaceType::Fill && IsRegionValid(dest_surface.GetInterval()) &&
        dest_surface.addr >= addr && dest_surface.end <= end) { // dest_surface is within our fill range
        if (fill_size != dest_surface.bytes_per_pixel) {
            // Check if bits repeat for our fill_size
            const u32 dest_bytes_per_pixel = std::max(dest_surface.bytes_per_pixel, 1u); // Take care of 4bpp formats
            std::vector<u8> fill_test(fill_size * dest_bytes_per_pixel);

            for (u32 i = 0; i < dest_bytes_per_pixel; ++i)
                std::memcpy(&fill_test[i * fill_size], &fill_data[0], fill_size);

            for (u32 i = 0; i < fill_size; ++i)
                if (std::memcmp(&fill_test[dest_bytes_per_pixel * i], &fill_test[0], dest_bytes_per_pixel) != 0)
                    return false;

            if (dest_surface.bytes_per_pixel == 0 && (fill_test[0] & 0xF) != (fill_test[0] >> 4)) // 4bpp compare
                return false;
        }
        return true;
    }
    if (CanSubRect(dest_surface) && dest_surface.width == stride)
        return true;

    return false;
}

static void CopySurface(const Surface& src_surface, const Surface& dest_surface) {
    if (src_surface == dest_surface)
        return;

    // This is only called when CanCopy is true, no need to run checks here
    if (src_surface->type == SurfaceType::Fill) {
        // FillSurface needs a 4 bytes buffer
        const u32 fill_offset = (dest_surface->addr - src_surface->addr) % src_surface->fill_size;
        std::array<u8, 4> fill_buffer;

        u32 fill_buff_pos = fill_offset;
        for (int i : {0, 1, 2, 3})
            fill_buffer[i] = src_surface->fill_data[fill_buff_pos++ % src_surface->fill_size];

        FillSurface(dest_surface, &fill_buffer[0]);
    }
    if (src_surface->CanSubRect(*dest_surface)) {
        BlitTextures(src_surface->texture.handle, src_surface->GetScaledSubRect(*dest_surface),
            dest_surface->texture.handle, dest_surface->GetScaledRect(),
            src_surface->type);
    }
    dest_surface->gl_buffer_dirty = true;
}

MICROPROFILE_DEFINE(OpenGL_SurfaceLoad, "OpenGL", "Surface Load", MP_RGB(128, 64, 192));
void CachedSurface::LoadGLBuffer(PAddr load_start, PAddr load_end) {
    ASSERT(type != SurfaceType::Fill);

    const u8* const texture_src_data = Memory::GetPhysicalPointer(addr);
    if (texture_src_data == nullptr)
        return;

    MICROPROFILE_SCOPE(OpenGL_SurfaceLoad);

    ASSERT(load_start >= addr && load_end <= end);
    const u32 start_offset = load_start - addr;

    if (!is_tiled) {
        ASSERT(type == SurfaceType::Color);
        std::memcpy(&gl_buffer[start_offset], texture_src_data + start_offset, load_end - load_start);
    }
    else {
        if (type == SurfaceType::Texture) {
            Pica::Texture::TextureInfo tex_info{};
            tex_info.width = width;
            tex_info.height = height;
            tex_info.format = static_cast<Pica::TexturingRegs::TextureFormat>(pixel_format);
            tex_info.SetDefaultStride();
            tex_info.physical_address = addr;

            for (unsigned y = 0; y < height; ++y) {
                for (unsigned x = 0; x < width; ++x) {
                    auto vec4 = Pica::Texture::LookupTexture(texture_src_data, x, height - 1 - y, tex_info);
                    const size_t offset = (x + (width * y)) * 4;
                    std::memcpy(&gl_buffer[offset], vec4.AsArray(), 4);
                }
            }
        }
        else {
            size_t copyfn_offset = MortonCopyFlags::MortonToGl;
            copyfn_offset |= (bytes_per_pixel - 1) << MortonCopyFlags::BytesPerPixelBits;
            copyfn_offset |= (gl_bytes_per_pixel - 1) << MortonCopyFlags::GLBytesPerPixelBits;

            if (load_start != addr || load_end != end)
                copyfn_offset |= MortonCopyFlags::CheckRange;
            if (pixel_format == PixelFormat::D24S8)
                copyfn_offset |= MortonCopyFlags::D24S8Format;

            MortonCopyFnTable[copyfn_offset](width, height,
                texture_src_data, &gl_buffer[gl_buffer_offset], addr, load_start, load_end);
        }
    }
}

MICROPROFILE_DEFINE(OpenGL_SurfaceFlush, "OpenGL", "Surface Flush", MP_RGB(128, 192, 64));
void CachedSurface::FlushGLBuffer(PAddr flush_start, PAddr flush_end) {
    u8* const dst_buffer = Memory::GetPhysicalPointer(addr);
    if (dst_buffer == nullptr)
        return;

    MICROPROFILE_SCOPE(OpenGL_SurfaceFlush);

    ASSERT(flush_start >= addr && flush_end <= end);
    const u32 start_offset = flush_start - addr;
    const u32 end_offset = flush_end - addr;

    if (type == SurfaceType::Fill) {
        const u32 coarse_start_offset = start_offset - (start_offset % fill_size);
        const u32 backup_bytes = start_offset % fill_size;
        std::array<u8, 4> backup_data;
        if (backup_bytes)
            std::memcpy(&backup_data[0], &dst_buffer[coarse_start_offset], backup_bytes);

        for (u32 offset = coarse_start_offset; offset < end_offset; offset += fill_size)
            std::memcpy(&dst_buffer[offset], &fill_data[0], std::min(fill_size, end_offset - offset));

        if (backup_bytes)
            std::memcpy(&dst_buffer[coarse_start_offset], &backup_data[0], backup_bytes);
    }
    else if (!is_tiled) {
        ASSERT(type == SurfaceType::Color);
        std::memcpy(dst_buffer + start_offset, &gl_buffer[start_offset], flush_end - flush_start);
    }
    else {
        size_t copyfn_offset = (bytes_per_pixel - 1) << MortonCopyFlags::BytesPerPixelBits;
        copyfn_offset |= (gl_bytes_per_pixel - 1) << MortonCopyFlags::GLBytesPerPixelBits;

        if (flush_start != addr || flush_end != end)
            copyfn_offset |= MortonCopyFlags::CheckRange;
        if (pixel_format == PixelFormat::D24S8)
            copyfn_offset |= MortonCopyFlags::D24S8Format;

        MortonCopyFnTable[copyfn_offset](width, height,
            &gl_buffer[gl_buffer_offset], dst_buffer, addr, flush_start, flush_end);
    }
}

void CachedSurface::UploadGLTexture() {
    if (type == SurfaceType::Fill)
        return;

    ASSERT(gl_buffer.size() == width * height * gl_bytes_per_pixel);

    const FormatTuple& tuple = GetFormatTuple(pixel_format);

    // Load data from memory to the surface
    OpenGLState cur_state = OpenGLState::GetCurState();

    GLuint old_tex = cur_state.texture_units[0].texture_2d;
    cur_state.texture_units[0].texture_2d = texture.handle;
    cur_state.Apply();

    glActiveTexture(GL_TEXTURE0);
    glTexImage2D(GL_TEXTURE_2D, 0, tuple.internal_format, width, height, 0,
        tuple.format, tuple.type, &gl_buffer[0]);

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAX_LEVEL, 0);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

    cur_state.texture_units[0].texture_2d = old_tex;
    cur_state.Apply();

    // If not 1x scale, blit 1x texture to a new scaled texture and replace texture in surface
    if (res_scale_width != 1.f || res_scale_height != 1.f) {
        OGLTexture scaled_texture;
        scaled_texture.Create();

        AllocateSurfaceTexture(scaled_texture.handle, tuple, GetScaledWidth(), GetScaledHeight());
        BlitTextures(texture.handle, GetRect(), scaled_texture.handle, GetScaledRect(), type);

        std::swap(texture.handle, scaled_texture.handle);
    }

    gl_buffer_dirty = false;
}

void CachedSurface::DownloadGLTexture() {
    if (gl_buffer.size() == 0)
        gl_buffer.resize(width * height * gl_bytes_per_pixel);

    if (!gl_buffer_dirty || type == SurfaceType::Fill)
        return;

    const FormatTuple& tuple = GetFormatTuple(pixel_format);

    OpenGLState cur_state = OpenGLState::GetCurState();
    GLuint old_tex = cur_state.texture_units[0].texture_2d;

    // If not 1x scale, blit scaled texture to a new 1x texture and use that to flush
    OGLTexture unscaled_tex;
    if (res_scale_width != 1.f || res_scale_height != 1.f) {
        unscaled_tex.Create();

        AllocateSurfaceTexture(unscaled_tex.handle, tuple, width, height);
        BlitTextures(texture.handle, GetScaledRect(), unscaled_tex.handle, GetRect(), type);

        cur_state.texture_units[0].texture_2d = unscaled_tex.handle;
    }
    else {
        cur_state.texture_units[0].texture_2d = texture.handle;
    }
    cur_state.Apply();

    glActiveTexture(GL_TEXTURE0);
    glGetTexImage(GL_TEXTURE_2D, 0, tuple.format, tuple.type, &gl_buffer[0]);

    cur_state.texture_units[0].texture_2d = old_tex;
    cur_state.Apply();

    gl_buffer_dirty = false;
}

enum MatchType : int {
    Exact = (1 << 0), // Surfaces perfectly match
    SubRect = (1 << 1), // Surface encompasses params
    Invalid = (1 << 2), // Flag that can be applied to other match types, invalid matches require validation before they can be used
    Copy = (1 << 3), // Surface we can copy from
    TexCopy = (1 << 4), // Surface that will match a display transfer "texture copy" parameters
    All = (1 << 5) - 1,
    None = 0,
};

constexpr MatchType operator | (MatchType lhs, MatchType rhs) {
    return static_cast<MatchType>(static_cast<int>(lhs) | static_cast<int>(rhs));
}

/// Get the best surface match (and its match type) for the given flags, higher flag value meaning lower priority
template <MatchType find_flags>
std::tuple<Surface, MatchType> FindMatch(const SurfaceCache& surface_cache, const SurfaceParams& params, bool match_res_scale) {
    constexpr float MATCH_GOODNESS_RESET = -1.f;

    Surface match_surface = nullptr;
    MatchType match_type = MatchType::All; // Starting from lowest possible priority
    float best_match_goodness = MATCH_GOODNESS_RESET;

    for (auto& pair : RangeFromInterval(surface_cache, params.GetInterval())) {
        for (auto& surface : pair.second) {
            const bool res_scale_match = (params.res_scale_width == surface->res_scale_width && params.res_scale_height == surface->res_scale_height);
            const float match_goodness = surface->res_scale_width * surface->res_scale_height;
            const MatchType invalid_mask = surface->IsRegionValid(params.GetInterval()) ? MatchType::None : MatchType::Invalid;

            if (!(find_flags & MatchType::Invalid) && invalid_mask == MatchType::Invalid)
                continue;

            const auto IsMatch_Helper = [&](MatchType check_type, auto match_fn) {
                if (!(find_flags & check_type))
                    return false;

                check_type = check_type | invalid_mask;

                // Lower flag value means higher priority
                if (match_type < check_type) // We already have a better match type
                    return true; // Return true to skip to the next surface

                if (!match_fn())
                    return false;

                if (!match_res_scale || res_scale_match || surface->type == SurfaceType::Fill) { // Found a match
                    if (match_type > check_type) {
                        best_match_goodness = MATCH_GOODNESS_RESET;
                        match_type = check_type;
                    }
                    if (match_goodness > best_match_goodness) {
                        best_match_goodness = match_goodness;
                        match_surface = surface;
                    }
                }
                return false;
            };
            if (IsMatch_Helper(MatchType::Exact, [&] { return surface->ExactMatch(params); }))
                continue;
            if (IsMatch_Helper(MatchType::SubRect, [&] { return surface->CanSubRect(params); }))
                continue;
            if (IsMatch_Helper(MatchType::Copy, [&] { return surface->CanCopy(params); }))
                continue;
            if (IsMatch_Helper(MatchType::TexCopy, [&] {
                if (surface->pixel_format == PixelFormat::Invalid ||
                    surface->addr > params.addr || surface->end < params.end ||
                    ((params.addr - surface->addr) * 8) % SurfaceParams::GetFormatBpp(surface->pixel_format) != 0 ||
                    (params.width * 8) % SurfaceParams::GetFormatBpp(surface->pixel_format) != 0 ||
                    (params.stride * 8) % SurfaceParams::GetFormatBpp(surface->pixel_format) != 0)
                    return false;

                const u32 begin_pixel_index = surface->PixelsInBytes(params.addr - surface->addr);
                const int x0 = begin_pixel_index % surface->width;
                const int y0 = begin_pixel_index / surface->width;

                if (!surface->is_tiled)
                    return (surface->PixelsInBytes(params.stride) == surface->stride &&
                        x0 + surface->PixelsInBytes(params.width) <= surface->stride);

                return (surface->PixelsInBytes(params.addr - surface->addr) % 64 == 0 &&
                    surface->PixelsInBytes(params.width) % 64 == 0 &&
                    surface->PixelsInBytes(params.stride) == surface->stride * 8 &&
                    x0 + surface->PixelsInBytes(params.width / 8) <= surface->stride);
            }))
                continue;
        }
    }
    return std::make_tuple(match_surface, (match_surface == nullptr) ? MatchType::None : match_type);
}

RasterizerCacheOpenGL::RasterizerCacheOpenGL() {
    transfer_framebuffers[0].Create();
    transfer_framebuffers[1].Create();
}

RasterizerCacheOpenGL::~RasterizerCacheOpenGL() {
    FlushAll();
    while (!surface_cache.empty())
        UnregisterSurface(*surface_cache.begin()->second.begin());
    transfer_framebuffers[0].Release();
    transfer_framebuffers[1].Release();
}

bool RasterizerCacheOpenGL::BlitSurfaces(const Surface& src_surface,
                                         const MathUtil::Rectangle<int>& src_rect,
                                         const Surface& dst_surface,
                                         const MathUtil::Rectangle<int>& dst_rect) {
    if (!SurfaceParams::CheckFormatsBlittable(src_surface->pixel_format,
                                              dst_surface->pixel_format))
        return false;

    return BlitTextures(src_surface->texture.handle, src_rect,
                        dst_surface->texture.handle, dst_rect,
                        src_surface->type);
}

Surface RasterizerCacheOpenGL::GetSurface(const SurfaceParams& params, bool match_res_scale, bool load_if_create) {
    if (params.addr == 0 || params.height * params.width == 0) {
        return nullptr;
    }

    // Check for an exact or subrect match in existing surfaces
    Surface surface_match;
    MatchType match_type;
    std::tie(surface_match, match_type) =
        FindMatch<MatchType::Exact | MatchType::Invalid>(surface_cache, params, match_res_scale);

    if (surface_match != nullptr) {
        if (load_if_create && (match_type & MatchType::Invalid)) {
            ValidateSurface(surface_match, params.addr, params.size);
        }
        return surface_match;
    }

    ASSERT(params.width == params.stride); // Use GetSurfaceSubRect instead

    Surface new_surface = CreateSurface(params);
    if (load_if_create)
        ValidateSurface(new_surface, params.addr, params.size);

    RegisterSurface(new_surface);

    return new_surface;
}

SurfaceRect_Tuple RasterizerCacheOpenGL::GetSurfaceSubRect(const SurfaceParams& params,
                                                           bool match_res_scale,
                                                           bool load_if_create) {
    MathUtil::Rectangle<int> out_rect{};

    if (params.addr == 0 || params.height * params.width == 0) {
        return std::make_tuple(nullptr, out_rect);
    }

    // Attempt to find encompassing surface
    Surface subrect_match;
    MatchType match_type;
    std::tie(subrect_match, match_type) = FindMatch<MatchType::Exact | MatchType::SubRect | MatchType::Invalid>(surface_cache, params, match_res_scale);

    // Return the best subrect surface if found
    if (subrect_match != nullptr) {
        out_rect = subrect_match->GetScaledSubRect(params);
        // Tiled surfaces are flipped vertically in the rasterizer vs. 3DS memory.
        if (params.is_tiled)
            std::swap(out_rect.top, out_rect.bottom);

        if (load_if_create && (match_type & MatchType::Invalid))
            ValidateSurface(subrect_match, params.addr, params.size);

        return std::make_tuple(subrect_match, out_rect);
    }

    // No subrect found - create and return a new surface
    SurfaceParams new_params = params;
    new_params.width = params.stride; // Can't have gaps in a surface
    new_params.UpdateParams();

    out_rect = new_params.GetScaledRect();
    if (new_params.is_tiled)
        std::swap(out_rect.top, out_rect.bottom);

    // If stride was bigger than width we need to adjust our output rect
    out_rect.right = static_cast<int>(params.width * new_params.res_scale_width);

    Surface new_surface = CreateSurface(new_params);
    if (load_if_create)
        ValidateSurface(new_surface, new_params.addr, new_params.size);

    RegisterSurface(new_surface);

    return std::make_tuple(new_surface, out_rect);
}

Surface RasterizerCacheOpenGL::GetTextureSurface(const Pica::TexturingRegs::FullTextureConfig& config) {
    Pica::Texture::TextureInfo info = Pica::Texture::TextureInfo::FromPicaRegister(config.config, config.format);

    SurfaceParams params;
    params.addr = info.physical_address;
    params.width = info.width;
    params.height = info.height;
    params.is_tiled = true;
    params.pixel_format = SurfaceParams::PixelFormatFromTextureFormat(info.format);
    params.UpdateParams();
    return GetSurface(params, false, true);
}

SurfaceSurfaceRect_Tuple RasterizerCacheOpenGL::GetFramebufferSurfaces(bool using_color_fb,
                                                                       bool using_depth_fb) {
    const auto& regs = Pica::g_state.regs;
    const auto& config = regs.framebuffer.framebuffer;

    // Make sur that framebuffers don't overlap if both color and depth are being used
    u32 fb_area = config.GetWidth() * config.GetHeight();
    bool framebuffers_overlap = config.GetColorBufferPhysicalAddress() != 0 &&
        config.GetDepthBufferPhysicalAddress() != 0 &&
        MathUtil::IntervalsIntersect(
            config.GetColorBufferPhysicalAddress(),
            fb_area * GPU::Regs::BytesPerPixel(GPU::Regs::PixelFormat(config.color_format.Value())),
            config.GetDepthBufferPhysicalAddress(),
            fb_area * Pica::FramebufferRegs::BytesPerDepthPixel(config.depth_format));

    if (framebuffers_overlap && using_color_fb && using_depth_fb) {
        LOG_CRITICAL(Render_OpenGL, "Color and depth framebuffer memory regions overlap; overlapping framebuffers not supported!");
        using_depth_fb = false;
    }

    // get color and depth surfaces
    SurfaceParams color_params;
    SurfaceParams depth_params;
    color_params.is_tiled = depth_params.is_tiled = true;

    color_params.addr = config.GetColorBufferPhysicalAddress();
    color_params.width = depth_params.width = config.GetWidth();
    color_params.height = depth_params.height = config.GetHeight();
    color_params.pixel_format = SurfaceParams::PixelFormatFromColorFormat(config.color_format);
    color_params.UpdateParams();

    MathUtil::Rectangle<int> rect{};
    Surface color_surface = nullptr;
    if (using_color_fb)
        std::tie(color_surface, rect) = GetSurfaceSubRect(color_params, true, true);

    depth_params.pixel_format = SurfaceParams::PixelFormatFromDepthFormat(config.depth_format);
    depth_params.addr = config.GetDepthBufferPhysicalAddress();
    depth_params.UpdateParams();

    Surface depth_surface = nullptr;
    if (using_depth_fb && color_surface != nullptr) {
        const PAddr validate_addr = depth_params.addr;
        const u32 validate_size = depth_params.size;

        // Can't specify separate color and depth viewport offsets in OpenGL, so make sure depth_surface will have the same offsets
        depth_params.addr -= color_surface->PixelsInBytes(color_params.addr - color_surface->addr) * depth_params.bytes_per_pixel;
        depth_params.height = color_surface->height;
        depth_params.UpdateParams();

        depth_surface = GetSurface(depth_params, true, false);
        ValidateSurface(depth_surface, validate_addr, validate_size);
    }
    else if (using_depth_fb) {
        std::tie(depth_surface, rect) = GetSurfaceSubRect(depth_params, true, true);
    }

    return std::make_tuple(color_surface, depth_surface, rect);
}

Surface RasterizerCacheOpenGL::GetFillSurface(const GPU::Regs::MemoryFillConfig& config) {
    Surface new_surface = std::make_shared<CachedSurface>();

    new_surface->addr = config.GetStartAddress();
    new_surface->end = config.GetEndAddress();
    new_surface->size = new_surface->end - new_surface->addr;
    new_surface->type = SurfaceType::Fill;
    std::memcpy(&new_surface->fill_data[0], &config.value_32bit, 4);
    if (config.fill_32bit)
        new_surface->fill_size = 4;
    else if (config.fill_24bit)
        new_surface->fill_size = 3;
    else
        new_surface->fill_size = 2;

    RegisterSurface(new_surface);
    return new_surface;
}

SurfaceRect_Tuple RasterizerCacheOpenGL::GetTexCopySurface(const SurfaceParams& params) {
    MathUtil::Rectangle<int> rect{};

    Surface match_surface;
    MatchType match_type;
    std::tie(match_surface, match_type) = FindMatch<MatchType::TexCopy | MatchType::Invalid>(surface_cache, params, false);

    if (match_type & MatchType::Invalid)
        ValidateSurface(match_surface, params.addr, params.size);

    if (match_surface != nullptr) {
        SurfaceParams match_subrect = params;
        match_subrect.width = match_surface->PixelsInBytes(params.width);
        match_subrect.stride = match_surface->PixelsInBytes(params.stride);

        if (match_surface->is_tiled) {
            match_subrect.width /= 8;
            match_subrect.stride /= 8;
            match_subrect.height *= 8;
        }

        rect = match_surface->GetScaledSubRect(match_subrect);
        if (match_surface->is_tiled)
            std::swap(rect.top, rect.bottom);
    }

    return std::make_tuple(match_surface, rect);
}

void RasterizerCacheOpenGL::ValidateSurface(const Surface& surface, PAddr addr, u32 size) {
    if (size == 0)
        return;

    bool upload_texture = false;
    const auto validate_interval = (surface->type != SurfaceType::Texture) ?
                                   SurfaceInterval::right_open(addr, addr + size) :
                                   surface->GetInterval();

    if (surface->type == SurfaceType::Fill) {
        // Sanity check, fill surfaces will always be valid when used
        ASSERT(surface->IsRegionValid(validate_interval));
        return;
    }

    for (;;) {
        const auto it = surface->invalid_regions.find(validate_interval);
        if (it == surface->invalid_regions.end())
            break;

        const auto interval = *it & validate_interval;
        const PAddr interval_start = boost::icl::first(interval);
        const PAddr interval_end = boost::icl::last_next(interval);

        // Look for a valid surface to blit
        SurfaceParams params = *surface;
        const u32 pixel_offset = params.PixelsInBytes(interval_start - params.addr);
        if (!params.is_tiled) {
            params.addr += (pixel_offset - (pixel_offset % params.width)) *
                           SurfaceParams::GetFormatBpp(params.pixel_format) / 8; // Start of the row
            params.height = (params.PixelsInBytes(interval_end - params.addr - 1) / params.width) + 1;
        }
        else {
            params.addr += (pixel_offset - (pixel_offset % (params.width * 8))) *
                           SurfaceParams::GetFormatBpp(params.pixel_format) / 8; // Start of the tiled row
            params.height = ((params.PixelsInBytes(interval_end - params.addr - 1) / (params.width * 8)) + 1) * 8;
        }
        params.UpdateParams();

        Surface match_surface;
        MatchType match_type;
        std::tie(match_surface, match_type) =
            FindMatch<MatchType::Exact | MatchType::SubRect | MatchType::Copy>(surface_cache, params, true);

        if (match_type == MatchType::Copy) {
            // Need to call CopySurface and possibly create a new one first, which GetSurface will do for us
            if (params.GetInterval() == surface->GetInterval()) {
                CopySurface(match_surface, surface);
                surface->invalid_regions.clear();
                return;
            }
            Surface tmp_surface = GetSurface(params, true, false);
            if (tmp_surface != nullptr)
                CopySurface(match_surface, tmp_surface);
            match_surface = tmp_surface;
        }

        if (match_surface != nullptr) {
            const auto src_rect = (match_type == MatchType::SubRect) ?
                                  match_surface->GetScaledSubRect(params) :
                                  match_surface->GetScaledRect();
            const auto dest_rect = surface->GetScaledSubRect(params);

            BlitSurfaces(match_surface, src_rect, surface, dest_rect);
            surface->gl_buffer_dirty = true;

            surface->invalid_regions.erase(params.GetInterval());
            continue;
        }

        // Load data from 3DS memory
        FlushRegion(interval_start, interval_end - interval_start);
        surface->DownloadGLTexture();
        surface->LoadGLBuffer(interval_start, interval_end);
        upload_texture = true;

        surface->invalid_regions.erase(interval);
    }

    if (upload_texture)
        surface->UploadGLTexture();
}

void RasterizerCacheOpenGL::FlushRegion(PAddr addr, u32 size) {
    if (size == 0)
        return;

    const auto flush_interval = SurfaceInterval::right_open(addr, addr + size);
    for (auto& pair : RangeFromInterval(dirty_regions, flush_interval)) {
        const auto interval = pair.first & flush_interval;
        auto& surface = pair.second;

        // Sanity check, this surface is the last one that marked this region dirty
        ASSERT(surface->IsRegionValid(interval));
        surface->DownloadGLTexture();
        surface->FlushGLBuffer(boost::icl::first(interval), boost::icl::last_next(interval));
    }

    // Reset dirty regions
    dirty_regions.erase(flush_interval);
}

void RasterizerCacheOpenGL::FlushAll() {
    FlushRegion(0, 0xFFFFFFFF);
}

void RasterizerCacheOpenGL::InvalidateRegion(PAddr addr, u32 size, const Surface& region_owner) {
    if (size == 0)
        return;

    const auto invalid_interval = SurfaceInterval::right_open(addr, addr + size);

    if (region_owner != nullptr) {
        ASSERT(region_owner->type != SurfaceType::Texture);
        ASSERT(addr >= region_owner->addr && addr + size <= region_owner->end);
        ASSERT(region_owner->width == region_owner->stride); // Surfaces can't have a gap
        region_owner->gl_buffer_dirty = true;
        region_owner->invalid_regions.erase(invalid_interval);
    }

    SurfaceSet remove_surfaces;

    for (auto& pair : RangeFromInterval(surface_cache, invalid_interval)) {
        for (auto& cached_surface : pair.second) {
            if (cached_surface == region_owner)
                continue;

            // If cpu is invalidating this region we want to remove it
            // to (likely) mark the memory pages as uncached
            // but before that we have to flush its region that is still valid
            if (region_owner == nullptr) {
                const auto flush_intervals = SurfaceRegions(cached_surface->GetInterval()) - invalid_interval;
                for (const auto& interval : flush_intervals) {
                    FlushRegion(boost::icl::first(interval), boost::icl::length(interval));
                }
                remove_surfaces.emplace(cached_surface);
                continue;
            }

            const auto interval = cached_surface->GetInterval() & invalid_interval;

            cached_surface->invalid_regions.insert(interval);

            // Remove only "empty" fill surfaces to avoid destroying and recreating OGL textures
            if (cached_surface->type == SurfaceType::Fill &&
                !cached_surface->IsRegionPartiallyValid(cached_surface->GetInterval()))
                remove_surfaces.emplace(cached_surface);
        }
    }

    if (region_owner != nullptr)
        dirty_regions.set(std::make_pair(invalid_interval, region_owner));
    else
        dirty_regions.erase(invalid_interval);

    for (auto& remove_surface : remove_surfaces)
        UnregisterSurface(remove_surface);
}

Surface RasterizerCacheOpenGL::CreateSurface(const SurfaceParams& params) {
    Surface surface = std::make_shared<CachedSurface>();
    static_cast<SurfaceParams&>(*surface) = params;

    surface->texture.Create();

    // OpenGL needs 4 bpp alignment for D24 since using GL_UNSIGNED_INT as type
    surface->gl_bytes_per_pixel =
        (surface->pixel_format == PixelFormat::D24 || surface->type == SurfaceType::Texture) ?
        4 :
        surface->bytes_per_pixel;

    surface->gl_buffer_offset = (surface->pixel_format == PixelFormat::D24) ? 1 : 0;

    surface->gl_buffer_dirty = false;
    surface->invalid_regions.insert(surface->GetInterval());
    AllocateSurfaceTexture(surface->texture.handle,
                           GetFormatTuple(surface->pixel_format),
                           surface->GetScaledWidth(),
                           surface->GetScaledHeight());

    return surface;
}

void RasterizerCacheOpenGL::RegisterSurface(const Surface& surface) {
    surface_cache.add(std::make_pair(surface->GetInterval(), SurfaceSet({ surface })));
    UpdatePagesCachedCount(surface->addr, surface->size, 1);
}

void RasterizerCacheOpenGL::UnregisterSurface(const Surface& surface) {
    UpdatePagesCachedCount(surface->addr, surface->size, -1);
    surface_cache.subtract(std::make_pair(surface->GetInterval(), SurfaceSet({ surface })));
}

void RasterizerCacheOpenGL::UpdatePagesCachedCount(PAddr addr, u32 size, int delta) {
    const u32 num_pages = ((addr + size - 1) >> Memory::PAGE_BITS) - (addr >> Memory::PAGE_BITS) + 1;
    const u32 page_start = addr >> Memory::PAGE_BITS;
    const u32 page_end = page_start + num_pages;

    // Interval maps will erase segments if count reaches 0, so if delta is negative we have to subtract after iterating
    const auto pages_interval = PageMap::interval_type::right_open(page_start, page_end);
    if (delta > 0)
        cached_pages.add(std::make_pair(pages_interval, delta));

    for (auto& pair : RangeFromInterval(cached_pages, pages_interval)) {
        const auto interval = pair.first & pages_interval;
        const int count = pair.second;

        const PAddr interval_start_addr = boost::icl::first(interval) << Memory::PAGE_BITS;
        const PAddr interval_end_addr = boost::icl::last_next(interval) << Memory::PAGE_BITS;
        const u32 interval_size = interval_end_addr - interval_start_addr;

        if (delta > 0 && count == delta)
            Memory::RasterizerMarkRegionCached(interval_start_addr, interval_size, true);
        else if (delta < 0 && count == -delta)
            Memory::RasterizerMarkRegionCached(interval_start_addr, interval_size, false);
        else
            ASSERT(count >= 0);
    }

    if (delta < 0)
        cached_pages.add(std::make_pair(pages_interval, delta));
}
