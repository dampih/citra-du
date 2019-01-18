// Copyright 2016 Citra Emulator Project
// Licensed under GPLv2 or any later version
// Refer to the license.txt file included.

#include <sstream>
#include <string>
#include <vector>
#include "common/common_paths.h"
#include "common/file_util.h"
#include "common/string_util.h"
#include "video_core/renderer_opengl/post_processing_opengl.h"

namespace OpenGL {

constexpr char dolphin_shader_header[] = R"(
#version 150 core

// hlsl to glsl types
#define float2 vec2
#define float3 vec3
#define float4 vec4
#define uint2 uvec2
#define uint3 uvec3
#define uint4 uvec4
#define int2 ivec2
#define int3 ivec3
#define int4 ivec4

// hlsl to glsl function translation
#define frac fract
#define lerp mix

// Output variable
out float4 color;
// Input coordinates
in float2 frag_tex_coord;
// Resolution
uniform float4 resolution;
// Layer
uniform int layer;

uniform sampler2D color_texture;
uniform sampler2D color_texture_r;

// Interfacing functions
float4 Sample()
{
    return texture(color_texture, frag_tex_coord);
}

float4 SampleLocation(float2 location)
{
    return texture(color_texture, location);
}

float4 SampleLayer(int layer)
{
    if(layer == 0)
        return texture(color_texture, frag_tex_coord);
    else
        return texture(color_texture_r, frag_tex_coord);
}

#define SampleOffset(offset) textureOffset(color_texture, frag_tex_coord, offset)

float2 GetResolution()
{
    return resolution.xy;
}

float2 GetInvResolution()
{
    return resolution.zw;
}

float2 GetCoordinates()
{
    return frag_tex_coord;
}

void SetOutput(float4 color_in)
{
    color = color_in;
}

)";

std::vector<std::string> GetPostProcessingShaders(bool anaglyph) {
    std::string shader_dir = FileUtil::GetUserPath(FileUtil::UserPath::ShaderDir);
    std::vector<std::string> shader_names;

    if (!FileUtil::IsDirectory(shader_dir)) {
        FileUtil::CreateDir(shader_dir);
    }

    if (anaglyph) {
        shader_dir = shader_dir + "anaglyph";
        if (!FileUtil::IsDirectory(shader_dir)) {
            FileUtil::CreateDir(shader_dir);
        }
    }

    // Would it make more sense to just add a directory list function to FileUtil?
    const auto callback = [&shader_names](u64* num_entries_out, const std::string& directory,
                                          const std::string& virtual_name) -> bool {
        const std::string physical_name = directory + DIR_SEP + virtual_name;
        if (!FileUtil::IsDirectory(physical_name)) {
            // The following is done to avoid coupling this to Qt
            std::size_t dot_pos = virtual_name.rfind(".");
            if (dot_pos != std::string::npos) {
                if (Common::ToLower(virtual_name.substr(dot_pos + 1)) == "glsl") {
                    shader_names.push_back(virtual_name.substr(0, dot_pos));
                }
            }
        }
        return true;
    };

    FileUtil::ForeachDirectoryEntry(nullptr, shader_dir, callback);

    std::sort(shader_names.begin(), shader_names.end());

    return shader_names;
}

std::string GetShader(bool anaglyph, std::string shader) {
    std::string shader_dir = FileUtil::GetUserPath(FileUtil::UserPath::ShaderDir);
    std::string shader_path;

    if (anaglyph) {
        shader_dir = shader_dir + "anaglyph";
    }

    // Examining the directory is done because the shader extension might have an odd case
    // This can be eliminated if it is specified that the shader extension must be lowercase
    const auto callback = [&shader, &shader_path](u64* num_entries_out,
                                                  const std::string& directory,
                                                  const std::string& virtual_name) -> bool {
        const std::string physical_name = directory + DIR_SEP + virtual_name;
        if (!FileUtil::IsDirectory(physical_name)) {
            // The following is done to avoid coupling this to Qt
            std::size_t dot_pos = virtual_name.rfind(".");
            if (dot_pos != std::string::npos) {
                if (Common::ToLower(virtual_name.substr(dot_pos + 1)) == "glsl" &&
                    virtual_name.substr(0, dot_pos) == shader) {
                    shader_path = physical_name;
                    return false;
                }
            }
        }
        return true;
    };

    FileUtil::ForeachDirectoryEntry(nullptr, shader_dir, callback);
    if (shader_path.empty()) {
        return "";
    }

    std::ifstream file;
    OpenFStream(file, shader_path, std::ios_base::in);
    if (!file) {
        return "";
    }

    std::stringstream shader_text;
    shader_text << file.rdbuf();

    return dolphin_shader_header + shader_text.str();
}

} // namespace OpenGL
