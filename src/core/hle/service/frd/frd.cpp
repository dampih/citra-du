// Copyright 2015 Citra Emulator Project
// Licensed under GPLv2 or any later version
// Refer to the license.txt file included.

#include "common/assert.h"
#include "common/logging/log.h"
#include "common/string_util.h"
#include "core/hle/ipc.h"
#include "core/hle/ipc_helpers.h"
#include "core/hle/kernel/event.h"
#include "core/hle/kernel/handle_table.h"
#include "core/hle/result.h"
#include "core/hle/service/frd/frd.h"
#include "core/hle/service/frd/frd_a.h"
#include "core/hle/service/frd/frd_u.h"
#include "core/hle/service/service.h"
#include "core/memory.h"


namespace Service {
namespace FRD {

static FriendKey my_friend_key = {0, 0, 0ull};
static MyPresence my_presence = {};
static bool logged = false;
static Kernel::SharedPtr<Kernel::Event> notification_event = nullptr;

u64 PrincipalIdToFriendCode(u32 principal_id) {
    u64 friend_code = principal_id;
    // TODO: This function takes the principalId given and applies SHA-1 over it (byte order: little
    // endian). The first byte of the SHA-1 digest is then shifted right by 1, which forms the
    // checksum byte.
    return friend_code;
}

void _Login(Interface* self) {
    IPC::RequestParser rp(Kernel::GetCommandBuffer(), 0x3, 0, 2);

    LOG_WARNING(Service_FRD, "(STUBBED) called");
    IPC::RequestBuilder rb = rp.MakeBuilder(1, 0);
    rb.Push(RESULT_SUCCESS);
}

void Login(Interface* self) {
    IPC::RequestParser rp(Kernel::GetCommandBuffer(), 0x3, 0, 2);
    const Kernel::Handle event_handle = rp.PopHandle();
    Kernel::SharedPtr<Kernel::Event> completion_event =
        Kernel::g_handle_table.Get<Kernel::Event>(event_handle);
    LOG_WARNING(Service_FRD, "(STUBBED) called, event_handle=0x%x", event_handle);
    IPC::RequestBuilder rb = rp.MakeBuilder(1, 0);
    if (completion_event == nullptr) {
        rb.Push(Kernel::ERR_INVALID_HANDLE);
    } else {
        logged = true;
        completion_event->Signal();
        rb.Push(RESULT_SUCCESS);
    }
}

void Logout(Interface* self) {
    IPC::RequestParser rp(Kernel::GetCommandBuffer(), 0x4, 0, 0);

    logged = false;

    LOG_WARNING(Service_FRD, "(STUBBED) called");
    IPC::RequestBuilder rb = rp.MakeBuilder(1, 0);
    rb.Push(RESULT_SUCCESS);
}

void AttachToEventNotification(Interface* self) {
    IPC::RequestParser rp(Kernel::GetCommandBuffer(), 0x20, 0, 2);

    const Kernel::Handle event_handle = rp.PopHandle();
    notification_event = Kernel::g_handle_table.Get<Kernel::Event>(event_handle);
    if (notification_event) {
        notification_event->name = "FRD:notification_event";
    }

    LOG_WARNING(Service_FRD, "(STUBBED) called");
    IPC::RequestBuilder rb = rp.MakeBuilder(1, 0);
    rb.Push(RESULT_SUCCESS);
}

void PrincipalIdToFriendCode(Interface* self) {
    IPC::RequestParser rp(Kernel::GetCommandBuffer(), 0x24, 1, 0);

    u32 principal_id = rp.Pop<u32>();
    u64 friend_code = PrincipalIdToFriendCode(principal_id);

    LOG_WARNING(Service_FRD, "(STUBBED) called, principal_id=0x%08x", principal_id);
    IPC::RequestBuilder rb = rp.MakeBuilder(3, 0);
    rb.Push(RESULT_SUCCESS);
    rb.Push(friend_code);
}

void IsValidFriendCode(Interface* self) {
    IPC::RequestParser rp(Kernel::GetCommandBuffer(), 0x26, 2, 0);

    u64 friend_code = rp.Pop<u64>();
    u32 principal_id = static_cast<u32>(friend_code & 0xFFFFFFFF);
    u64 new_friend_code = PrincipalIdToFriendCode(principal_id);
    bool valid = (friend_code >> 32) == (new_friend_code >> 32);

    LOG_WARNING(Service_FRD, "(STUBBED) called, friend_code=0x%llx", friend_code);
    IPC::RequestBuilder rb = rp.MakeBuilder(2, 0);
    rb.Push(RESULT_SUCCESS);
    rb.Push(valid);
}

void GetMyPresence(Interface* self) {
    IPC::RequestParser rp(Kernel::GetCommandBuffer(), 0x8, 0, 0);

    size_t size;
    VAddr buffer = rp.PeekStaticBuffer(0, &size);
    ASSERT_MSG(size == sizeof(MyPresence), "wrong buffer size");
    Memory::WriteBlock(buffer, &my_presence, sizeof(MyPresence));

    LOG_WARNING(Service_FRD, "(STUBBED) called");
    IPC::RequestBuilder rb = rp.MakeBuilder(1, 0);
    rb.Push(RESULT_SUCCESS);
}

void GetFriendKeyList(Interface* self) {
    u32* cmd_buff = Kernel::GetCommandBuffer();

    u32 unknown = cmd_buff[1];
    u32 frd_count = cmd_buff[2];
    u32 frd_key_addr = cmd_buff[65];

    FriendKey zero_key = {};
    for (u32 i = 0; i < frd_count; ++i) {
        Memory::WriteBlock(frd_key_addr + i * sizeof(FriendKey), &zero_key, sizeof(FriendKey));
    }

    cmd_buff[1] = RESULT_SUCCESS.raw; // No error
    cmd_buff[2] = 0;                  // 0 friends
    LOG_WARNING(Service_FRD, "(STUBBED) called, unknown=%d, frd_count=%d, frd_key_addr=0x%08X",
                unknown, frd_count, frd_key_addr);
}

void GetFriendProfile(Interface* self) {
    u32* cmd_buff = Kernel::GetCommandBuffer();

    u32 count = cmd_buff[1];
    u32 frd_key_addr = cmd_buff[3];
    u32 profiles_addr = cmd_buff[65];

    Profile zero_profile = {};
    for (u32 i = 0; i < count; ++i) {
        Memory::WriteBlock(profiles_addr + i * sizeof(Profile), &zero_profile, sizeof(Profile));
    }

    cmd_buff[1] = RESULT_SUCCESS.raw; // No error
    LOG_WARNING(Service_FRD,
                "(STUBBED) called, count=%d, frd_key_addr=0x%08X, profiles_addr=0x%08X", count,
                frd_key_addr, profiles_addr);
}

void GetFriendAttributeFlags(Interface* self) {
    u32* cmd_buff = Kernel::GetCommandBuffer();

    u32 count = cmd_buff[1];
    u32 frd_key_addr = cmd_buff[3];
    u32 attr_flags_addr = cmd_buff[65];

    for (u32 i = 0; i < count; ++i) {
        // TODO:(mailwl) figure out AttributeFlag size and zero all buffer. Assume 1 byte
        Memory::Write8(attr_flags_addr + i, 0);
    }

    cmd_buff[1] = RESULT_SUCCESS.raw; // No error
    LOG_WARNING(Service_FRD,
                "(STUBBED) called, count=%d, frd_key_addr=0x%08X, attr_flags_addr=0x%08X", count,
                frd_key_addr, attr_flags_addr);
}

void GetMyFriendKey(Interface* self) {
    u32* cmd_buff = Kernel::GetCommandBuffer();

    cmd_buff[1] = RESULT_SUCCESS.raw; // No error
    std::memcpy(&cmd_buff[2], &my_friend_key, sizeof(FriendKey));
    LOG_WARNING(Service_FRD, "(STUBBED) called");
}

void GetMyScreenName(Interface* self) {
    u32* cmd_buff = Kernel::GetCommandBuffer();

    cmd_buff[1] = RESULT_SUCCESS.raw; // No error
    // TODO: (mailwl) get the name from config
    Common::UTF8ToUTF16("Citra").copy(reinterpret_cast<char16_t*>(&cmd_buff[2]), 11);
    LOG_WARNING(Service_FRD, "(STUBBED) called");
}

void UnscrambleLocalFriendCode(Service::Interface* self) {
    const size_t scrambled_friend_code_size = 12;
    const size_t friend_code_size = 8;

    IPC::RequestParser rp(Kernel::GetCommandBuffer(), 0x1C, 1, 2);
    const u32 friend_code_count = rp.Pop<u32>();
    size_t in_buffer_size;
    const VAddr scrambled_friend_codes = rp.PopStaticBuffer(&in_buffer_size);
    ASSERT_MSG(in_buffer_size == (friend_code_count * scrambled_friend_code_size),
               "Wrong input buffer size");

    size_t out_buffer_size;
    VAddr unscrambled_friend_codes = rp.PeekStaticBuffer(0, &out_buffer_size);
    ASSERT_MSG(out_buffer_size == (friend_code_count * friend_code_size),
               "Wrong output buffer size");

    for (u32 current = 0; current < friend_code_count; ++current) {
        // TODO(B3N30): Unscramble the codes and compare them against the friend list
        //              Only write 0 if the code isn't in friend list, otherwise write the
        //              unscrambled one
        //
        // Code for unscrambling (should be compared to HW):
        // std::array<u16, 6> scambled_friend_code;
        // Memory::ReadBlock(scrambled_friend_codes+(current*scrambled_friend_code_size),
        // scambled_friend_code.data(), scrambled_friend_code_size); std::array<u16, 4>
        // unscrambled_friend_code; unscrambled_friend_code[0] = scambled_friend_code[0] ^
        // scambled_friend_code[5]; unscrambled_friend_code[1] = scambled_friend_code[1] ^
        // scambled_friend_code[5]; unscrambled_friend_code[2] = scambled_friend_code[2] ^
        // scambled_friend_code[5]; unscrambled_friend_code[3] = scambled_friend_code[3] ^
        // scambled_friend_code[5];

        u64 result = 0ull;
        Memory::WriteBlock(unscrambled_friend_codes + (current * sizeof(result)), &result,
                           sizeof(result));
    }

    LOG_WARNING(Service_FRD, "(STUBBED) called");
    IPC::RequestBuilder rb = rp.MakeBuilder(1, 2);
    rb.Push(RESULT_SUCCESS);
    rb.PushStaticBuffer(unscrambled_friend_codes, out_buffer_size, 0);
}

void SetClientSdkVersion(Interface* self) {
    IPC::RequestParser rp(Kernel::GetCommandBuffer(), 0x32, 1, 1);
    const u32 version = rp.Pop<u32>();
    self->SetVersion(version);
    const u32 processid_header = rp.Pop<u32>();
    LOG_WARNING(Service_FRD, "(STUBBED) called, version: 0x%08X, ProcessID Header=0x%x", version, processid_header);
    IPC::RequestBuilder rb = rp.MakeBuilder(1, 0);
    rb.Push(RESULT_SUCCESS);
}

void Init() {
    using namespace Kernel;

    AddService(new FRD_A_Interface);
    AddService(new FRD_U_Interface);
}

void Shutdown() {}

} // namespace FRD
} // namespace Service
