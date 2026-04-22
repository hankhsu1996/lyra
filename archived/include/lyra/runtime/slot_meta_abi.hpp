#pragma once

#include <cstdint>

namespace lyra::runtime::slot_meta_abi {

// ABI version. Bumped when field layout changes.
// Codegen passes this to LyraRunSimulation; runtime validates before parsing.
// v2: Split kHandle(2) into kString(2) + kHandle(3), renumber kAggregate to 4.
// v3: Add storage_owner_slot_id for canonical forwarding.
// v4: Add storage domain (kDesignGlobal vs kInstanceOwned) with per-domain
//     addressing fields. Module-local owned slots use instance identity +
//     body-relative offset instead of arena-absolute base_off.
inline constexpr uint32_t kVersion = 4;

// Number of uint32_t words per row.
inline constexpr uint32_t kStride = 11;

// Field indices within a row (each row is kStride consecutive uint32_t).
// Domain: 0 = kDesignGlobal, 1 = kInstanceOwned.
inline constexpr uint32_t kFieldDomain = 0;
// For kDesignGlobal: arena-absolute byte offset. For kInstanceOwned: unused
// (0).
inline constexpr uint32_t kFieldDesignBaseOff = 1;
// For kInstanceOwned: construction-order index of the owning instance.
// For kDesignGlobal: unused (0).
inline constexpr uint32_t kFieldOwnerInstanceId = 2;
// For kInstanceOwned: body-relative byte offset within the instance's
// inline storage. For kDesignGlobal: unused (0).
inline constexpr uint32_t kFieldInstanceRelOff = 3;
inline constexpr uint32_t kFieldTotalBytes = 4;
inline constexpr uint32_t kFieldKind = 5;
inline constexpr uint32_t kFieldValueOff = 6;
inline constexpr uint32_t kFieldValueBytes = 7;
inline constexpr uint32_t kFieldUnkOff = 8;
inline constexpr uint32_t kFieldUnkBytes = 9;
// Canonical storage owner slot id. For storage owners, equals the slot's
// own id (row index). For forwarded aliases, equals the canonical owner.
inline constexpr uint32_t kFieldStorageOwnerSlotId = 10;

}  // namespace lyra::runtime::slot_meta_abi
