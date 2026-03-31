#pragma once

#include <cstddef>
#include <cstdint>

#include "lyra/runtime/process_frame.hpp"

namespace lyra::runtime {

// Per-instance owned storage.
// Each RuntimeInstance owns its module-local state. Storage is always
// heap-allocated via AllocateOwnedInlineStorage / AllocateOwnedAppendixStorage
// and freed by FreeRuntimeInstanceStorage.
// The inline region holds fixed-offset slot data; the appendix region holds
// backing data for owned containers.
struct RuntimeInstanceStorage {
  std::byte* inline_base = nullptr;
  uint64_t inline_size = 0;

  std::byte* appendix_base = nullptr;
  uint64_t appendix_size = 0;
};

// Free any owned storage in an instance's storage record.
void FreeRuntimeInstanceStorage(RuntimeInstanceStorage& storage);

// Runtime-owned representation of one module instance.
//
// This is the first-class object model introduced by R1. A RuntimeInstance
// owns its module-local state, carries stable identity for runtime services
// (%m, observers), and is the execution anchor for module-local process code.
//
// Replaces the prior de facto spread of instance identity across
// StagedProcess, ProcessFrameHeader, InstanceLedgerEntry, and arena offsets.
//
// Binary contract: field order and types must match the LLVM struct type
// emitted by BuildRuntimeInstanceType / BuildRuntimeInstanceStorageType
// in layout.cpp. The field enums below are the single canonical source of
// field ordering for codegen GEP access.
struct RuntimeInstance {
  ~RuntimeInstance() {
    FreeRuntimeInstanceStorage(storage);
  }

  RuntimeInstance() = default;
  RuntimeInstance(const RuntimeInstance&) = delete;
  auto operator=(const RuntimeInstance&) -> RuntimeInstance& = delete;
  RuntimeInstance(RuntimeInstance&&) = delete;
  auto operator=(RuntimeInstance&&) -> RuntimeInstance& = delete;

  uint32_t instance_id = 0;
  SharedBodyFn body = nullptr;

  RuntimeInstanceStorage storage;

  const char* path_c_str = nullptr;
  uint32_t owner_ordinal = 0;

  // Process binding: all module processes for this instance share one object.
  uint32_t module_proc_base = 0;
  uint32_t num_module_processes = 0;

  // Temporary flat slot-id base for the pre-R3 runtime boundary.
  // Current code still computes (base + local_id) and feeds globally-indexed
  // engine APIs. This will be replaced by true engine-private dense
  // coordination indexing. The value is still the old placement-derived
  // design-global base offset.
  uint32_t local_signal_coord_base = 0;
};

// Strongly typed field indices for RuntimeInstanceStorage.
// Must match the LLVM struct type emitted by BuildRuntimeInstanceStorageType.
enum class RuntimeInstanceStorageField : unsigned {
  kInlineBase = 0,
  kInlineSize = 1,
  kAppendixBase = 2,
  kAppendixSize = 3,
  kFieldCount = 4,
};

// Strongly typed field indices for RuntimeInstance.
// Must match the LLVM struct type emitted by BuildRuntimeInstanceType.
enum class RuntimeInstanceField : unsigned {
  kInstanceId = 0,
  kBody = 1,
  kStorage = 2,
  kPathCStr = 3,
  kOwnerOrdinal = 4,
  kModuleProcBase = 5,
  kNumModuleProcesses = 6,
  kLocalSignalCoordBase = 7,
  kFieldCount = 8,
};

// Hard binary contract assertions for RuntimeInstanceStorage.
// If the struct layout changes, these fail at compile time.
static_assert(offsetof(RuntimeInstanceStorage, inline_base) == 0);
static_assert(
    offsetof(RuntimeInstanceStorage, inline_size) ==
    offsetof(RuntimeInstanceStorage, inline_base) + sizeof(std::byte*));
static_assert(
    offsetof(RuntimeInstanceStorage, appendix_base) ==
    offsetof(RuntimeInstanceStorage, inline_size) + sizeof(uint64_t));
static_assert(
    offsetof(RuntimeInstanceStorage, appendix_size) ==
    offsetof(RuntimeInstanceStorage, appendix_base) + sizeof(std::byte*));

// Hard binary contract assertions for RuntimeInstance.
// Field order must match RuntimeInstanceField enum and LLVM struct type.
static_assert(offsetof(RuntimeInstance, instance_id) == 0);
static_assert(
    offsetof(RuntimeInstance, body) > offsetof(RuntimeInstance, instance_id));
static_assert(
    offsetof(RuntimeInstance, storage) ==
    offsetof(RuntimeInstance, body) + sizeof(SharedBodyFn));
static_assert(
    offsetof(RuntimeInstance, path_c_str) ==
    offsetof(RuntimeInstance, storage) + sizeof(RuntimeInstanceStorage));
static_assert(
    offsetof(RuntimeInstance, owner_ordinal) ==
    offsetof(RuntimeInstance, path_c_str) + sizeof(const char*));
static_assert(
    offsetof(RuntimeInstance, module_proc_base) ==
    offsetof(RuntimeInstance, owner_ordinal) + sizeof(uint32_t));
static_assert(
    offsetof(RuntimeInstance, num_module_processes) ==
    offsetof(RuntimeInstance, module_proc_base) + sizeof(uint32_t));
static_assert(
    offsetof(RuntimeInstance, local_signal_coord_base) ==
    offsetof(RuntimeInstance, num_module_processes) + sizeof(uint32_t));

// Allocate zero-initialized owned storage for an instance's inline region.
auto AllocateOwnedInlineStorage(uint64_t size) -> std::byte*;

// Allocate zero-initialized owned storage for an instance's appendix region.
auto AllocateOwnedAppendixStorage(uint64_t size) -> std::byte*;

}  // namespace lyra::runtime
