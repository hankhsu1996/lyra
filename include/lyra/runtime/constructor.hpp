#pragma once

#include <cstdint>
#include <span>
#include <vector>

#include "lyra/runtime/body_realization_desc.hpp"
#include "lyra/runtime/process_frame.hpp"
#include "lyra/runtime/process_schema.hpp"

namespace lyra::runtime {

// Result of constructor-time process realization.
//
// This is one ownership unit containing all constructor-produced artifacts.
// main() receives this result, passes the process set to LyraRunSimulation,
// and destroys the result after simulation completes.
//
// H3-H5 may extend this with additional constructor-produced data
// (metadata tables, trigger tables, etc.). The single-owner model supports
// this: the result grows, but ownership remains one object.
struct ConstructionResult {
  // Heap-allocated pointer array: one entry per simulation process.
  // Ordered: connections [0, num_connection), modules [num_connection,
  // num_total).
  std::vector<void*> states;

  // Packed process state buffer backing all frame allocations.
  // Owned by this result. Freed in destructor.
  // nullptr is valid (empty construction or moved-from state).
  void* packed_buffer = nullptr;

  // Total simulation processes (connection + module).
  uint32_t num_total = 0;

  // Partition boundary: processes [0, num_connection) are connection processes.
  uint32_t num_connection = 0;

  ConstructionResult() = default;
  ~ConstructionResult();

  ConstructionResult(const ConstructionResult&) = delete;
  auto operator=(const ConstructionResult&) -> ConstructionResult& = delete;
  ConstructionResult(ConstructionResult&& other) noexcept;
  auto operator=(ConstructionResult&& other) noexcept -> ConstructionResult&;
};

// Runtime-owned construction object for process realization.
//
// Owns all constructor-time mutable state: running instance identity,
// slot-base progression, frame allocation, and frame header binding.
// Both connection and module processes are realized through this same
// object, producing one unified construction result.
//
// The emitted constructor program drives this object in instance-major
// order (one AddInstance per module instance, in ModuleIndex order).
// BeginBody sets the active body for subsequent AddInstance calls.
// Body switching is expected: the same body may be set multiple times
// as the program interleaves instances of different bodies. This is
// the H2 compatibility shape that preserves the old instance-major
// process ordering required by H3-H5 metadata tables.
//
// Usage (instance-major):
//   Constructor ctor(schemas, slot_byte_offsets, num_package_slots,
//   design_state); ctor.AddConnection(conn_desc);
//   // Instance-major: switch bodies as needed
//   ctor.BeginBody(body_a_desc, body_a_entries);
//   ctor.AddInstance();   // instance 0 (body A)
//   ctor.BeginBody(body_b_desc, body_b_entries);
//   ctor.AddInstance();   // instance 1 (body B)
//   ctor.BeginBody(body_a_desc, body_a_entries);
//   ctor.AddInstance();   // instance 2 (body A again)
//   auto result = ctor.Finalize();
//
// After Finalize(), the constructor rejects further mutation.
class Constructor {
 public:
  Constructor(
      std::span<const ProcessStateSchema> schemas,
      std::span<const uint64_t> slot_byte_offsets, uint32_t num_package_slots,
      std::span<std::byte> design_state);

  // Add a connection process. Must be called before any BeginBody/AddInstance.
  void AddConnection(const ConnectionRealizationDesc& desc);

  // Set the active body for subsequent AddInstance calls. May be called
  // multiple times with the same or different bodies as the emitted
  // constructor program walks instances in instance-major order.
  void BeginBody(
      const BodyRealizationDesc& desc,
      std::span<const BodyProcessEntry> entries);

  // Add one instance of the current body. The constructor derives
  // instance_id, signal_id_offset, and base_byte_offset from running
  // counters -- no per-instance data is needed from the caller.
  void AddInstance();

  // Finalize construction and return the unified result.
  // After this call, further mutation is rejected.
  [[nodiscard]] auto Finalize() -> ConstructionResult;

 private:
  void CheckNotFinalized(const char* caller) const;

  // Constructor-private runtime staging for a single process.
  // This is transient internal state, not an artifact model.
  // Binding values are fully resolved at staging time so Finalize
  // only copies them to frame headers without casts or arithmetic.
  struct StagedProcess {
    uint32_t schema_index;
    // Module-process binding (null/zero for connections).
    SharedBodyFn body;
    void* this_ptr;
    uint32_t instance_id;
    uint32_t signal_id_offset;
    bool is_module;
  };

  std::span<const ProcessStateSchema> schemas_;
  std::span<const uint64_t> slot_byte_offsets_;
  std::span<std::byte> design_state_;

  uint32_t next_instance_id_ = 0;
  uint32_t next_slot_base_ = 0;

  // Current body state, copied by value from BeginBody arguments.
  bool has_body_ = false;
  uint32_t current_slot_count_ = 0;
  std::span<const BodyProcessEntry> current_entries_;

  std::vector<StagedProcess> staged_;
  uint32_t num_connection_ = 0;
  bool connections_finalized_ = false;
  bool finalized_ = false;
};

}  // namespace lyra::runtime

// C ABI surface for the emitted constructor function.
// These are called from LLVM IR generated by EmitDesignMain.
//
// The constructor produces a single runtime-owned ConstructionResult.
// Emitted code receives an opaque handle and extracts states/counts
// via accessor functions. The result owns both the states pointer
// array and the packed frame buffer as one ownership unit.
extern "C" {

auto LyraConstructorCreate(
    const lyra::runtime::ProcessStateSchema* schemas, uint32_t num_schemas,
    const uint64_t* slot_byte_offsets, uint32_t num_slots,
    uint32_t num_package_slots, void* design_state, uint64_t design_state_size)
    -> void*;

void LyraConstructorAddConnection(
    void* ctor, const lyra::runtime::ConnectionRealizationDesc* desc);

void LyraConstructorBeginBody(
    void* ctor, const lyra::runtime::BodyRealizationDesc* desc,
    const lyra::runtime::BodyProcessEntry* entries, uint32_t num_entries);

void LyraConstructorAddInstance(void* ctor);

// Finalize construction and return an opaque ConstructionResult handle.
// The result owns all constructor-produced artifacts as one ownership unit.
// Caller must eventually call LyraConstructionResultDestroy.
auto LyraConstructorFinalize(void* ctor) -> void*;

// Accessors for the opaque ConstructionResult handle.
auto LyraConstructionResultGetStates(void* result) -> void**;
auto LyraConstructionResultGetNumTotal(void* result) -> uint32_t;
auto LyraConstructionResultGetNumConnection(void* result) -> uint32_t;

// Destroy the construction result and all owned allocations.
void LyraConstructionResultDestroy(void* result);

}  // extern "C"
