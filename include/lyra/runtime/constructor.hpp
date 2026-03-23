#pragma once

#include <cstdint>
#include <deque>
#include <span>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

#include "lyra/runtime/body_realization_desc.hpp"
#include "lyra/runtime/process_frame.hpp"
#include "lyra/runtime/process_schema.hpp"

namespace lyra::runtime {

// Constructor-produced process metadata artifact.
// Owned by Constructor during realization, moved into ConstructionResult
// at Finalize.
struct RealizedProcessMeta {
  std::vector<uint32_t> words;
  std::vector<char> pool;
};

// Non-owning view of a process metadata template.
// Transport shape for the constructor API only. References emitted LLVM
// globals via the C ABI boundary. The owning form
// (Layout::OwnedProcessMetaTemplate) is the source of truth.
struct ProcessMetaTemplateView {
  std::span<const ProcessMetaTemplateEntry> entries;
  const char* pool = nullptr;
  uint32_t pool_size = 0;
};

// Complete body descriptor for one BeginBody call.
// Packages process entries and metadata template into one coherent unit.
// This is a constructor-facing transport view assembled from emitted globals
// at the C ABI boundary. The emitted body descriptor package (the set of
// LLVM globals for one body) is the first-class compile-time concept;
// this struct is its runtime-facing projection.
struct BodyDescriptorPackage {
  const BodyRealizationDesc* desc = nullptr;
  std::span<const BodyProcessEntry> entries;
  ProcessMetaTemplateView meta;
};

// Result of constructor-time process realization.
//
// This is one ownership unit containing all constructor-produced artifacts.
// main() receives this result, passes the process set to LyraRunSimulation,
// and destroys the result after simulation completes.
struct ConstructionResult {
  // Heap-allocated pointer array: one entry per simulation process.
  // Ordered: connections [0, num_connection), modules [num_connection,
  // num_total).
  std::vector<void*> states;

  // Packed process state buffer backing all frame allocations.
  // Owned by this result. Freed in destructor.
  void* packed_buffer = nullptr;

  // Total simulation processes (connection + module).
  uint32_t num_total = 0;

  // Partition boundary: processes [0, num_connection) are connection processes.
  uint32_t num_connection = 0;

  // Constructor-produced process metadata.
  RealizedProcessMeta process_meta;

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
// slot-base progression, frame allocation, frame header binding, and
// process metadata realization.
//
// Both connection and module processes are realized through this same
// object, producing one unified construction result.
//
// The emitted constructor program drives this object in instance-major
// order (one AddInstance per module instance, in ModuleIndex order).
// BeginBody sets the active body descriptor for subsequent AddInstance
// calls. Body switching is expected: the same body may be set multiple
// times as the program interleaves instances of different bodies.
//
// Usage (instance-major):
//   Constructor ctor(schemas, slot_byte_offsets, num_package_slots,
//                    design_state, conn_meta);
//   ctor.AddConnection(conn_desc_0);
//   ctor.AddConnection(conn_desc_1);
//   ctor.BeginBody(body_a_package);
//   ctor.AddInstance("top.u0");
//   ctor.BeginBody(body_b_package);
//   ctor.AddInstance("top.u1");
//   ctor.BeginBody(body_a_package);
//   ctor.AddInstance("top.u2");
//   auto result = ctor.Finalize();
//
// After Finalize(), the constructor rejects further mutation.
class Constructor {
 public:
  Constructor(
      std::span<const ProcessStateSchema> schemas,
      std::span<const uint64_t> slot_byte_offsets, uint32_t num_package_slots,
      std::span<std::byte> design_state, ProcessMetaTemplateView conn_meta);

  // Add a connection process. Must be called before any BeginBody/AddInstance.
  // Fails immediately if connection template is exhausted.
  void AddConnection(const ConnectionRealizationDesc& desc);

  // Set the active body descriptor for subsequent AddInstance calls.
  // May be called multiple times with the same or different bodies.
  // Validates package coherence: entries.size() must equal
  // meta.entries.size().
  void BeginBody(const BodyDescriptorPackage& package);

  // Add one instance of the current body. instance_path is the
  // hierarchical path for this instance (constructor-owned architecture:
  // the constructor is the canonical consumer of per-instance paths).
  // Fails immediately if no active body.
  void AddInstance(const char* instance_path);

  // Finalize construction and return the unified result.
  // After this call, further mutation is rejected.
  [[nodiscard]] auto Finalize() -> ConstructionResult;

 private:
  void CheckNotFinalized(const char* caller) const;

  // Constructor-private runtime staging for a single process.
  struct StagedProcess {
    uint32_t schema_index;
    SharedBodyFn body;
    void* this_ptr;
    uint32_t instance_id;
    uint32_t signal_id_offset;
    bool is_module;
  };

  // Currently active body descriptor view.
  // All fields set atomically by BeginBody, consumed by AddInstance.
  struct ActiveBodyDescriptor {
    uint32_t slot_count = 0;
    std::span<const BodyProcessEntry> entries;
    ProcessMetaTemplateView meta;
    bool active = false;
  };

  std::span<const ProcessStateSchema> schemas_;
  std::span<const uint64_t> slot_byte_offsets_;
  std::span<std::byte> design_state_;

  uint32_t next_instance_id_ = 0;
  uint32_t next_slot_base_ = 0;

  // Connection metadata template (immutable after construction).
  ProcessMetaTemplateView conn_meta_;
  uint32_t conn_meta_index_ = 0;

  // Active body descriptor (set atomically by BeginBody).
  ActiveBodyDescriptor body_;

  std::vector<StagedProcess> staged_;
  uint32_t num_connection_ = 0;
  bool connections_finalized_ = false;
  bool finalized_ = false;

  // Realized metadata output.
  RealizedProcessMeta realized_meta_;

  // Stable string storage for intern map keys.
  // std::deque never invalidates existing entries on push_back,
  // so string_view keys into this container remain valid for the
  // lifetime of the constructor.
  std::deque<std::string> interned_strings_;
  std::unordered_map<std::string_view, uint32_t> string_intern_;

  // Intern a file string from a validated template view into the
  // realized pool. All call sites pass views validated at boundary
  // setup (BeginBody or constructor creation). Returns 0 for empty.
  auto InternString(std::span<const char> pool, uint32_t pool_off) -> uint32_t;

  // Append a string to the realized pool (NUL-terminated).
  // Returns the pool offset. No dedup.
  auto AppendString(std::string_view s) -> uint32_t;
};

}  // namespace lyra::runtime

// C ABI surface for the emitted constructor function.
// These are called from LLVM IR generated by EmitDesignMain.
//
// The C ABI decomposes C++ types into raw pointers for the LLVM IR
// boundary. The C++ Constructor API (above) is the real shape.
extern "C" {

auto LyraConstructorCreate(
    const lyra::runtime::ProcessStateSchema* schemas, uint32_t num_schemas,
    const uint64_t* slot_byte_offsets, uint32_t num_slots,
    uint32_t num_package_slots, void* design_state, uint64_t design_state_size,
    const lyra::runtime::ProcessMetaTemplateEntry* conn_meta_entries,
    uint32_t num_conn_meta, const char* conn_meta_pool,
    uint32_t conn_meta_pool_size) -> void*;

void LyraConstructorAddConnection(
    void* ctor, const lyra::runtime::ConnectionRealizationDesc* desc);

void LyraConstructorBeginBody(
    void* ctor, const lyra::runtime::BodyRealizationDesc* desc,
    const lyra::runtime::BodyProcessEntry* entries, uint32_t num_entries,
    const lyra::runtime::ProcessMetaTemplateEntry* meta_entries,
    uint32_t num_meta, const char* meta_pool, uint32_t meta_pool_size);

void LyraConstructorAddInstance(void* ctor, const char* instance_path);

auto LyraConstructorFinalize(void* ctor) -> void*;

auto LyraConstructionResultGetStates(void* result) -> void**;
auto LyraConstructionResultGetNumTotal(void* result) -> uint32_t;
auto LyraConstructionResultGetNumConnection(void* result) -> uint32_t;

auto LyraConstructionResultGetProcessMetaWords(void* result) -> const uint32_t*;
auto LyraConstructionResultGetProcessMetaWordCount(void* result) -> uint32_t;
auto LyraConstructionResultGetProcessMetaPool(void* result) -> const char*;
auto LyraConstructionResultGetProcessMetaPoolSize(void* result) -> uint32_t;

void LyraConstructionResultDestroy(void* result);

}  // extern "C"
