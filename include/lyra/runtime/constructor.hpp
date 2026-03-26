#pragma once

#include <cstdint>
#include <deque>
#include <span>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

#include "lyra/runtime/body_realization_desc.hpp"
#include "lyra/runtime/engine_types.hpp"
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

// Non-owning view of a trigger metadata template.
// Transport shape for the C ABI boundary. Both body and connection
// trigger templates are projected through this same view type.
// Constructor consumes triggers by explicit local process ordinal
// via proc_ranges[ordinal].
struct TriggerTemplateView {
  std::span<const TriggerTemplateEntry> entries;
  std::span<const TriggerRange> proc_ranges;
  std::span<const uint8_t> proc_shapes;
  std::span<const uint8_t> proc_groupable;
};

// Non-owning view of a comb kernel metadata template.
struct CombTemplateView {
  std::span<const CombTemplateEntry> entries;
  std::span<const CombKernelDesc> kernels;
};

// Non-owning view of an observable descriptor template.
// Transport shape for the C ABI boundary. Both body and package/global
// descriptor templates are projected through this same view type.
struct ObservableDescriptorTemplateView {
  std::span<const ObservableDescriptorEntry> entries;
  const char* pool = nullptr;
  uint32_t pool_size = 0;
};

// Non-owning view of init patch entries for the C ABI boundary.
struct InitPatchView {
  std::span<const InitPatchEntry> entries;
};

// Non-owning view of init handle entries for the C ABI boundary.
struct InitHandleView {
  std::span<const InitHandleEntry> entries;
};

// Non-owning view of param init slot template entries for the C ABI boundary.
struct ParamInitView {
  std::span<const ParamInitSlotEntry> slots;
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
  TriggerTemplateView triggers;
  CombTemplateView comb;
  ObservableDescriptorTemplateView observable_descriptors;
  InitPatchView init_patches;
  InitHandleView init_handles;
  ParamInitView init_params;
};

// Constructor-produced trigger metadata artifact.
// Builder/owner that encapsulates header-patch bookkeeping.
struct RealizedTriggerMeta {
  std::vector<uint32_t> words;
  uint32_t entry_count = 0;

  void Init();
  void AppendEntry(
      uint32_t proc_idx, uint32_t slot_id, uint32_t edge, uint32_t flags);
  void Finalize();

  // Header-only word table (entry_count == 0) is semantically empty.
  [[nodiscard]] auto IsEmpty() const -> bool {
    return entry_count == 0;
  }
};

// Constructor-produced comb kernel metadata artifact.
// Builder/owner that encapsulates header-patch bookkeeping.
struct RealizedCombMeta {
  std::vector<uint32_t> words;
  uint32_t kernel_count = 0;

  void Init();
  auto BeginKernel(uint32_t proc_idx, uint32_t flags) -> uint32_t;
  void AppendTrigger(uint32_t slot_id, uint32_t byte_off, uint32_t byte_size);
  void EndKernel(uint32_t trigger_count_pos, uint32_t trigger_count);
  void Finalize();

  // Header-only word table (kernel_count == 0) is semantically empty.
  [[nodiscard]] auto IsEmpty() const -> bool {
    return kernel_count == 0;
  }
};

// Constructor-produced slot metadata artifact.
// Builder that produces realized slot-meta word tables in the current
// slot_meta_abi format.
struct RealizedSlotMeta {
  std::vector<uint32_t> words;
  uint32_t slot_count = 0;

  void Init();
  void AppendSlot(
      uint32_t byte_offset, uint32_t total_bytes, uint32_t storage_kind,
      uint32_t value_off, uint32_t value_bytes, uint32_t unk_off,
      uint32_t unk_bytes, uint32_t storage_owner_slot_id);
  void Finalize();
};

// Constructor-produced trace signal metadata artifact.
// Builder that produces realized trace-meta word tables and string pool
// in the current trace_signal_meta_abi format.
struct RealizedTraceSignalMeta {
  std::vector<uint32_t> words;
  std::vector<char> pool;
  uint32_t signal_count = 0;

  void Init();
  auto AppendName(std::string_view name) -> uint32_t;
  auto AppendHierarchicalName(
      std::string_view prefix, std::string_view local_name) -> uint32_t;
  void AppendSignal(
      uint32_t name_pool_off, uint32_t bit_width, uint32_t trace_kind,
      uint32_t storage_owner_slot_id);
  void Finalize();
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

  // Constructor-produced trigger metadata.
  RealizedTriggerMeta trigger_meta;

  // Constructor-produced comb kernel metadata.
  RealizedCombMeta comb_meta;

  // Constructor-produced slot metadata.
  RealizedSlotMeta slot_meta;

  // Constructor-produced trace signal metadata.
  RealizedTraceSignalMeta trace_signal_meta;

  // Constructor-owned instance paths for runtime naming (%m).
  std::vector<std::string> instance_paths;
  // Stable C-string pointer view into instance_paths.
  // Populated during Finalize, valid for the lifetime of this result.
  std::vector<const char*> instance_path_ptrs;

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
      std::span<std::byte> design_state, ProcessMetaTemplateView conn_meta,
      TriggerTemplateView conn_triggers,
      ObservableDescriptorTemplateView pkg_observable,
      InitPatchView pkg_init_patches, InitHandleView pkg_init_handles);

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
  // param_data/param_data_size carry pre-lowered canonical storage bytes
  // for this instance's parameter initialization. nullptr/0 if no params.
  // Fails immediately if no active body.
  void AddInstance(
      const char* instance_path, const void* param_data,
      uint32_t param_data_size);

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
    // Body-local state size from BodyRealizationDesc. Body-local
    // sizing metadata, not realized instance placement. Realized byte
    // placement comes from slot_byte_offsets_ (forwarding-aware).
    uint64_t total_state_size_bytes = 0;
    std::span<const BodyProcessEntry> entries;
    ProcessMetaTemplateView meta;
    TriggerTemplateView triggers;
    CombTemplateView comb;
    ObservableDescriptorTemplateView observable_descriptors;
    InitPatchView init_patches;
    InitHandleView init_handles;
    ParamInitView init_params;
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

  // Connection trigger template (immutable after construction).
  TriggerTemplateView conn_triggers_;

  // Active body descriptor (set atomically by BeginBody).
  ActiveBodyDescriptor body_;

  std::vector<StagedProcess> staged_;
  uint32_t num_connection_ = 0;
  bool connections_finalized_ = false;
  bool finalized_ = false;

  // Realized metadata output.
  RealizedProcessMeta realized_meta_;

  // Realized trigger/comb output.
  RealizedTriggerMeta realized_triggers_;
  RealizedCombMeta realized_comb_;

  // Realized slot/trace metadata output.
  RealizedSlotMeta realized_slot_meta_;
  RealizedTraceSignalMeta realized_trace_meta_;

  // Constructor-owned instance paths.
  std::vector<std::string> instance_paths_;

  // Package/global observable descriptor template (immutable after
  // construction).
  ObservableDescriptorTemplateView pkg_observable_;

  // Package/global init descriptor views (immutable after construction).
  InitPatchView pkg_init_patches_;
  InitHandleView pkg_init_handles_;

  // Process-index verification infrastructure.
  struct InstanceLedgerEntry {
    uint32_t owner_ordinal;
    uint32_t module_proc_base;
    uint32_t num_processes;
  };
  std::vector<InstanceLedgerEntry> instance_ledger_;
  uint32_t next_module_instance_ordinal_ = 0;

  enum class TemplateDomain : uint8_t { kConnection, kModule };
  struct TriggerProvenanceRecord {
    TemplateDomain domain;
    uint32_t owner_ordinal;
    uint32_t local_ordinal;
    uint32_t realized_proc_idx;
  };
  struct CombProvenanceRecord {
    uint32_t owner_ordinal;
    uint32_t proc_within_body;
    uint32_t realized_proc_idx;
  };
  std::vector<TriggerProvenanceRecord> trigger_provenance_;
  std::vector<CombProvenanceRecord> comb_provenance_;

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
    uint32_t conn_meta_pool_size,
    const lyra::runtime::TriggerTemplateEntry* conn_trigger_entries,
    uint32_t num_conn_trigger_entries,
    const lyra::runtime::TriggerRange* conn_trigger_ranges,
    uint32_t num_conn_trigger_ranges, const uint8_t* conn_trigger_shapes,
    const uint8_t* conn_trigger_groupable,
    const lyra::runtime::ObservableDescriptorEntry* pkg_obs_entries,
    uint32_t num_pkg_obs, const char* pkg_obs_pool, uint32_t pkg_obs_pool_size,
    const lyra::runtime::InitPatchEntry* pkg_init_patches,
    uint32_t num_pkg_init_patches,
    const lyra::runtime::InitHandleEntry* pkg_init_handles,
    uint32_t num_pkg_init_handles) -> void*;

void LyraConstructorAddConnection(
    void* ctor, const lyra::runtime::ConnectionRealizationDesc* desc);

void LyraConstructorBeginBody(
    void* ctor, const lyra::runtime::BodyRealizationDesc* desc,
    const lyra::runtime::BodyProcessEntry* entries, uint32_t num_entries,
    const lyra::runtime::ProcessMetaTemplateEntry* meta_entries,
    uint32_t num_meta, const char* meta_pool, uint32_t meta_pool_size,
    const lyra::runtime::TriggerTemplateEntry* trigger_entries,
    uint32_t num_trigger_entries,
    const lyra::runtime::TriggerRange* trigger_ranges,
    uint32_t num_trigger_ranges, const uint8_t* trigger_shapes,
    const uint8_t* trigger_groupable,
    const lyra::runtime::CombTemplateEntry* comb_entries,
    uint32_t num_comb_entries,
    const lyra::runtime::CombKernelDesc* comb_kernels,
    uint32_t num_comb_kernels,
    const lyra::runtime::ObservableDescriptorEntry* obs_entries,
    uint32_t num_obs, const char* obs_pool, uint32_t obs_pool_size,
    const lyra::runtime::InitPatchEntry* init_patches,
    uint32_t num_init_patches,
    const lyra::runtime::InitHandleEntry* init_handles,
    uint32_t num_init_handles,
    const lyra::runtime::ParamInitSlotEntry* init_param_slots,
    uint32_t num_init_param_slots);

void LyraConstructorAddInstance(
    void* ctor, const char* instance_path, const void* param_data,
    uint32_t param_data_size);

auto LyraConstructorFinalize(void* ctor) -> void*;

auto LyraConstructionResultGetStates(void* result) -> void**;
auto LyraConstructionResultGetNumTotal(void* result) -> uint32_t;
auto LyraConstructionResultGetNumConnection(void* result) -> uint32_t;

auto LyraConstructionResultGetProcessMetaWords(void* result) -> const uint32_t*;
auto LyraConstructionResultGetProcessMetaWordCount(void* result) -> uint32_t;
auto LyraConstructionResultGetProcessMetaPool(void* result) -> const char*;
auto LyraConstructionResultGetProcessMetaPoolSize(void* result) -> uint32_t;

auto LyraConstructionResultGetTriggerWords(void* result) -> const uint32_t*;
auto LyraConstructionResultGetTriggerWordCount(void* result) -> uint32_t;
auto LyraConstructionResultGetCombWords(void* result) -> const uint32_t*;
auto LyraConstructionResultGetCombWordCount(void* result) -> uint32_t;

auto LyraConstructionResultGetSlotMetaWords(void* result) -> const uint32_t*;
auto LyraConstructionResultGetSlotMetaCount(void* result) -> uint32_t;

auto LyraConstructionResultGetTraceSignalMetaWords(void* result)
    -> const uint32_t*;
auto LyraConstructionResultGetTraceSignalMetaWordCount(void* result)
    -> uint32_t;
auto LyraConstructionResultGetTraceSignalMetaPool(void* result) -> const char*;
auto LyraConstructionResultGetTraceSignalMetaPoolSize(void* result) -> uint32_t;

auto LyraConstructionResultGetInstancePaths(void* result) -> const char**;
auto LyraConstructionResultGetInstancePathCount(void* result) -> uint32_t;

void LyraConstructionResultDestroy(void* result);

}  // extern "C"
