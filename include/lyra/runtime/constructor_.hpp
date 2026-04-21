// NOTE: This file is named constructor_.hpp (with trailing underscore)
// because the Claude Code TUI crashes when editing files whose name
// starts with "constructor". The underscore suffix is a permanent
// workaround. The corresponding source file is constructor_.cpp.
#pragma once

#include <cstdint>
#include <deque>
#include <memory>
#include <span>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

#include "lyra/runtime/body_realization_desc.hpp"
#include "lyra/runtime/construction_program_abi.hpp"
#include "lyra/runtime/engine_types.hpp"
#include "lyra/runtime/instance_metadata.hpp"
#include "lyra/runtime/process_frame.hpp"
#include "lyra/runtime/process_schema.hpp"
#include "lyra/runtime/runtime_instance.hpp"
#include "lyra/runtime/suspend_record.hpp"

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
  StorageConstructionRecipeView init_recipe;
  StorageConstructionRootView init_recipe_roots;
  ParamInitView init_params;
  // Per body-local process decision metadata (parallel to entries).
  std::span<const DecisionTableDescriptor> decision_tables;
};

// Canonical stable storage for a body template. Keyed by body identity
// (BodyRealizationDesc pointer). Bundles hold pointers into .package.
struct StableBodyTemplate {
  const void* key = nullptr;
  BodyDescriptorPackage package;
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

// Constructor-produced slot metadata artifact.
// Builder that produces realized slot-meta word tables in the current
// slot_meta_abi format.
struct RealizedSlotMeta {
  std::vector<uint32_t> words;
  uint32_t slot_count = 0;

  void Init();
  void AppendDesignGlobalSlot(
      uint32_t design_base_off, uint32_t total_bytes, uint32_t storage_kind,
      uint32_t value_off, uint32_t value_bytes, uint32_t unk_off,
      uint32_t unk_bytes, uint32_t storage_owner_slot_id);
  void AppendInstanceOwnedSlot(
      uint32_t owner_instance_id, uint32_t instance_rel_off,
      uint32_t total_bytes, uint32_t storage_kind, uint32_t value_off,
      uint32_t value_bytes, uint32_t unk_off, uint32_t unk_bytes,
      uint32_t storage_owner_slot_id);
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

  // Constructor-produced runtime instance objects.
  // One per module instance, ordered by instance_id.
  // Lifetime: owned by this result, outlives simulation.
  std::vector<std::unique_ptr<RuntimeInstance>> instances;

  // Constructor-produced generate scope objects.
  // Lifetime: owned by this result, outlives simulation.
  std::vector<std::unique_ptr<RuntimeGenerateScope>> generate_scopes;

  // R4 prep: per-instance metadata bundles alongside existing flat handoff.
  // Each bundle binds one module instance to its shared body template.
  // Validated: bundle[i].instance_id == i.
  // Existing flat metadata tables above remain fully populated and are
  // the active runtime handoff in this cut. Bundles become the primary
  // handoff when the engine consumer path is migrated in a later cut.
  std::vector<InstanceMetadataBundle> instance_bundles;

  // Materialized connection descriptors with direct RuntimeInstance*
  // pointers. Produced by LyraConstructionResultSetConnectionDescriptors
  // using direct array access into instances[object_index].
  std::vector<RuntimeConnectionDescriptor> connection_descriptors;

  // Stable storage for body templates referenced by bundles.
  // Bundle body_desc pointers point into .package fields of these entries.
  // Moved from Constructor::body_desc_storage_ during Finalize.
  // Lifetime: owned by this result, outlives simulation.
  std::deque<StableBodyTemplate> body_desc_storage;

  // Constructor-produced process metadata.
  RealizedProcessMeta process_meta;

  // Constructor-produced trigger metadata.
  RealizedTriggerMeta trigger_meta;

  // Constructor-produced slot metadata.
  RealizedSlotMeta slot_meta;

  // Constructor-produced trace signal metadata.
  RealizedTraceSignalMeta trace_signal_meta;

  // Stable raw pointer view into instances for the runtime ABI.
  // Populated during Finalize, valid for the lifetime of this result.
  std::vector<const RuntimeInstance*> instance_ptrs;

  // Resolved installable computations for the engine.
  // Each entry carries a void-returning writeback callable plus its
  // owner instance. The callable writes the child target itself
  // through the body's ExternalRefId binding (populated separately
  // via per-instance ext_ref_bindings). The engine only invokes the
  // callable and registers triggers on dep slots. Populated during
  // Finalize.
  struct ResolvedInstalledComputation {
    ICBodyFn eval_fn = nullptr;
    RuntimeInstance* owner_instance = nullptr;
    // Body-local dependency slot indices on the owner instance.
    std::vector<uint32_t> dep_body_local_slots;
  };
  std::vector<ResolvedInstalledComputation> installed_computations;

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
      StorageConstructionRecipeView pkg_init_recipe,
      StorageConstructionRootView pkg_init_recipe_roots);

  // Add a connection process. Must be called before any BeginBody/AddInstance.
  // Fails immediately if connection template is exhausted.
  void AddConnection(const ConnectionRealizationDesc& desc);

  // Set the active body descriptor for subsequent AddInstance calls.
  // May be called multiple times with the same or different bodies.
  // Validates package coherence: entries.size() must equal
  // meta.entries.size().
  void BeginBody(const BodyDescriptorPackage& package);

  // Create a generate scope node in parent scope context.
  // Hierarchy-only: no storage, no body, no processes.
  // The full hierarchical path is derived inside the constructor from
  // parent_scope->path_storage + label; callers must not transport it.
  auto CreateScope(
      RuntimeScope* parent_scope, const char* label, uint32_t ordinal_in_parent)
      -> RuntimeScope*;

  // Flat-replay entry point: create one child instance of the currently-
  // active body, applying transmitted-parameter values from a pre-lowered
  // contiguous byte buffer. Used only by LyraConstructorRunProgram for
  // bodies that stay on the flat path; the direct-constructor path goes
  // through AllocateObject + a direct call to the emitted child
  // body-constructor.
  // parent_scope: parent scope, or nullptr for top-level (root) instances.
  // instance_index: compile-time object_index (sorted all_instances position).
  //   Used as owner_ordinal for stable indexing.
  // label: scope label for path derivation (e.g. "u0"). The full
  //   hierarchical path is derived from parent_scope + label by the
  //   constructor; callers must not transport full paths.
  // param_data/param_data_size: pre-lowered canonical storage bytes.
  // Returns the created instance (owned by the constructor).
  auto CreateChildInstance(
      RuntimeScope* parent_scope, uint32_t ordinal_in_parent,
      uint32_t instance_index, const char* label, const void* param_data,
      uint32_t param_data_size, uint64_t realized_inline_size,
      uint64_t realized_appendix_size) -> RuntimeInstance*;

  // Pure object allocation: create a RuntimeInstance, allocate its
  // inline + appendix + deferred storage, and return ownership to the
  // caller. Does NOT attach the instance to a scope, apply body init,
  // or stage it. Body activation via BeginBody must precede this call
  // (storage sizing comes from the active body).
  auto AllocateObject(
      uint64_t realized_inline_size, uint64_t realized_appendix_size)
      -> std::unique_ptr<RuntimeInstance>;

  // Attach a freshly-allocated instance to a parent scope. Writes only
  // scope identity and hierarchy: scope kind, parent edge with ordinal,
  // and hierarchical path. Does not touch storage content, process-side
  // state, or the constructor's staged tables. The registration handle
  // (owner_ordinal / staged_instances_ slot) is assigned later by
  // FinalizeInstance.
  void AttachInstanceToScope(
      RuntimeInstance& instance, RuntimeScope* parent_scope,
      uint32_t ordinal_in_parent, const char* label);

  // Apply the active body's storage construction recipe to an attached
  // instance (default-init of body-owned state only). Parameter slots
  // are written by the emitted body-constructor's formal-to-member
  // binding statements and MUST NOT flow through this helper.
  void ApplyBodyInit(RuntimeInstance& instance);

  // Commit an attached + initialized instance into the constructor's
  // staged tables. Registration-only: assigns owner_ordinal to
  // instance_index, places the instance at staged_instances_[instance_
  // index], stages body processes, records design-global observables,
  // writes the metadata bundle, and advances the slot base. Returns the
  // stable instance pointer.
  auto FinalizeInstance(
      std::unique_ptr<RuntimeInstance> instance, uint32_t instance_index)
      -> RuntimeInstance*;

  // Read-only accessor for an already-staged instance by its compile-time
  // object_index. Used by LyraConstructorRunProgram to pick up instances
  // pre-created by an emitted body-constructor when the flat replay loop
  // reaches their entry. Throws if the slot is empty.
  [[nodiscard]] auto GetStagedInstance(uint32_t instance_index) const
      -> RuntimeInstance*;

  // Finalize construction and return the unified result.
  // After this call, further mutation is rejected.
  [[nodiscard]] auto Finalize() -> ConstructionResult;

 private:
  void CheckNotFinalized(const char* caller) const;
  void ClearActiveBody();

  // Shared tail of instance construction for both the flat-replay and
  // direct-constructor paths. Assumes the caller has already allocated
  // the instance storage and applied body-level init (recipe + optional
  // param init). Internally decomposes registration into
  // RegisterStagedInstance, StageInstanceProcesses,
  // RecordInstanceObservables, RecordInstanceBundle, and
  // AdvanceInstanceSlotBase -- each covers exactly one concern; this
  // wrapper exists only for shared sequencing.
  void FinalizeInstanceSetup(
      std::unique_ptr<RuntimeInstance> instance, uint32_t instance_index);

  // Registration step: place `instance` at staged_instances_[index] and
  // stamp owner_ordinal = index. No process, observable, or bundle work.
  void RegisterStagedInstance(
      std::unique_ptr<RuntimeInstance> instance, uint32_t instance_index);

  // Registration step: push a StagedProcess record for every body entry
  // of the active body, pointing at `instance_index`. Bumps
  // next_module_instance_ordinal_. No scope, observable, or bundle work.
  void StageInstanceProcesses(uint32_t instance_index);

  // Registration step: walk the active body's design-global observable
  // entries and append them to realized_slot_meta_ / realized_trace_
  // meta_ using this instance's stable path. Instance-owned entries are
  // skipped (engine-derived from bundles). No scope/process work.
  void RecordInstanceObservables(uint32_t instance_index);

  // Registration step: record the per-instance metadata bundle used
  // by the engine to locate this instance's body descriptor and path.
  // Must run after RegisterStagedInstance so staged_instances_[index]
  // is non-null.
  void RecordInstanceBundle(uint32_t instance_index);

  // Registration step: advance next_slot_base_ by the active body's
  // slot_count and bump next_instance_id_. Runs after all metadata for
  // this instance has been emitted.
  void AdvanceInstanceSlotBase();

  // Constructor-private runtime staging for a single process.
  struct StagedProcess {
    uint32_t schema_index = 0;
    SharedBodyFn body = nullptr;
    // Index into staged_instances_ for module processes, UINT32_MAX for
    // connection processes.
    uint32_t instance_index = UINT32_MAX;
    bool is_module = false;
    // Unused padding (expression connections are no longer processes).
  };

  // Currently active body descriptor view.
  // All fields set atomically by BeginBody, consumed by AddInstance.
  struct ActiveBodyDescriptor {
    uint32_t slot_count = 0;
    // Body-local state sizes from BodyRealizationDesc. Body-local
    // sizing metadata, not realized instance placement. Realized byte
    // placement comes from slot_byte_offsets_ (forwarding-aware).
    uint64_t inline_state_size_bytes = 0;
    uint64_t appendix_state_size_bytes = 0;
    uint64_t total_state_size_bytes = 0;
    std::span<const BodyProcessEntry> entries;
    ProcessMetaTemplateView meta;
    TriggerTemplateView triggers;
    CombTemplateView comb;
    ObservableDescriptorTemplateView observable_descriptors;
    StorageConstructionRecipeView init_recipe;
    StorageConstructionRootView init_recipe_roots;
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

  // Pointer to the current body's stable descriptor (R4).
  // Points into body_desc_storage_. Set by BeginBody, cleared by
  // ClearActiveBody.
  const BodyDescriptorPackage* current_body_package_ = nullptr;

  // Canonical stable storage for body templates (R4). One entry per
  // unique body identity. Bundles hold pointers into .package fields.
  // std::deque: push_back never invalidates existing element addresses.
  std::deque<StableBodyTemplate> body_desc_storage_;

  std::vector<StagedProcess> staged_;
  // Constructor-owned RuntimeInstance objects created during
  // CreateChildInstance. Moved into ConstructionResult at Finalize.
  std::vector<std::unique_ptr<RuntimeInstance>> staged_instances_;
  // Constructor-owned generate scope objects created during CreateScope.
  // Moved into ConstructionResult at Finalize.
  std::vector<std::unique_ptr<RuntimeGenerateScope>> staged_generate_scopes_;
  // R4 prep: per-instance metadata bundles. Moved into ConstructionResult
  // at Finalize.
  std::vector<InstanceMetadataBundle> staged_bundles_;
  uint32_t num_connection_ = 0;
  bool connections_finalized_ = false;
  bool finalized_ = false;

  // Connection-only realized metadata output.
  RealizedProcessMeta realized_meta_;

  // Connection-only realized trigger output.
  RealizedTriggerMeta realized_triggers_;

  // Realized slot/trace metadata output.
  RealizedSlotMeta realized_slot_meta_;
  RealizedTraceSignalMeta realized_trace_meta_;

  // Package/global observable descriptor template (immutable after
  // construction).
  ObservableDescriptorTemplateView pkg_observable_;

  // Package/global storage construction recipe (immutable after construction).
  StorageConstructionRecipeView pkg_init_recipe_;
  StorageConstructionRootView pkg_init_recipe_roots_;

  // Process-index verification infrastructure.
  uint32_t next_module_instance_ordinal_ = 0;

  enum class TemplateDomain : uint8_t { kConnection, kModule };
  struct TriggerProvenanceRecord {
    TemplateDomain domain;
    uint32_t owner_ordinal;
    uint32_t local_ordinal;
    uint32_t realized_proc_idx;
  };
  std::vector<TriggerProvenanceRecord> trigger_provenance_;

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
    const lyra::runtime::StorageConstructionOp* pkg_init_recipe,
    uint32_t num_pkg_init_recipe_ops, const uint32_t* pkg_init_recipe_roots,
    uint32_t num_pkg_init_recipe_roots,
    const uint32_t* pkg_init_recipe_child_indices,
    uint32_t num_pkg_init_recipe_child_indices) -> void*;

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
    const lyra::runtime::StorageConstructionOp* init_recipe,
    uint32_t num_init_recipe_ops, const uint32_t* init_recipe_roots,
    uint32_t num_init_recipe_roots, const uint32_t* init_recipe_child_indices,
    uint32_t num_init_recipe_child_indices,
    const lyra::runtime::ParamInitSlotEntry* init_param_slots,
    uint32_t num_init_param_slots,
    const lyra::runtime::DecisionTableDescriptor* decision_tables,
    uint32_t num_decision_tables);

void LyraConstructorAddInstance(
    void* ctor, const char* instance_path, const void* param_data,
    uint32_t param_data_size, uint64_t realized_inline_size,
    uint64_t realized_appendix_size);

void LyraConstructorRunProgram(
    void* ctor, const lyra::runtime::BodyDescriptorRef* body_descs,
    uint32_t body_desc_count, const char* path_pool, uint32_t path_pool_size,
    const uint8_t* param_pool, uint32_t param_pool_size,
    const lyra::runtime::ConstructionProgramEntry* entries,
    uint32_t entry_count,
    const lyra::runtime::PortConstInitEntry* port_const_inits,
    uint32_t port_const_init_count, const uint8_t* port_const_pool,
    uint32_t port_const_pool_size);

// Activate the child body whose description is packed into `body_ref`.
// Thin shim over Constructor::BeginBody used by backend-emitted
// body-constructor code: each plain-child site starts with a
// begin-body-by-ref call so AllocateObject operates against the correct
// active body package.
void LyraConstructorBeginBodyByRef(
    void* ctor, const lyra::runtime::BodyDescriptorRef* body_ref);

// Allocate a RuntimeInstance under the currently-active body. Returns
// a raw pointer to a constructor-owned holding slot; the object is not
// yet attached to any scope and not yet staged. The caller must follow
// with LyraConstructorAttachAndInitializeObject to commit it.
// Preconditions: BeginBody (or BeginBodyByRef) has been called.
auto LyraConstructorAllocateObject(
    void* ctor, uint64_t realized_inline_size, uint64_t realized_appendix_size)
    -> void*;

// Attach a freshly-allocated object to a parent scope: scope identity,
// parent edge with ordinal, and hierarchical path. Registration-related
// state (owner_ordinal, staged position, bundles) is written later by
// LyraConstructorFinalizeInstance. Object ownership remains with the
// caller (raw pointer handed off by LyraConstructorAllocateObject).
void LyraConstructorAttachInstanceToScope(
    void* ctor, void* object, void* parent_scope, uint32_t ordinal_in_parent,
    const char* label);

// Apply the active body's storage init recipe to the attached object.
void LyraConstructorApplyBodyInit(void* ctor, void* object);

// Commit the fully-initialized object into the constructor's staged
// tables (process staging, observables, bundle, slot-base). Transfers
// ownership into the constructor's staged_instances_ at instance_index.
// Returns the stable instance pointer for use as the callee's
// this_instance argument in the direct-call path.
auto LyraConstructorFinalizeInstance(
    void* ctor, void* object, uint32_t instance_index) -> void*;

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

auto LyraConstructionResultGetInstances(void* result)
    -> const lyra::runtime::RuntimeInstance* const*;
auto LyraConstructionResultGetInstanceCount(void* result) -> uint32_t;

auto LyraConstructionResultGetInstanceBundles(void* result)
    -> const lyra::runtime::InstanceMetadataBundle*;
auto LyraConstructionResultGetInstanceBundleCount(void* result) -> uint32_t;

void LyraConstructionResultDestroy(void* result);

// Set per-instance ext-ref binding records.
// pool: flat array of SerializedExtRefBinding records from codegen.
// Resolves target_instance_id to live RuntimeInstance* and stores
// resolved bindings in each instance's owned_ext_ref_bindings.
// offsets[i]: index into pool for instance i (UINT32_MAX = no ext refs).
// counts[i]: number of bindings for instance i.
// Called after LyraConstructorFinalize, before simulation.
void LyraConstructionResultSetExtRefBindings(
    void* result, const void* pool, uint32_t pool_count,
    const uint32_t* offsets, const uint32_t* counts, uint32_t num_instances);

// Materialize connection descriptors from serialized transport into
// runtime-facing RuntimeConnectionDescriptor with direct RuntimeInstance*
// pointers. Uses result.instances[object_index] -- direct array access,
// no central lookup. Called after Finalize, before simulation.
void LyraConstructionResultSetConnectionDescriptors(
    void* result_raw, const void* serialized_raw, uint32_t num_descs);

// Get the materialized connection descriptor array from the result.
auto LyraConstructionResultGetConnectionDescriptors(void* result) -> void*;

// Get the number of materialized connection descriptors.
auto LyraConstructionResultGetConnectionDescriptorCount(void* result)
    -> uint32_t;

}  // extern "C"
