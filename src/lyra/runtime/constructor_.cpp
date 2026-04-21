// NOTE: See constructor_.hpp for why this file has a trailing underscore.
#include "lyra/runtime/constructor_.hpp"

#include <algorithm>
#include <cstdint>
#include <cstring>
#include <format>
#include <memory>
#include <span>
#include <string_view>
#include <unordered_map>
#include <utility>

#include "lyra/common/ext_ref_binding.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/frame_allocator.hpp"
#include "lyra/runtime/owned_storage_handle.hpp"
#include "lyra/runtime/process_frame.hpp"
#include "lyra/runtime/process_meta_abi.hpp"
#include "lyra/runtime/process_trigger_abi.hpp"
#include "lyra/runtime/slot_meta.hpp"
#include "lyra/runtime/storage_construction.hpp"

namespace lyra::runtime {

// Derive hierarchical path from parent scope context + local scope label.
// All path construction policy lives here.
auto BuildScopePath(const RuntimeScope* parent, const char* label)
    -> std::string {
  if (parent == nullptr) return {label};
  return std::string(parent->path_storage) + "." + label;
}

ConstructionResult::~ConstructionResult() {
  FreePackedBuffer(packed_buffer);
}

ConstructionResult::ConstructionResult(ConstructionResult&& other) noexcept
    : states(std::move(other.states)),
      packed_buffer(std::exchange(other.packed_buffer, nullptr)),
      num_total(std::exchange(other.num_total, 0)),
      num_connection(std::exchange(other.num_connection, 0)),
      instances(std::move(other.instances)),
      generate_scopes(std::move(other.generate_scopes)),
      instance_bundles(std::move(other.instance_bundles)),
      connection_descriptors(std::move(other.connection_descriptors)),
      body_desc_storage(std::move(other.body_desc_storage)),
      process_meta(std::move(other.process_meta)),
      trigger_meta(std::move(other.trigger_meta)),
      slot_meta(std::move(other.slot_meta)),
      trace_signal_meta(std::move(other.trace_signal_meta)),
      instance_ptrs(std::move(other.instance_ptrs)),
      installed_computations(std::move(other.installed_computations)) {
}

auto ConstructionResult::operator=(ConstructionResult&& other) noexcept
    -> ConstructionResult& {
  if (this != &other) {
    FreePackedBuffer(packed_buffer);
    states = std::move(other.states);
    packed_buffer = std::exchange(other.packed_buffer, nullptr);
    num_total = std::exchange(other.num_total, 0);
    num_connection = std::exchange(other.num_connection, 0);
    instances = std::move(other.instances);
    generate_scopes = std::move(other.generate_scopes);
    instance_bundles = std::move(other.instance_bundles);
    connection_descriptors = std::move(other.connection_descriptors);
    body_desc_storage = std::move(other.body_desc_storage);
    process_meta = std::move(other.process_meta);
    trigger_meta = std::move(other.trigger_meta);
    slot_meta = std::move(other.slot_meta);
    trace_signal_meta = std::move(other.trace_signal_meta);
    instance_ptrs = std::move(other.instance_ptrs);
    installed_computations = std::move(other.installed_computations);
  }
  return *this;
}

void RealizedTriggerMeta::Init() {
  words.clear();
  words.push_back(0);
  entry_count = 0;
}

void RealizedTriggerMeta::AppendEntry(
    uint32_t proc_idx, uint32_t slot_id, uint32_t edge, uint32_t flags) {
  if (words.empty()) {
    throw common::InternalError(
        "RealizedTriggerMeta::AppendEntry", "Init() not called");
  }
  words.push_back(proc_idx);
  words.push_back(slot_id);
  words.push_back(edge);
  words.push_back(flags);
  ++entry_count;
}

void RealizedTriggerMeta::Finalize() {
  if (!words.empty()) {
    words[0] = entry_count;
  }
}

void RealizedSlotMeta::Init() {
  words.clear();
  slot_count = 0;
}

void RealizedSlotMeta::AppendDesignGlobalSlot(
    uint32_t design_base_off, uint32_t total_bytes, uint32_t storage_kind,
    uint32_t value_off, uint32_t value_bytes, uint32_t unk_off,
    uint32_t unk_bytes, uint32_t storage_owner_slot_id) {
  words.push_back(0);
  words.push_back(design_base_off);
  words.push_back(0);
  words.push_back(0);
  words.push_back(total_bytes);
  words.push_back(storage_kind);
  words.push_back(value_off);
  words.push_back(value_bytes);
  words.push_back(unk_off);
  words.push_back(unk_bytes);
  words.push_back(storage_owner_slot_id);
  ++slot_count;
}

void RealizedSlotMeta::AppendInstanceOwnedSlot(
    uint32_t owner_instance_id, uint32_t instance_rel_off, uint32_t total_bytes,
    uint32_t storage_kind, uint32_t value_off, uint32_t value_bytes,
    uint32_t unk_off, uint32_t unk_bytes, uint32_t storage_owner_slot_id) {
  words.push_back(1);
  words.push_back(0);
  words.push_back(owner_instance_id);
  words.push_back(instance_rel_off);
  words.push_back(total_bytes);
  words.push_back(storage_kind);
  words.push_back(value_off);
  words.push_back(value_bytes);
  words.push_back(unk_off);
  words.push_back(unk_bytes);
  words.push_back(storage_owner_slot_id);
  ++slot_count;
}

void RealizedSlotMeta::Finalize() {
}

void RealizedTraceSignalMeta::Init() {
  words.clear();
  pool.clear();
  pool.push_back('\0');
  signal_count = 0;
}

auto RealizedTraceSignalMeta::AppendName(std::string_view name) -> uint32_t {
  if (name.empty()) return 0;
  auto off = static_cast<uint32_t>(pool.size());
  pool.insert(pool.end(), name.begin(), name.end());
  pool.push_back('\0');
  return off;
}

auto RealizedTraceSignalMeta::AppendHierarchicalName(
    std::string_view prefix, std::string_view local_name) -> uint32_t {
  auto off = static_cast<uint32_t>(pool.size());
  pool.insert(pool.end(), prefix.begin(), prefix.end());
  pool.push_back('.');
  pool.insert(pool.end(), local_name.begin(), local_name.end());
  pool.push_back('\0');
  return off;
}

void RealizedTraceSignalMeta::AppendSignal(
    uint32_t name_pool_off, uint32_t bit_width, uint32_t trace_kind,
    uint32_t storage_owner_slot_id) {
  words.push_back(name_pool_off);
  words.push_back(bit_width);
  words.push_back(trace_kind);
  words.push_back(storage_owner_slot_id);
  ++signal_count;
}

void RealizedTraceSignalMeta::Finalize() {
}

namespace {

// Validate a raw uint8_t shape byte is a valid WaitShapeKind.
void ValidateShapeByte(uint8_t raw, uint32_t index, const char* caller) {
  if (raw > static_cast<uint8_t>(WaitShapeKind::kDynamic)) {
    throw common::InternalError(
        caller,
        std::format(
            "proc_shapes[{}] = {} is not a valid WaitShapeKind", index, raw));
  }
}

// Validate a process metadata template view: pool sentinel, and that
// every entry's file_pool_off is within pool bounds and points to a
// NUL-terminated string before pool_size.
void ValidateMetaTemplate(
    const ProcessMetaTemplateView& tmpl, const char* caller) {
  if (tmpl.entries.empty() && tmpl.pool == nullptr && tmpl.pool_size == 0) {
    return;
  }
  if (tmpl.pool == nullptr || tmpl.pool_size == 0) {
    throw common::InternalError(caller, "meta pool null/size mismatch");
  }
  auto pool = std::span(tmpl.pool, tmpl.pool_size);
  if (pool[0] != '\0') {
    throw common::InternalError(caller, "meta pool missing '\\0' sentinel");
  }
  for (uint32_t i = 0; i < tmpl.entries.size(); ++i) {
    uint32_t off = tmpl.entries[i].file_pool_off;
    if (off == 0) continue;
    if (off >= tmpl.pool_size) {
      throw common::InternalError(
          caller, std::format(
                      "entry {} file_pool_off {} >= pool_size {}", i, off,
                      tmpl.pool_size));
    }
    bool found_nul = false;
    for (uint32_t j = off; j < tmpl.pool_size; ++j) {
      if (pool[j] == '\0') {
        found_nul = true;
        break;
      }
    }
    if (!found_nul) {
      throw common::InternalError(
          caller,
          std::format(
              "entry {} file string at offset {} not NUL-terminated", i, off));
    }
  }
}

static_assert(std::is_standard_layout_v<lyra::runtime::OwnedStorageHandle>);
static_assert(
    sizeof(lyra::runtime::OwnedStorageHandle) ==
    sizeof(void*) + sizeof(uint64_t));

void ApplyParamInitFromContiguous(
    RuntimeInstance& instance, std::span<const ParamInitSlotEntry> slots,
    const void* value_data, uint32_t value_total_bytes) {
  if (slots.empty()) return;
  auto src =
      std::span(static_cast<const uint8_t*>(value_data), value_total_bytes);
  uint32_t src_offset = 0;
  for (const auto& slot : slots) {
    if (src_offset + slot.byte_size > value_total_bytes) {
      throw lyra::common::InternalError(
          "ApplyParamInitFromContiguous",
          std::format(
              "param payload overrun: offset {} + size {} > total {}",
              src_offset, slot.byte_size, value_total_bytes));
    }
    auto* dst = ResolveInstanceStorageOffset(
        instance, slot.rel_byte_offset, slot.byte_size,
        "ApplyParamInitFromContiguous");
    std::memcpy(dst, &src[src_offset], slot.byte_size);
    src_offset += slot.byte_size;
  }
  if (src_offset != value_total_bytes) {
    throw lyra::common::InternalError(
        "ApplyParamInitFromContiguous",
        std::format(
            "param payload size mismatch: consumed {} bytes but payload has {}",
            src_offset, value_total_bytes));
  }
}

// Typed-argument companion to ApplyParamInitFromContiguous. Used by the
// direct-constructor path, which passes one independent pointer per
// transmitted parameter rather than a packed buffer. Each ptrs[i] must
// point to a value whose size matches slots[i].byte_size.
void ApplyParamInitFromArgs(
    RuntimeInstance& instance, std::span<const ParamInitSlotEntry> slots,
    std::span<const void* const> ptrs, std::span<const uint32_t> byte_sizes) {
  if (slots.empty() && ptrs.empty()) return;
  if (ptrs.size() != slots.size() || byte_sizes.size() != slots.size()) {
    throw lyra::common::InternalError(
        "ApplyParamInitFromArgs",
        std::format(
            "arg count ({}, {}) does not match body param slot count {}",
            ptrs.size(), byte_sizes.size(), slots.size()));
  }
  for (size_t i = 0; i < slots.size(); ++i) {
    const auto& slot = slots[i];
    if (byte_sizes[i] != slot.byte_size) {
      throw lyra::common::InternalError(
          "ApplyParamInitFromArgs",
          std::format(
              "arg {} byte size {} does not match slot byte size {}", i,
              byte_sizes[i], slot.byte_size));
    }
    auto* dst = ResolveInstanceStorageOffset(
        instance, slot.rel_byte_offset, slot.byte_size,
        "ApplyParamInitFromArgs");
    std::memcpy(dst, ptrs[i], slot.byte_size);
  }
}

}  // namespace

Constructor::Constructor(
    std::span<const ProcessStateSchema> schemas,
    std::span<const uint64_t> slot_byte_offsets, uint32_t num_package_slots,
    std::span<std::byte> design_state, ProcessMetaTemplateView conn_meta,
    TriggerTemplateView conn_triggers,
    ObservableDescriptorTemplateView pkg_observable,
    StorageConstructionRecipeView pkg_init_recipe,
    StorageConstructionRootView pkg_init_recipe_roots)
    : schemas_(schemas),
      slot_byte_offsets_(slot_byte_offsets),
      design_state_(design_state),
      next_slot_base_(num_package_slots),
      conn_meta_(conn_meta),
      conn_triggers_(conn_triggers),
      pkg_observable_(pkg_observable),
      pkg_init_recipe_(pkg_init_recipe),
      pkg_init_recipe_roots_(pkg_init_recipe_roots) {
  ValidateMetaTemplate(conn_meta_, "Constructor");
  realized_meta_.pool.push_back('\0');
  realized_triggers_.Init();
  realized_slot_meta_.Init();
  realized_trace_meta_.Init();

  // Package/global observable descriptor prelude.
  // Realized before any body-instance expansion. All entries are absolute.
  for (const auto& entry : pkg_observable_.entries) {
    realized_slot_meta_.AppendDesignGlobalSlot(
        entry.storage_byte_offset, entry.total_bytes, entry.storage_kind,
        entry.value_lane_offset, entry.value_lane_bytes, entry.unk_lane_offset,
        entry.unk_lane_bytes, entry.storage_owner_ref);

    std::string_view name;
    if (entry.local_name_pool_off > 0 &&
        entry.local_name_pool_off < pkg_observable_.pool_size) {
      auto pool = std::span(pkg_observable_.pool, pkg_observable_.pool_size);
      name = &pool[entry.local_name_pool_off];
    }
    uint32_t name_off = realized_trace_meta_.AppendName(name);
    realized_trace_meta_.AppendSignal(
        name_off, entry.bit_width, entry.trace_kind, entry.storage_owner_ref);
  }

  // Package/global storage construction.
  ApplyStorageConstructionRecipeToArena(
      design_state_, pkg_init_recipe_, pkg_init_recipe_roots_);

  // Validate connection trigger template if non-empty.
  if (!conn_triggers_.proc_ranges.empty()) {
    if (conn_triggers_.proc_shapes.size() !=
        conn_triggers_.proc_ranges.size()) {
      throw common::InternalError(
          "Constructor",
          std::format(
              "conn_triggers proc_shapes size {} != proc_ranges size {}",
              conn_triggers_.proc_shapes.size(),
              conn_triggers_.proc_ranges.size()));
    }
    if (conn_triggers_.proc_groupable.size() !=
        conn_triggers_.proc_ranges.size()) {
      throw common::InternalError(
          "Constructor",
          std::format(
              "conn_triggers proc_groupable size {} != proc_ranges size {}",
              conn_triggers_.proc_groupable.size(),
              conn_triggers_.proc_ranges.size()));
    }
    for (uint32_t i = 0; i < conn_triggers_.proc_ranges.size(); ++i) {
      const auto& r = conn_triggers_.proc_ranges[i];
      if (r.start > conn_triggers_.entries.size() ||
          r.count > conn_triggers_.entries.size() - r.start) {
        throw common::InternalError(
            "Constructor",
            std::format(
                "conn_triggers proc_ranges[{}] ({},{}) exceeds entries "
                "size {}",
                i, r.start, r.count, conn_triggers_.entries.size()));
      }
      if (conn_triggers_.proc_groupable[i] > kProcGroupable) {
        throw common::InternalError(
            "Constructor", std::format(
                               "conn_triggers proc_groupable[{}] = {} "
                               "(expected kProcNotGroupable or kProcGroupable)",
                               i, conn_triggers_.proc_groupable[i]));
      }
      ValidateShapeByte(
          conn_triggers_.proc_shapes[i], i, "Constructor conn_triggers");
    }
  }
}

void Constructor::CheckNotFinalized(const char* caller) const {
  if (finalized_) {
    throw common::InternalError(caller, "constructor already finalized");
  }
}

void Constructor::ClearActiveBody() {
  body_ = ActiveBodyDescriptor{};
  current_body_package_ = nullptr;
}

void Constructor::AddConnection(const ConnectionRealizationDesc& desc) {
  CheckNotFinalized("Constructor::AddConnection");
  if (connections_finalized_) {
    throw common::InternalError(
        "Constructor::AddConnection",
        "connections must be added before any body instances");
  }
  if (desc.schema_index >= schemas_.size()) {
    throw common::InternalError(
        "Constructor::AddConnection", "schema_index out of range");
  }
  if (conn_meta_index_ >= conn_meta_.entries.size()) {
    throw common::InternalError(
        "Constructor::AddConnection",
        std::format(
            "connection metadata template exhausted (index {} >= count {})",
            conn_meta_index_, conn_meta_.entries.size()));
  }

  staged_.push_back(
      StagedProcess{
          .schema_index = desc.schema_index,
          .body = nullptr,
          .instance_index = UINT32_MAX,
          .is_module = false,
      });

  const auto& meta = conn_meta_.entries[conn_meta_index_];
  auto conn_pool = std::span(conn_meta_.pool, conn_meta_.pool_size);
  uint32_t file_off = InternString(conn_pool, meta.file_pool_off);
  realized_meta_.words.push_back(0);
  realized_meta_.words.push_back(meta.kind_packed);
  realized_meta_.words.push_back(file_off);
  realized_meta_.words.push_back(meta.line);
  realized_meta_.words.push_back(meta.col);

  // Trigger realization for this connection process.
  // Connection trigger ordinal = num_connection_ (before increment).
  if (!conn_triggers_.proc_ranges.empty()) {
    uint32_t ordinal = num_connection_;
    if (ordinal >= conn_triggers_.proc_ranges.size()) {
      throw common::InternalError(
          "Constructor::AddConnection",
          std::format(
              "connection trigger template exhausted "
              "(ordinal {} >= count {})",
              ordinal, conn_triggers_.proc_ranges.size()));
    }
    auto proc_idx = static_cast<uint32_t>(staged_.size() - 1);
    const auto& range = conn_triggers_.proc_ranges[ordinal];
    uint32_t flags = (conn_triggers_.proc_groupable[ordinal] != 0)
                         ? process_trigger_abi::kFlagGroupable
                         : 0;
    for (uint32_t t = 0; t < range.count; ++t) {
      const auto& te = conn_triggers_.entries[range.start + t];
      realized_triggers_.AppendEntry(proc_idx, te.slot_id, te.edge, flags);
      trigger_provenance_.push_back(
          TriggerProvenanceRecord{
              .domain = TemplateDomain::kConnection,
              .owner_ordinal = ordinal,
              .local_ordinal = ordinal,
              .realized_proc_idx = proc_idx,
          });
    }
  }

  ++conn_meta_index_;
  ++num_connection_;
}

void Constructor::BeginBody(const BodyDescriptorPackage& package) {
  CheckNotFinalized("Constructor::BeginBody");
  connections_finalized_ = true;
  ClearActiveBody();

  if (package.desc == nullptr) {
    throw common::InternalError("Constructor::BeginBody", "null descriptor");
  }
  if (package.entries.size() != package.desc->num_processes) {
    throw common::InternalError(
        "Constructor::BeginBody",
        std::format(
            "entry count {} != descriptor num_processes {}",
            package.entries.size(), package.desc->num_processes));
  }
  if (package.entries.size() != package.meta.entries.size()) {
    throw common::InternalError(
        "Constructor::BeginBody",
        std::format(
            "process entry count {} != meta entry count {}",
            package.entries.size(), package.meta.entries.size()));
  }
  ValidateMetaTemplate(package.meta, "Constructor::BeginBody");

  // Validate trigger template view.
  // All-empty spans are the intended no-op bring-up state: all four
  // spans (entries, proc_ranges, proc_shapes, proc_groupable) must be
  // empty together. Partial population is rejected explicitly.
  const auto& trig = package.triggers;
  bool trig_has_ranges = !trig.proc_ranges.empty();
  bool trig_has_entries = !trig.entries.empty();
  bool trig_has_shapes = !trig.proc_shapes.empty();
  bool trig_has_groupable = !trig.proc_groupable.empty();
  if (!trig_has_ranges &&
      (trig_has_entries || trig_has_shapes || trig_has_groupable)) {
    throw common::InternalError(
        "Constructor::BeginBody",
        "trigger view partially populated: proc_ranges empty but other "
        "spans non-empty");
  }
  if (trig_has_ranges && (!trig_has_shapes || !trig_has_groupable)) {
    throw common::InternalError(
        "Constructor::BeginBody",
        "trigger view partially populated: proc_ranges non-empty but "
        "proc_shapes or proc_groupable empty");
  }
  if (trig_has_ranges) {
    if (trig.proc_ranges.size() != package.desc->num_processes) {
      throw common::InternalError(
          "Constructor::BeginBody",
          std::format(
              "trigger proc_ranges size {} != num_processes {}",
              trig.proc_ranges.size(), package.desc->num_processes));
    }
    if (trig.proc_shapes.size() != trig.proc_ranges.size()) {
      throw common::InternalError(
          "Constructor::BeginBody",
          std::format(
              "trigger proc_shapes size {} != proc_ranges size {}",
              trig.proc_shapes.size(), trig.proc_ranges.size()));
    }
    if (trig.proc_groupable.size() != trig.proc_ranges.size()) {
      throw common::InternalError(
          "Constructor::BeginBody",
          std::format(
              "trigger proc_groupable size {} != proc_ranges size {}",
              trig.proc_groupable.size(), trig.proc_ranges.size()));
    }
    for (uint32_t i = 0; i < trig.proc_ranges.size(); ++i) {
      const auto& r = trig.proc_ranges[i];
      if (r.start > trig.entries.size() ||
          r.count > trig.entries.size() - r.start) {
        throw common::InternalError(
            "Constructor::BeginBody",
            std::format(
                "trigger proc_ranges[{}] ({},{}) exceeds entries size {}", i,
                r.start, r.count, trig.entries.size()));
      }
    }
    for (uint32_t i = 0; i < trig.proc_groupable.size(); ++i) {
      if (trig.proc_groupable[i] > kProcGroupable) {
        throw common::InternalError(
            "Constructor::BeginBody",
            std::format(
                "trigger proc_groupable[{}] = {} "
                "(expected kProcNotGroupable or kProcGroupable)",
                i, trig.proc_groupable[i]));
      }
      ValidateShapeByte(
          trig.proc_shapes[i], i, "Constructor::BeginBody triggers");
    }
  }

  // Validate comb template view.
  // All-empty spans are the intended no-op bring-up state: both entries
  // and kernels must be empty together. Partial population is rejected.
  // Zero-trigger kernels (trigger_count == 0) are not a valid template
  // shape; every kernel must reference at least one trigger entry.
  const auto& comb = package.comb;
  bool comb_has_kernels = !comb.kernels.empty();
  bool comb_has_entries = !comb.entries.empty();
  if (comb_has_entries && !comb_has_kernels) {
    throw common::InternalError(
        "Constructor::BeginBody", "comb entries present without kernels");
  }
  if (comb_has_kernels && !comb_has_entries) {
    throw common::InternalError(
        "Constructor::BeginBody", "comb kernels present without entries");
  }
  if (comb_has_kernels) {
    for (uint32_t i = 0; i < comb.kernels.size(); ++i) {
      const auto& k = comb.kernels[i];
      if (k.proc_within_body >= package.desc->num_processes) {
        throw common::InternalError(
            "Constructor::BeginBody",
            std::format(
                "comb kernel[{}] proc_within_body {} >= num_processes {}", i,
                k.proc_within_body, package.desc->num_processes));
      }
      if (k.trigger_start > comb.entries.size() ||
          k.trigger_count > comb.entries.size() - k.trigger_start) {
        throw common::InternalError(
            "Constructor::BeginBody",
            std::format(
                "comb kernel[{}] ({},{}) exceeds entries size {}", i,
                k.trigger_start, k.trigger_count, comb.entries.size()));
      }
      if (k.trigger_count == 0) {
        throw common::InternalError(
            "Constructor::BeginBody",
            std::format("comb kernel[{}] has zero triggers", i));
      }
    }
  }

  for (const auto& entry : package.entries) {
    if (entry.schema_index >= schemas_.size()) {
      throw common::InternalError(
          "Constructor::BeginBody", "schema_index out of range");
    }
  }

  body_ = ActiveBodyDescriptor{
      .slot_count = package.desc->slot_count,
      .inline_state_size_bytes = package.desc->inline_state_size_bytes,
      .appendix_state_size_bytes = package.desc->appendix_state_size_bytes,
      .total_state_size_bytes = package.desc->total_state_size_bytes,
      .entries = package.entries,
      .meta = package.meta,
      .triggers = package.triggers,
      .comb = package.comb,
      .observable_descriptors = package.observable_descriptors,
      .init_recipe = package.init_recipe,
      .init_recipe_roots = package.init_recipe_roots,
      .init_params = package.init_params,
      .active = true,
  };

  // Canonicalize: reuse existing stable template if this body was seen
  // before. Body identity is keyed by BodyRealizationDesc pointer
  // (compile-time constant, unique per body).
  const void* key = package.desc;
  for (const auto& stored : body_desc_storage_) {
    if (stored.key == key) {
      current_body_package_ = &stored.package;
      return;
    }
  }
  body_desc_storage_.push_back(
      StableBodyTemplate{
          .key = key,
          .package = package,
      });
  current_body_package_ = &body_desc_storage_.back().package;
}

auto Constructor::CreateScope(
    RuntimeScope* parent_scope, const char* label, uint32_t ordinal_in_parent)
    -> RuntimeScope* {
  CheckNotFinalized("Constructor::CreateScope");
  auto gen = std::make_unique<RuntimeGenerateScope>();
  gen->scope.kind = RuntimeScopeKind::kGenerate;
  gen->scope.parent = parent_scope;
  gen->scope.ordinal_in_parent = ordinal_in_parent;
  gen->scope.path_storage = BuildScopePath(parent_scope, label);
  gen->scope.path_c_str = gen->scope.path_storage.c_str();

  if (parent_scope != nullptr) {
    parent_scope->children.push_back(
        RuntimeChildEdge{
            .ordinal_in_parent = ordinal_in_parent,
            .child = &gen->scope,
        });
  }

  auto* result = &gen->scope;
  staged_generate_scopes_.push_back(std::move(gen));
  return result;
}

auto Constructor::CreateChildInstance(
    RuntimeScope* parent_scope, uint32_t ordinal_in_parent,
    uint32_t instance_index, const char* label, const void* param_data,
    uint32_t param_data_size, std::span<const void* const> arg_ptrs,
    std::span<const uint32_t> arg_byte_sizes, uint64_t realized_inline_size,
    uint64_t realized_appendix_size) -> RuntimeInstance* {
  CheckNotFinalized("Constructor::CreateChildInstance");
  if (label == nullptr) {
    throw common::InternalError(
        "Constructor::CreateChildInstance", "null label");
  }
  if (!body_.active || current_body_package_ == nullptr) {
    throw common::InternalError(
        "Constructor::CreateChildInstance",
        "no active body package (call BeginBody first)");
  }
  if ((param_data == nullptr) != (param_data_size == 0)) {
    throw common::InternalError(
        "Constructor::CreateChildInstance",
        "param_data and param_data_size must be both empty or both present");
  }
  bool has_blob = param_data_size != 0;
  bool has_typed_args = !arg_ptrs.empty() || !arg_byte_sizes.empty();
  if (has_blob && has_typed_args) {
    throw common::InternalError(
        "Constructor::CreateChildInstance",
        "blob param_data and typed arg_ptrs are mutually exclusive");
  }
  connections_finalized_ = true;

  auto instance = std::make_unique<RuntimeInstance>();
  instance->scope.kind = RuntimeScopeKind::kInstance;
  instance->owner_ordinal = instance_index;
  instance->scope.ordinal_in_parent = ordinal_in_parent;
  instance->scope.parent = parent_scope;
  instance->scope.path_storage = BuildScopePath(parent_scope, label);
  instance->scope.path_c_str = instance->scope.path_storage.c_str();

  if (parent_scope != nullptr) {
    parent_scope->children.push_back(
        RuntimeChildEdge{
            .ordinal_in_parent = ordinal_in_parent,
            .child = &instance->scope,
        });
  }

  instance->storage.inline_base =
      AllocateOwnedInlineStorage(realized_inline_size);
  instance->storage.inline_size = realized_inline_size;
  instance->storage.appendix_base =
      AllocateOwnedAppendixStorage(realized_appendix_size);
  instance->storage.appendix_size = realized_appendix_size;
  instance->storage.deferred_inline_base =
      AllocateOwnedInlineStorage(realized_inline_size);
  instance->storage.deferred_appendix_base =
      AllocateOwnedAppendixStorage(realized_appendix_size);

  // Invariant: zero-slot bodies must not carry instance-state init.
  if (body_.slot_count == 0) {
    if (body_.init_recipe.num_ops != 0 ||
        body_.init_recipe_roots.num_roots != 0 ||
        body_.init_recipe.num_child_indices != 0 ||
        !body_.init_params.slots.empty() || has_blob || has_typed_args) {
      throw common::InternalError(
          "Constructor::CreateChildInstance",
          "zero-slot body must not carry instance-state init descriptors "
          "or param data");
    }
  }

  // Body-shaped storage construction.
  ApplyStorageConstructionRecipeToInstance(
      *instance, body_.init_recipe, body_.init_recipe_roots);
  if (has_typed_args) {
    ApplyParamInitFromArgs(
        *instance, body_.init_params.slots, arg_ptrs, arg_byte_sizes);
  } else {
    ApplyParamInitFromContiguous(
        *instance, body_.init_params.slots, param_data, param_data_size);
  }

  // Place instance at its compile-time object_index position so
  // result.instances[object_index] matches the compile-time model.
  if (instance_index >= staged_instances_.size()) {
    staged_instances_.resize(instance_index + 1);
  }
  staged_instances_[instance_index] = std::move(instance);

  // Stage module processes (no expression connection suffix - those are
  // now installable computations, not processes).
  auto num_entries = static_cast<uint32_t>(body_.entries.size());
  for (uint32_t ei = 0; ei < num_entries; ++ei) {
    const auto& entry = body_.entries[ei];
    SharedBodyFn body_fn{};
    std::memcpy(&body_fn, &entry.shared_body_fn, sizeof(body_fn));

    if (body_fn == nullptr) {
      throw common::InternalError(
          "Constructor::CreateChildInstance",
          std::format(
              "body entry {} has null shared_body_fn "
              "(emitted function pointer is zero)",
              ei));
    }

    staged_.push_back(
        StagedProcess{
            .schema_index = entry.schema_index,
            .body = body_fn,
            .instance_index = instance_index,
            .is_module = true,
        });
  }

  ++next_module_instance_ordinal_;

  // Use the instance's stable path for the design-global observable walk.
  const char* instance_path =
      staged_instances_[instance_index]->scope.path_c_str;

  // R4: Module-instance process meta, trigger, comb, instance-owned slot,
  // and instance-owned trace metadata are all engine-derived from bundles.
  // Constructor builds only design-global/package observable metadata
  // (slot meta + trace signals). Instance-owned entries are skipped here
  // and derived from the shared observable walk in
  // InitModuleInstancesFromBundles.
  if (!body_.observable_descriptors.entries.empty()) {
    bool has_storage = realized_inline_size > 0;
    if (!has_storage) {
      for (const auto& entry : body_.observable_descriptors.entries) {
        if ((entry.flags & kObservableFlagStorageAbsolute) == 0) {
          throw common::InternalError(
              "Constructor::CreateChildInstance",
              "body-relative observable entry in instance with no "
              "local storage");
        }
      }
    }

    // R4 closure: only design-global observable entries are constructor-built.
    // Instance-owned entries (storage_domain == 1) are engine-derived from
    // bundles in InitModuleInstancesFromBundles.
    for (const auto& entry : body_.observable_descriptors.entries) {
      if (entry.storage_domain != 0) continue;

      uint32_t realized_owner =
          (entry.flags & kObservableFlagOwnerAbsolute) != 0
              ? entry.storage_owner_ref
              : entry.storage_owner_ref + next_slot_base_;

      realized_slot_meta_.AppendDesignGlobalSlot(
          entry.storage_byte_offset, entry.total_bytes, entry.storage_kind,
          entry.value_lane_offset, entry.value_lane_bytes,
          entry.unk_lane_offset, entry.unk_lane_bytes, realized_owner);

      std::string_view local_name;
      if (entry.local_name_pool_off > 0 &&
          entry.local_name_pool_off < body_.observable_descriptors.pool_size) {
        auto obs_pool = std::span(
            body_.observable_descriptors.pool,
            body_.observable_descriptors.pool_size);
        local_name = &obs_pool[entry.local_name_pool_off];
      }
      uint32_t name_off = realized_trace_meta_.AppendHierarchicalName(
          instance_path, local_name);
      realized_trace_meta_.AppendSignal(
          name_off, entry.bit_width, entry.trace_kind, realized_owner);
    }
  }

  // R4 prep: record per-instance metadata bundle at the same position as
  // the instance (instance_index). Must be parallel to staged_instances_.
  if (instance_index >= staged_bundles_.size()) {
    staged_bundles_.resize(instance_index + 1);
  }
  staged_bundles_[instance_index] = InstanceMetadataBundle{
      .instance = staged_instances_[instance_index].get(),
      .body_desc = nullptr,
      .body_key = current_body_package_->desc,
      .instance_path = instance_path,
  };

  uint32_t new_slot_base = next_slot_base_ + body_.slot_count;
  if (new_slot_base < next_slot_base_) {
    throw common::InternalError(
        "Constructor::CreateChildInstance", "slot base overflow");
  }
  if (body_.slot_count > 0 && new_slot_base > slot_byte_offsets_.size()) {
    throw common::InternalError(
        "Constructor::CreateChildInstance",
        std::format(
            "post-increment slot base {} exceeds layout oracle size {}",
            new_slot_base, slot_byte_offsets_.size()));
  }
  next_instance_id_ += 1;
  next_slot_base_ = new_slot_base;

  return staged_instances_[instance_index].get();
}

auto Constructor::CreateChildInstanceDirect(
    RuntimeScope* parent_scope, uint32_t ordinal_in_parent,
    uint32_t instance_index, const char* label, uint64_t realized_inline_size,
    uint64_t realized_appendix_size, uint32_t num_args,
    const void* const* arg_ptrs, const uint32_t* arg_byte_sizes)
    -> RuntimeInstance* {
  // Direct-constructor entry point. Forwards typed transmitted-parameter
  // arguments into CreateChildInstance, which dispatches to
  // ApplyParamInitFromArgs when the typed spans are non-empty. No
  // compile-time byte pool is consulted.
  auto ptrs = std::span<const void* const>(arg_ptrs, num_args);
  auto sizes = std::span<const uint32_t>(arg_byte_sizes, num_args);
  return CreateChildInstance(
      parent_scope, ordinal_in_parent, instance_index, label,
      /*param_data=*/nullptr, /*param_data_size=*/0, ptrs, sizes,
      realized_inline_size, realized_appendix_size);
}

auto Constructor::GetStagedInstance(uint32_t instance_index) const
    -> RuntimeInstance* {
  if (instance_index >= staged_instances_.size() ||
      staged_instances_[instance_index] == nullptr) {
    throw common::InternalError(
        "Constructor::GetStagedInstance",
        std::format(
            "no staged instance at index {} (staged size {})", instance_index,
            staged_instances_.size()));
  }
  return staged_instances_[instance_index].get();
}

auto Constructor::InternString(std::span<const char> pool, uint32_t pool_off)
    -> uint32_t {
  if (pool.empty() || pool_off == 0) return 0;
  std::string_view sv(pool.subspan(pool_off).data());
  if (sv.empty()) return 0;
  auto it = string_intern_.find(sv);
  if (it != string_intern_.end()) return it->second;
  auto off = AppendString(sv);
  interned_strings_.emplace_back(sv);
  string_intern_[std::string_view(interned_strings_.back())] = off;
  return off;
}

auto Constructor::AppendString(std::string_view s) -> uint32_t {
  if (s.empty()) return 0;
  auto off = static_cast<uint32_t>(realized_meta_.pool.size());
  realized_meta_.pool.insert(realized_meta_.pool.end(), s.begin(), s.end());
  realized_meta_.pool.push_back('\0');
  return off;
}

namespace {}  // namespace

auto Constructor::Finalize() -> ConstructionResult {
  CheckNotFinalized("Constructor::Finalize");
  finalized_ = true;

  if (conn_meta_index_ != conn_meta_.entries.size()) {
    throw common::InternalError(
        "Constructor::Finalize",
        std::format(
            "connection metadata not fully consumed ({} of {})",
            conn_meta_index_, conn_meta_.entries.size()));
  }

  auto num_total = static_cast<uint32_t>(staged_.size());

  // R4: validate connection-only process meta count.
  // Module-instance process meta is engine-derived from bundles.
  auto expected_conn_meta_words =
      static_cast<size_t>(num_connection_) * process_meta_abi::kStride;
  if (realized_meta_.words.size() != expected_conn_meta_words) {
    throw common::InternalError(
        "Constructor::Finalize",
        std::format(
            "connection metadata word count {} != expected {} "
            "(num_connection={} * stride={})",
            realized_meta_.words.size(), expected_conn_meta_words,
            num_connection_, process_meta_abi::kStride));
  }

  // Finalize connection-only trigger, slot/trace realized builders.
  // R4: comb is fully engine-derived from bundles (no constructor output).
  realized_triggers_.Finalize();
  realized_slot_meta_.Finalize();
  realized_trace_meta_.Finalize();

  // Verify connection trigger template consumption.
  if (!conn_triggers_.proc_ranges.empty() &&
      num_connection_ != conn_triggers_.proc_ranges.size()) {
    throw common::InternalError(
        "Constructor::Finalize",
        std::format(
            "connection triggers not fully consumed ({} of {})",
            num_connection_, conn_triggers_.proc_ranges.size()));
  }

  // Invariant wall for sparse-indexed staging.
  // CreateChildInstance resizes staged_instances_ / staged_bundles_ to
  // the maximum instance_index and places each instance at its index
  // slot. If any index was skipped, we would have a null hole that
  // fails silently at dereference time. Validate density and
  // cross-array consistency before any downstream use.
  if (staged_bundles_.size() != staged_instances_.size()) {
    throw common::InternalError(
        "Constructor::Finalize",
        std::format(
            "staged_bundles_ size {} != staged_instances_ size {}",
            staged_bundles_.size(), staged_instances_.size()));
  }
  for (uint32_t li = 0; li < staged_instances_.size(); ++li) {
    if (staged_instances_[li] == nullptr) {
      throw common::InternalError(
          "Constructor::Finalize",
          std::format(
              "staged_instances_[{}] is null -- instance_index was "
              "skipped during CreateChildInstance staging",
              li));
    }
    if (staged_instances_[li]->owner_ordinal != li) {
      throw common::InternalError(
          "Constructor::Finalize",
          std::format(
              "instance ordinal mismatch: staged_instances_[{}] has "
              "owner_ordinal {}",
              li, staged_instances_[li]->owner_ordinal));
    }
    if (staged_bundles_[li].instance != staged_instances_[li].get()) {
      throw common::InternalError(
          "Constructor::Finalize",
          std::format(
              "staged_bundles_[{}].instance != staged_instances_[{}]", li, li));
    }
  }

  // R4: Only connection triggers remain in the flat provenance.
  // Module triggers are derived from bundles by the engine.
  for (const auto& rec : trigger_provenance_) {
    if (rec.realized_proc_idx != rec.owner_ordinal) {
      throw common::InternalError(
          "Constructor::Finalize",
          std::format(
              "connection trigger proc_idx mismatch: "
              "realized {} != owner_ordinal {}",
              rec.realized_proc_idx, rec.owner_ordinal));
    }
  }

  if (num_total == 0) {
    ConstructionResult result;
    result.process_meta = std::move(realized_meta_);
    result.trigger_meta = std::move(realized_triggers_);
    result.slot_meta = std::move(realized_slot_meta_);
    result.trace_signal_meta = std::move(realized_trace_meta_);
    return result;
  }

  // Sort module processes by instance_index to match the engine's bundle
  // ordering. Connection processes (indices 0..num_connection_-1) stay in
  // place; module processes (num_connection_..end) are sorted by the
  // instance_index they were staged with. This decouples the hierarchy
  // tree order (structural/declaration) from the payload/bundle order
  // (sorted instance_index).
  std::stable_sort(
      staged_.begin() + num_connection_, staged_.end(),
      [](const StagedProcess& a, const StagedProcess& b) {
        return a.instance_index < b.instance_index;
      });

  std::vector<uint32_t> schema_indices;
  schema_indices.reserve(num_total);
  for (const auto& proc : staged_) {
    schema_indices.push_back(proc.schema_index);
  }

  std::vector<void*> states(num_total);
  void* packed_buffer = AllocateProcessFrames(
      std::span(states), std::span(schema_indices), schemas_);

  ConstructionResult result;
  result.states = std::move(states);
  result.packed_buffer = packed_buffer;
  result.num_total = num_total;
  result.num_connection = num_connection_;
  result.instances = std::move(staged_instances_);
  result.generate_scopes = std::move(staged_generate_scopes_);
  result.process_meta = std::move(realized_meta_);
  result.trigger_meta = std::move(realized_triggers_);
  result.slot_meta = std::move(realized_slot_meta_);
  result.trace_signal_meta = std::move(realized_trace_meta_);
  // Paths are already owned by instances (set in CreateChild).
  // path_c_str may have been invalidated by std::string moves
  // during staged_instances_ -> result.instances transfer, but
  // unique_ptr move doesn't move the RuntimeInstance itself, so
  // path_storage and path_c_str remain valid.

  // Build stable raw pointer view for the runtime ABI.
  result.instance_ptrs.reserve(result.instances.size());
  for (const auto& inst : result.instances) {
    result.instance_ptrs.push_back(inst.get());
  }

  // R4: Move body descriptor storage into result (final owner).
  result.body_desc_storage = std::move(body_desc_storage_);

  // Resolve body_desc pointers against final storage.
  auto resolve_body_desc =
      [&](const void* key) -> const BodyDescriptorPackage* {
    for (const auto& stored : result.body_desc_storage) {
      if (stored.key == key) return &stored.package;
    }
    return nullptr;
  };

  // R4: validate and move per-instance metadata bundles.
  result.instance_bundles = std::move(staged_bundles_);

  if (result.instance_bundles.size() != result.instances.size() ||
      result.instance_bundles.size() != result.instances.size()) {
    throw common::InternalError(
        "Constructor::Finalize", "R4 bundle/path/instance count mismatch");
  }

  for (uint32_t i = 0; i < result.instance_bundles.size(); ++i) {
    auto& bundle = result.instance_bundles[i];
    const RuntimeInstance* expected_instance = result.instances[i].get();

    if (bundle.instance == nullptr) {
      throw common::InternalError(
          "Constructor::Finalize",
          std::format("R4 bundle {} has null instance", i));
    }

    // Resolve body_desc from body_key against final owner storage.
    bundle.body_desc = resolve_body_desc(bundle.body_key);
    if (bundle.body_desc == nullptr) {
      throw common::InternalError(
          "Constructor::Finalize",
          std::format("R4 bundle {} has unresolved body template", i));
    }
    if (bundle.instance != expected_instance) {
      throw common::InternalError(
          "Constructor::Finalize",
          std::format("R4 bundle {} instance pointer mismatch", i));
    }

    // Re-validate bundle instance_path against the instance's stable
    // path pointer (set in CreateChild, still valid after unique_ptr move).
    result.instance_bundles[i].instance_path =
        result.instances[i]->scope.path_c_str;
  }

  // Installable computation resolution.
  //
  // Each IC is an ordinary reactive writeback body. The body addresses
  // its child target from inside the callable through the owner's
  // ext_ref_bindings (populated by the construction program via
  // LyraConstructionResultSetExtRefBindings). Finalize therefore only
  // threads the callable, owner, and dependency set through to the
  // engine -- no target address, byte size, or child correlation is
  // carried on the IC descriptor.
  for (size_t bi = 0; bi < result.instance_bundles.size(); ++bi) {
    const auto& bundle = result.instance_bundles[bi];
    if (bundle.body_desc == nullptr) continue;
    const auto* body_desc = bundle.body_desc->desc;
    uint32_t num_ic = body_desc->num_installable_computations;
    if (num_ic == 0) continue;
    if (body_desc->installable_computations == nullptr) {
      throw common::InternalError(
          "Constructor::Finalize",
          "num_installable_computations > 0 but pointer is null");
    }

    auto* owner = result.instances[bi].get();
    std::span<const uint32_t> word_pool(
        body_desc->ic_word_pool, body_desc->num_ic_word_pool_entries);
    std::span<const InstallableComputationDesc> ic_descs(
        body_desc->installable_computations, num_ic);

    for (uint32_t ci = 0; ci < num_ic; ++ci) {
      const auto& desc = ic_descs[ci];

      // Collect body-local dependency slots. Consumed by the engine
      // through each owner's local_reactive_trigger_map, which is
      // indexed by body-local slot id.
      std::vector<uint32_t> dep_body_local_slots;
      if (desc.dep_count > 0) {
        uint32_t dep_end = desc.dep_pool_offset + desc.dep_count;
        if (dep_end > word_pool.size()) {
          throw common::InternalError(
              "Constructor::Finalize",
              std::format(
                  "ic dep range [{}, {}) exceeds pool size {}",
                  desc.dep_pool_offset, dep_end, word_pool.size()));
        }
        dep_body_local_slots.reserve(desc.dep_count);
        for (uint32_t di = 0; di < desc.dep_count; ++di) {
          dep_body_local_slots.push_back(word_pool[desc.dep_pool_offset + di]);
        }
      }

      result.installed_computations.push_back(
          ConstructionResult::ResolvedInstalledComputation{
              .eval_fn = desc.eval_fn,
              .owner_instance = owner,
              .dep_body_local_slots = std::move(dep_body_local_slots),
          });
    }
  }

  return result;
}

}  // namespace lyra::runtime

// C ABI wrappers for emitted LLVM IR constructor function.

namespace {

void ValidateHandle(const void* handle, const char* caller) {
  if (handle == nullptr) {
    throw lyra::common::InternalError(caller, "null handle");
  }
}

void ValidateAbiArray(
    const void* ptr, uint32_t count, const char* what, const char* caller) {
  if (count == 0) return;
  if (ptr == nullptr) {
    throw lyra::common::InternalError(
        caller, std::format("{} pointer is null with non-zero count", what));
  }
}

// Validate meta template ABI inputs at the C boundary: pointer/count
// coherence, pool sentinel, and all entry file_pool_off offsets in range
// with NUL termination. This catches malformed emitted templates before
// span construction or downstream constructor logic.
void ValidateMetaAbiInputs(
    const lyra::runtime::ProcessMetaTemplateEntry* entries, uint32_t count,
    const char* pool, uint32_t pool_size, const char* caller) {
  if (count > 0 && entries == nullptr) {
    throw lyra::common::InternalError(
        caller, "non-zero meta entry count with null entries pointer");
  }
  if (pool_size > 0 && pool == nullptr) {
    throw lyra::common::InternalError(
        caller, "non-zero meta pool size with null pool pointer");
  }
  if (count > 0 && pool_size == 0) {
    throw lyra::common::InternalError(
        caller, "non-empty meta entries with zero pool size");
  }
  if (pool != nullptr && pool_size > 0) {
    auto pool_span = std::span(pool, pool_size);
    if (pool_span[0] != '\0') {
      throw lyra::common::InternalError(
          caller, "meta pool missing '\\0' sentinel at offset 0");
    }
    auto entries_span = std::span(entries, count);
    for (uint32_t i = 0; i < count; ++i) {
      uint32_t off = entries_span[i].file_pool_off;
      if (off == 0) continue;
      if (off >= pool_size) {
        throw lyra::common::InternalError(
            caller, std::format(
                        "entry {} file_pool_off {} >= pool_size {}", i, off,
                        pool_size));
      }
      bool found_nul = false;
      for (uint32_t j = off; j < pool_size; ++j) {
        if (pool_span[j] == '\0') {
          found_nul = true;
          break;
        }
      }
      if (!found_nul) {
        throw lyra::common::InternalError(
            caller, std::format(
                        "entry {} file string at offset {} not NUL-terminated",
                        i, off));
      }
    }
  }
}

void ValidateObservableDescriptorTemplate(
    std::span<const lyra::runtime::ObservableDescriptorEntry> entries,
    const char* pool, uint32_t pool_size, bool require_package_global,
    const char* caller) {
  if (entries.empty()) return;
  if (pool == nullptr) {
    throw lyra::common::InternalError(
        caller,
        "non-empty observable descriptor entries with null pool pointer");
  }
  if (pool_size == 0) {
    throw lyra::common::InternalError(
        caller, "observable descriptor pool must contain the empty sentinel");
  }
  auto pool_span = std::span(pool, pool_size);
  if (pool_span[0] != '\0') {
    throw lyra::common::InternalError(
        caller, "observable descriptor pool[0] must be the empty sentinel");
  }

  for (size_t i = 0; i < entries.size(); ++i) {
    const auto& e = entries[i];
    if (e.local_name_pool_off >= pool_size) {
      throw lyra::common::InternalError(
          caller, std::format(
                      "observable descriptor {} name offset {} out of bounds "
                      "for pool size {}",
                      i, e.local_name_pool_off, pool_size));
    }

    bool is_pkg = (e.flags & lyra::runtime::kObservableFlagPackageGlobal) != 0;
    bool owner_abs =
        (e.flags & lyra::runtime::kObservableFlagOwnerAbsolute) != 0;

    if (require_package_global) {
      if (!is_pkg || !owner_abs) {
        throw lyra::common::InternalError(
            caller, std::format(
                        "package observable descriptor {} must be "
                        "package-global and absolute",
                        i));
      }
    } else if (is_pkg) {
      throw lyra::common::InternalError(
          caller,
          std::format(
              "body observable descriptor {} must not be package-global", i));
    }
  }
}

// Reconstruct a BodyDescriptorPackage view from a flat POD
// BodyDescriptorRef. File-local ABI bridge helper: centralizes span
// construction so LyraConstructorRunProgram does not inline field wiring.
auto MakeBodyDescriptorPackageView(const lyra::runtime::BodyDescriptorRef& ref)
    -> lyra::runtime::BodyDescriptorPackage {
  return lyra::runtime::BodyDescriptorPackage{
      .desc = ref.desc,
      .entries = std::span(ref.entries, ref.num_entries),
      .meta =
          lyra::runtime::ProcessMetaTemplateView{
              .entries = std::span(ref.meta_entries, ref.num_meta_entries),
              .pool = ref.meta_pool,
              .pool_size = ref.meta_pool_size,
          },
      .triggers =
          lyra::runtime::TriggerTemplateView{
              .entries =
                  std::span(ref.trigger_entries, ref.num_trigger_entries),
              .proc_ranges =
                  std::span(ref.trigger_ranges, ref.num_trigger_ranges),
              .proc_shapes =
                  std::span(ref.trigger_shapes, ref.num_trigger_ranges),
              .proc_groupable =
                  std::span(ref.trigger_groupable, ref.num_trigger_ranges),
          },
      .comb =
          lyra::runtime::CombTemplateView{
              .entries = std::span(ref.comb_entries, ref.num_comb_entries),
              .kernels = std::span(ref.comb_kernels, ref.num_comb_kernels),
          },
      .observable_descriptors =
          lyra::runtime::ObservableDescriptorTemplateView{
              .entries = std::span(ref.obs_entries, ref.num_obs_entries),
              .pool = ref.obs_pool,
              .pool_size = ref.obs_pool_size,
          },
      .init_recipe =
          lyra::runtime::StorageConstructionRecipeView{
              .ops = ref.init_recipe,
              .num_ops = ref.num_init_recipe_ops,
              .child_indices = ref.init_recipe_child_indices,
              .num_child_indices = ref.num_init_recipe_child_indices,
          },
      .init_recipe_roots =
          lyra::runtime::StorageConstructionRootView{
              .root_indices = ref.init_recipe_roots,
              .num_roots = ref.num_init_recipe_roots,
          },
      .init_params =
          lyra::runtime::ParamInitView{
              .slots =
                  std::span(ref.init_param_slots, ref.num_init_param_slots),
          },
      .decision_tables =
          std::span(ref.decision_tables, ref.num_decision_tables),
  };
}

}  // namespace

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
    uint32_t num_pkg_init_recipe_child_indices) -> void* {
  ValidateMetaAbiInputs(
      conn_meta_entries, num_conn_meta, conn_meta_pool, conn_meta_pool_size,
      "LyraConstructorCreate");
  ValidateObservableDescriptorTemplate(
      std::span(pkg_obs_entries, num_pkg_obs), pkg_obs_pool, pkg_obs_pool_size,
      true, "LyraConstructorCreate");
  ValidateAbiArray(
      pkg_init_recipe, num_pkg_init_recipe_ops, "pkg_init_recipe",
      "LyraConstructorCreate");
  ValidateAbiArray(
      pkg_init_recipe_roots, num_pkg_init_recipe_roots, "pkg_init_recipe_roots",
      "LyraConstructorCreate");
  ValidateAbiArray(
      pkg_init_recipe_child_indices, num_pkg_init_recipe_child_indices,
      "pkg_init_recipe_child_indices", "LyraConstructorCreate");
  lyra::runtime::ProcessMetaTemplateView conn_meta{
      .entries = std::span(conn_meta_entries, num_conn_meta),
      .pool = conn_meta_pool,
      .pool_size = conn_meta_pool_size,
  };
  lyra::runtime::TriggerTemplateView conn_triggers{
      .entries = std::span(conn_trigger_entries, num_conn_trigger_entries),
      .proc_ranges = std::span(conn_trigger_ranges, num_conn_trigger_ranges),
      .proc_shapes = std::span(conn_trigger_shapes, num_conn_trigger_ranges),
      .proc_groupable =
          std::span(conn_trigger_groupable, num_conn_trigger_ranges),
  };
  lyra::runtime::ObservableDescriptorTemplateView pkg_obs{
      .entries = std::span(pkg_obs_entries, num_pkg_obs),
      .pool = pkg_obs_pool,
      .pool_size = pkg_obs_pool_size,
  };
  lyra::runtime::StorageConstructionRecipeView pkg_recipe{
      .ops = pkg_init_recipe,
      .num_ops = num_pkg_init_recipe_ops,
      .child_indices = pkg_init_recipe_child_indices,
      .num_child_indices = num_pkg_init_recipe_child_indices,
  };
  lyra::runtime::StorageConstructionRootView pkg_recipe_roots{
      .root_indices = pkg_init_recipe_roots,
      .num_roots = num_pkg_init_recipe_roots,
  };
  auto ctor = std::make_unique<lyra::runtime::Constructor>(
      std::span(schemas, num_schemas), std::span(slot_byte_offsets, num_slots),
      num_package_slots,
      std::span(static_cast<std::byte*>(design_state), design_state_size),
      conn_meta, conn_triggers, pkg_obs, pkg_recipe, pkg_recipe_roots);
  return ctor.release();
}

void LyraConstructorAddConnection(
    void* ctor, const lyra::runtime::ConnectionRealizationDesc* desc) {
  ValidateHandle(ctor, "LyraConstructorAddConnection");
  ValidateHandle(desc, "LyraConstructorAddConnection");
  static_cast<lyra::runtime::Constructor*>(ctor)->AddConnection(*desc);
}

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
    uint32_t num_decision_tables) {
  ValidateHandle(ctor, "LyraConstructorBeginBody");
  ValidateHandle(desc, "LyraConstructorBeginBody");
  ValidateMetaAbiInputs(
      meta_entries, num_meta, meta_pool, meta_pool_size,
      "LyraConstructorBeginBody");
  ValidateObservableDescriptorTemplate(
      std::span(obs_entries, num_obs), obs_pool, obs_pool_size, false,
      "LyraConstructorBeginBody");
  ValidateAbiArray(
      init_recipe, num_init_recipe_ops, "init_recipe",
      "LyraConstructorBeginBody");
  ValidateAbiArray(
      init_recipe_roots, num_init_recipe_roots, "init_recipe_roots",
      "LyraConstructorBeginBody");
  ValidateAbiArray(
      init_recipe_child_indices, num_init_recipe_child_indices,
      "init_recipe_child_indices", "LyraConstructorBeginBody");
  ValidateAbiArray(
      init_param_slots, num_init_param_slots, "init_param_slots",
      "LyraConstructorBeginBody");
  lyra::runtime::BodyDescriptorPackage package{
      .desc = desc,
      .entries = std::span(entries, num_entries),
      .meta =
          lyra::runtime::ProcessMetaTemplateView{
              .entries = std::span(meta_entries, num_meta),
              .pool = meta_pool,
              .pool_size = meta_pool_size,
          },
      .triggers =
          lyra::runtime::TriggerTemplateView{
              .entries = std::span(trigger_entries, num_trigger_entries),
              .proc_ranges = std::span(trigger_ranges, num_trigger_ranges),
              .proc_shapes = std::span(trigger_shapes, num_trigger_ranges),
              .proc_groupable =
                  std::span(trigger_groupable, num_trigger_ranges),
          },
      .comb =
          lyra::runtime::CombTemplateView{
              .entries = std::span(comb_entries, num_comb_entries),
              .kernels = std::span(comb_kernels, num_comb_kernels),
          },
      .observable_descriptors =
          lyra::runtime::ObservableDescriptorTemplateView{
              .entries = std::span(obs_entries, num_obs),
              .pool = obs_pool,
              .pool_size = obs_pool_size,
          },
      .init_recipe =
          lyra::runtime::StorageConstructionRecipeView{
              .ops = init_recipe,
              .num_ops = num_init_recipe_ops,
              .child_indices = init_recipe_child_indices,
              .num_child_indices = num_init_recipe_child_indices,
          },
      .init_recipe_roots =
          lyra::runtime::StorageConstructionRootView{
              .root_indices = init_recipe_roots,
              .num_roots = num_init_recipe_roots,
          },
      .init_params =
          lyra::runtime::ParamInitView{
              .slots = std::span(init_param_slots, num_init_param_slots),
          },
      .decision_tables = std::span(decision_tables, num_decision_tables),
  };
  static_cast<lyra::runtime::Constructor*>(ctor)->BeginBody(package);
}

void LyraConstructorAddInstance(
    void* ctor, const char* instance_path, const void* param_data,
    uint32_t param_data_size, uint64_t realized_inline_size,
    uint64_t realized_appendix_size) {
  ValidateHandle(ctor, "LyraConstructorAddInstance");
  ValidateAbiArray(
      param_data, param_data_size, "param_data", "LyraConstructorAddInstance");
  static_cast<lyra::runtime::Constructor*>(ctor)->CreateChildInstance(
      nullptr, 0, 0, instance_path, param_data, param_data_size,
      /*arg_ptrs=*/{}, /*arg_byte_sizes=*/{}, realized_inline_size,
      realized_appendix_size);
}

void LyraConstructorRunProgram(
    void* ctor_raw, const lyra::runtime::BodyDescriptorRef* body_descs,
    uint32_t body_desc_count, const char* path_pool, uint32_t path_pool_size,
    const uint8_t* param_pool, uint32_t param_pool_size,
    const lyra::runtime::ConstructionProgramEntry* entries,
    uint32_t entry_count,
    const lyra::runtime::PortConstInitEntry* port_const_inits,
    uint32_t port_const_init_count, const uint8_t* port_const_pool,
    uint32_t port_const_pool_size) {
  ValidateHandle(ctor_raw, "LyraConstructorRunProgram");
  auto& ctor = *static_cast<lyra::runtime::Constructor*>(ctor_raw);

  if (entry_count != 0 && path_pool == nullptr) {
    throw lyra::common::InternalError(
        "LyraConstructorRunProgram",
        "non-empty construction program requires non-null path_pool");
  }

  uint32_t last_body_group = UINT32_MAX;
  auto entry_span = std::span(entries, entry_count);
  auto body_desc_span = std::span(body_descs, body_desc_count);
  auto path_span = std::span(path_pool, path_pool_size);
  auto param_span = std::span(param_pool, param_pool_size);
  auto pc_init_span = std::span(port_const_inits, port_const_init_count);
  auto pc_pool_span = std::span(port_const_pool, port_const_pool_size);

  // Transient scope-resolution table: converts flat parent_scope_index
  // from the emitted program into live scope pointers during ingestion.
  std::vector<lyra::runtime::RuntimeScope*> created_scopes;
  created_scopes.reserve(entry_count);

  // Cut-3 parallel tracker: true iff the scope at the same index belongs
  // to a body that opted into the backend-emitted construction path. When
  // true, the scope's direct children have already been created by the
  // parent body's emitted body-constructor and must not be re-created by
  // the flat replay loop.
  std::vector<uint8_t> created_scope_is_flagged_parent;
  created_scope_is_flagged_parent.reserve(entry_count);

  for (uint32_t i = 0; i < entry_count; ++i) {
    const auto& e = entry_span[i];

    // Resolve parent scope from transport index.
    lyra::runtime::RuntimeScope* parent_scope = nullptr;
    bool parent_is_flagged = false;
    if (e.parent_scope_index != UINT32_MAX) {
      if (e.parent_scope_index >= created_scopes.size()) {
        throw lyra::common::InternalError(
            "LyraConstructorRunProgram",
            std::format(
                "entry {} parent_scope_index {} >= created count {}", i,
                e.parent_scope_index, created_scopes.size()));
      }
      parent_scope = created_scopes[e.parent_scope_index];
      parent_is_flagged =
          created_scope_is_flagged_parent[e.parent_scope_index] != 0;
    }

    // Read parent-relative scope label from path pool. The constructor
    // derives the full hierarchical path from parent_scope + label.
    if (e.label_offset >= path_pool_size) {
      throw lyra::common::InternalError(
          "LyraConstructorRunProgram",
          std::format(
              "entry {} label_offset {} >= path_pool_size {}", i,
              e.label_offset, path_pool_size));
    }
    const char* label = &path_span[e.label_offset];

    if (e.node_kind == lyra::runtime::ConstructionNodeKind::kGenerate) {
      // Generate scope node: hierarchy-only, no body/storage/processes.
      auto* scope = ctor.CreateScope(parent_scope, label, e.ordinal_in_parent);
      created_scopes.push_back(scope);
      created_scope_is_flagged_parent.push_back(0);
    } else {
      // Module instance node.
      if (e.body_group >= body_desc_count) {
        throw lyra::common::InternalError(
            "LyraConstructorRunProgram",
            std::format(
                "entry {} body_group {} >= body_desc_count {}", i, e.body_group,
                body_desc_count));
      }

      const auto& this_body_ref = body_desc_span[e.body_group];
      bool this_body_uses_mir_ctor =
          this_body_ref.desc != nullptr &&
          this_body_ref.desc->uses_mir_constructor != 0;

      lyra::runtime::RuntimeInstance* inst = nullptr;

      if (parent_is_flagged) {
        // Cut-3 skip path: the parent body's emitted body-constructor
        // already materialized this child through
        // LyraConstructorAddChildObject. The flat replay loop only
        // reconstructs created_scopes and must not touch body activation,
        // storage, or port-const inits for this entry. Uncovered seams
        // (non-empty params, port-const inits, generate-scope parents)
        // stay on the old path because IsPlainChildConstructor rejects
        // them upstream.
        inst = ctor.GetStagedInstance(e.instance_index);
      } else {
        if (e.body_group != last_body_group) {
          ctor.BeginBody(MakeBodyDescriptorPackageView(this_body_ref));
          last_body_group = e.body_group;
        }

        const void* param_data = nullptr;
        uint32_t param_size = e.param_size;
        if (param_size != 0) {
          if (param_pool == nullptr) {
            throw lyra::common::InternalError(
                "LyraConstructorRunProgram",
                std::format(
                    "entry {} has param_size {} but param_pool is null", i,
                    param_size));
          }
          if (e.param_offset > param_pool_size ||
              param_size > param_pool_size - e.param_offset) {
            throw lyra::common::InternalError(
                "LyraConstructorRunProgram",
                std::format(
                    "entry {} param range [{}, {}) exceeds param_pool_size {}",
                    i, e.param_offset,
                    static_cast<uint64_t>(e.param_offset) + param_size,
                    param_pool_size));
          }
          param_data = &param_span[e.param_offset];
        }

        inst = ctor.CreateChildInstance(
            parent_scope, e.ordinal_in_parent, e.instance_index, label,
            param_data, param_size, /*arg_ptrs=*/{}, /*arg_byte_sizes=*/{},
            e.realized_inline_size, e.realized_appendix_size);

        // Apply constant port connection inits for this child.
        // port_const_init_offset/count index into the flat
        // port_const_inits array. Each entry writes a constant value to a
        // child slot.
        for (uint32_t pc = e.port_const_init_offset;
             pc < e.port_const_init_offset + e.port_const_init_count; ++pc) {
          if (pc >= port_const_init_count) {
            throw lyra::common::InternalError(
                "LyraConstructorRunProgram",
                std::format(
                    "entry {} port_const_init index {} >= count {}", i, pc,
                    port_const_init_count));
          }
          const auto& init = pc_init_span[pc];
          if (init.value_offset + init.value_size > port_const_pool_size) {
            throw lyra::common::InternalError(
                "LyraConstructorRunProgram",
                std::format(
                    "port const init value range [{}, {}) exceeds pool {}",
                    init.value_offset,
                    static_cast<uint64_t>(init.value_offset) + init.value_size,
                    port_const_pool_size));
          }
          auto* dst = ResolveInstanceStorageOffset(
              *inst, init.rel_byte_offset, init.value_size,
              "ApplyPortConstInit");
          std::memcpy(dst, &pc_pool_span[init.value_offset], init.value_size);
        }
      }

      created_scopes.push_back(&inst->scope);
      created_scope_is_flagged_parent.push_back(
          this_body_uses_mir_ctor ? 1 : 0);

      // Invoke the body-constructor for flagged bodies once the instance
      // exists (created here or picked up from the skip path). The
      // emitted fn runs on this scope and materializes its direct
      // children via LyraConstructorAddChildObject. Subsequent entries
      // whose parent is this instance take the skip path above.
      if (this_body_uses_mir_ctor &&
          this_body_ref.desc->body_constructor_fn != nullptr) {
        this_body_ref.desc->body_constructor_fn(&ctor, &inst->scope);
      }
    }
  }
}

void LyraConstructorBeginBodyByRef(
    void* ctor_raw, const lyra::runtime::BodyDescriptorRef* body_ref) {
  ValidateHandle(ctor_raw, "LyraConstructorBeginBodyByRef");
  if (body_ref == nullptr) {
    throw lyra::common::InternalError(
        "LyraConstructorBeginBodyByRef", "null body_ref");
  }
  auto& ctor = *static_cast<lyra::runtime::Constructor*>(ctor_raw);
  ctor.BeginBody(MakeBodyDescriptorPackageView(*body_ref));
}

auto LyraConstructorAddChildObject(
    void* ctor_raw, void* parent_scope_raw, uint32_t ordinal_in_parent,
    uint32_t instance_index, const char* label, uint64_t realized_inline_size,
    uint64_t realized_appendix_size, uint32_t num_args,
    const void* const* arg_ptrs, const uint32_t* arg_byte_sizes) -> void* {
  ValidateHandle(ctor_raw, "LyraConstructorAddChildObject");
  if (parent_scope_raw == nullptr) {
    throw lyra::common::InternalError(
        "LyraConstructorAddChildObject", "null parent_scope");
  }
  if (num_args > 0 && (arg_ptrs == nullptr || arg_byte_sizes == nullptr)) {
    throw lyra::common::InternalError(
        "LyraConstructorAddChildObject",
        "num_args > 0 requires non-null arg arrays");
  }
  auto& ctor = *static_cast<lyra::runtime::Constructor*>(ctor_raw);
  auto* parent_scope =
      static_cast<lyra::runtime::RuntimeScope*>(parent_scope_raw);
  return ctor.CreateChildInstanceDirect(
      parent_scope, ordinal_in_parent, instance_index, label,
      realized_inline_size, realized_appendix_size, num_args, arg_ptrs,
      arg_byte_sizes);
}

auto LyraConstructorFinalize(void* ctor_raw) -> void* {
  ValidateHandle(ctor_raw, "LyraConstructorFinalize");
  std::unique_ptr<lyra::runtime::Constructor> ctor(
      static_cast<lyra::runtime::Constructor*>(ctor_raw));
  auto result =
      std::make_unique<lyra::runtime::ConstructionResult>(ctor->Finalize());
  return result.release();
}

auto LyraConstructionResultGetStates(void* result_raw) -> void** {
  ValidateHandle(result_raw, "LyraConstructionResultGetStates");
  return static_cast<lyra::runtime::ConstructionResult*>(result_raw)
      ->states.data();
}

auto LyraConstructionResultGetNumTotal(void* result_raw) -> uint32_t {
  ValidateHandle(result_raw, "LyraConstructionResultGetNumTotal");
  return static_cast<lyra::runtime::ConstructionResult*>(result_raw)->num_total;
}

auto LyraConstructionResultGetNumConnection(void* result_raw) -> uint32_t {
  ValidateHandle(result_raw, "LyraConstructionResultGetNumConnection");
  return static_cast<lyra::runtime::ConstructionResult*>(result_raw)
      ->num_connection;
}

auto LyraConstructionResultGetProcessMetaWords(void* result_raw)
    -> const uint32_t* {
  ValidateHandle(result_raw, "LyraConstructionResultGetProcessMetaWords");
  return static_cast<lyra::runtime::ConstructionResult*>(result_raw)
      ->process_meta.words.data();
}

auto LyraConstructionResultGetProcessMetaWordCount(void* result_raw)
    -> uint32_t {
  ValidateHandle(result_raw, "LyraConstructionResultGetProcessMetaWordCount");
  auto& words = static_cast<lyra::runtime::ConstructionResult*>(result_raw)
                    ->process_meta.words;
  return static_cast<uint32_t>(
      words.size() / lyra::runtime::process_meta_abi::kStride);
}

auto LyraConstructionResultGetProcessMetaPool(void* result_raw) -> const char* {
  ValidateHandle(result_raw, "LyraConstructionResultGetProcessMetaPool");
  return static_cast<lyra::runtime::ConstructionResult*>(result_raw)
      ->process_meta.pool.data();
}

auto LyraConstructionResultGetProcessMetaPoolSize(void* result_raw)
    -> uint32_t {
  ValidateHandle(result_raw, "LyraConstructionResultGetProcessMetaPoolSize");
  return static_cast<uint32_t>(
      static_cast<lyra::runtime::ConstructionResult*>(result_raw)
          ->process_meta.pool.size());
}

auto LyraConstructionResultGetTriggerWords(void* result_raw)
    -> const uint32_t* {
  ValidateHandle(result_raw, "LyraConstructionResultGetTriggerWords");
  auto& meta =
      static_cast<lyra::runtime::ConstructionResult*>(result_raw)->trigger_meta;
  if (meta.IsEmpty()) return nullptr;
  return meta.words.data();
}

auto LyraConstructionResultGetTriggerWordCount(void* result_raw) -> uint32_t {
  ValidateHandle(result_raw, "LyraConstructionResultGetTriggerWordCount");
  auto& meta =
      static_cast<lyra::runtime::ConstructionResult*>(result_raw)->trigger_meta;
  if (meta.IsEmpty()) return 0;
  return static_cast<uint32_t>(meta.words.size());
}

// R4: comb metadata is engine-derived from bundles. Returns empty.
auto LyraConstructionResultGetCombWords(void* result_raw) -> const uint32_t* {
  ValidateHandle(result_raw, "LyraConstructionResultGetCombWords");
  return nullptr;
}

auto LyraConstructionResultGetCombWordCount(void* result_raw) -> uint32_t {
  ValidateHandle(result_raw, "LyraConstructionResultGetCombWordCount");
  return 0;
}

auto LyraConstructionResultGetSlotMetaWords(void* result_raw)
    -> const uint32_t* {
  ValidateHandle(result_raw, "LyraConstructionResultGetSlotMetaWords");
  auto& meta =
      static_cast<lyra::runtime::ConstructionResult*>(result_raw)->slot_meta;
  if (meta.slot_count == 0) return nullptr;
  return meta.words.data();
}

auto LyraConstructionResultGetSlotMetaCount(void* result_raw) -> uint32_t {
  ValidateHandle(result_raw, "LyraConstructionResultGetSlotMetaCount");
  return static_cast<lyra::runtime::ConstructionResult*>(result_raw)
      ->slot_meta.slot_count;
}

auto LyraConstructionResultGetTraceSignalMetaWords(void* result_raw)
    -> const uint32_t* {
  ValidateHandle(result_raw, "LyraConstructionResultGetTraceSignalMetaWords");
  auto& meta = static_cast<lyra::runtime::ConstructionResult*>(result_raw)
                   ->trace_signal_meta;
  if (meta.signal_count == 0) return nullptr;
  return meta.words.data();
}

auto LyraConstructionResultGetTraceSignalMetaWordCount(void* result_raw)
    -> uint32_t {
  ValidateHandle(
      result_raw, "LyraConstructionResultGetTraceSignalMetaWordCount");
  return static_cast<uint32_t>(
      static_cast<lyra::runtime::ConstructionResult*>(result_raw)
          ->trace_signal_meta.words.size());
}

auto LyraConstructionResultGetTraceSignalMetaPool(void* result_raw) -> const
    char* {
  ValidateHandle(result_raw, "LyraConstructionResultGetTraceSignalMetaPool");
  auto& meta = static_cast<lyra::runtime::ConstructionResult*>(result_raw)
                   ->trace_signal_meta;
  if (meta.pool.empty()) return nullptr;
  return meta.pool.data();
}

auto LyraConstructionResultGetTraceSignalMetaPoolSize(void* result_raw)
    -> uint32_t {
  ValidateHandle(
      result_raw, "LyraConstructionResultGetTraceSignalMetaPoolSize");
  return static_cast<uint32_t>(
      static_cast<lyra::runtime::ConstructionResult*>(result_raw)
          ->trace_signal_meta.pool.size());
}

auto LyraConstructionResultGetInstances(void* result_raw)
    -> const lyra::runtime::RuntimeInstance* const* {
  ValidateHandle(result_raw, "LyraConstructionResultGetInstances");
  auto& result = *static_cast<lyra::runtime::ConstructionResult*>(result_raw);
  if (result.instance_ptrs.empty()) return nullptr;
  return result.instance_ptrs.data();
}

auto LyraConstructionResultGetInstanceCount(void* result_raw) -> uint32_t {
  ValidateHandle(result_raw, "LyraConstructionResultGetInstanceCount");
  return static_cast<uint32_t>(
      static_cast<lyra::runtime::ConstructionResult*>(result_raw)
          ->instance_ptrs.size());
}

auto LyraConstructionResultGetInstanceBundles(void* result_raw)
    -> const lyra::runtime::InstanceMetadataBundle* {
  ValidateHandle(result_raw, "LyraConstructionResultGetInstanceBundles");
  auto& bundles = static_cast<lyra::runtime::ConstructionResult*>(result_raw)
                      ->instance_bundles;
  if (bundles.empty()) return nullptr;
  return bundles.data();
}

auto LyraConstructionResultGetInstanceBundleCount(void* result_raw)
    -> uint32_t {
  ValidateHandle(result_raw, "LyraConstructionResultGetInstanceBundleCount");
  return static_cast<uint32_t>(
      static_cast<lyra::runtime::ConstructionResult*>(result_raw)
          ->instance_bundles.size());
}

void LyraConstructionResultDestroy(void* result_raw) {
  if (result_raw == nullptr) return;
  std::unique_ptr<lyra::runtime::ConstructionResult> result(
      static_cast<lyra::runtime::ConstructionResult*>(result_raw));
}

void LyraConstructionResultSetExtRefBindings(
    void* result_raw, const void* pool_raw, uint32_t pool_count,
    const uint32_t* offsets, const uint32_t* counts, uint32_t num_instances) {
  if (result_raw == nullptr) return;
  auto& result = *static_cast<lyra::runtime::ConstructionResult*>(result_raw);
  auto pool = std::span(
      static_cast<const lyra::common::SerializedExtRefBinding*>(pool_raw),
      pool_count);
  auto offset_span = std::span(offsets, num_instances);
  auto count_span = std::span(counts, num_instances);
  auto n =
      std::min(static_cast<size_t>(num_instances), result.instances.size());
  for (size_t i = 0; i < n; ++i) {
    if (offset_span[i] == UINT32_MAX) {
      result.instances[i]->ext_ref_bindings = nullptr;
      result.instances[i]->ext_ref_binding_count = 0;
      continue;
    }
    auto serialized_bindings = pool.subspan(offset_span[i], count_span[i]);
    auto& inst = *result.instances[i];
    inst.owned_ext_ref_bindings.resize(serialized_bindings.size());
    for (size_t j = 0; j < serialized_bindings.size(); ++j) {
      const auto& s = serialized_bindings[j];
      if (s.target_instance_id >= result.instances.size()) {
        throw lyra::common::InternalError(
            "LyraConstructionResultSetExtRefBindings",
            std::format(
                "ext-ref binding [instance={}, ref={}] has "
                "target_instance_id {} >= instance count {}",
                i, j, s.target_instance_id, result.instances.size()));
      }
      inst.owned_ext_ref_bindings[j] = lyra::runtime::ResolvedExtRefBinding{
          .target_instance = result.instances[s.target_instance_id].get(),
          .target_byte_offset = s.target_byte_offset,
          .target_local_signal = s.target_local_signal,
      };
    }
    inst.ext_ref_bindings = inst.owned_ext_ref_bindings.data();
    inst.ext_ref_binding_count =
        static_cast<uint32_t>(inst.owned_ext_ref_bindings.size());
  }
}

void LyraConstructionResultSetConnectionDescriptors(
    void* result_raw, const void* serialized_raw, uint32_t num_descs) {
  if (result_raw == nullptr || serialized_raw == nullptr || num_descs == 0) {
    return;
  }
  auto& result = *static_cast<lyra::runtime::ConstructionResult*>(result_raw);
  auto serialized = std::span(
      static_cast<const lyra::runtime::SerializedConnectionDescriptor*>(
          serialized_raw),
      num_descs);
  result.connection_descriptors.reserve(num_descs);
  for (const auto& s : serialized) {
    auto* src = result.instances.at(s.src_object_index).get();
    auto* dst = result.instances.at(s.dst_object_index).get();
    auto* trig = result.instances.at(s.trigger_object_index).get();
    result.connection_descriptors.push_back(
        lyra::runtime::RuntimeConnectionDescriptor{
            .src_instance = src,
            .src_byte_offset = s.src_byte_offset,
            .dst_instance = dst,
            .dst_byte_offset = s.dst_byte_offset,
            .dst_local_signal =
                lyra::runtime::LocalSignalId{s.dst_local_signal},
            .byte_size = s.byte_size,
            .trigger_edge = s.trigger_edge,
            .trigger_bit_index = s.trigger_bit_index,
            .trigger_byte_offset = s.trigger_byte_offset,
            .trigger_byte_size = s.trigger_byte_size,
            .trigger_instance = trig,
            .trigger_local_id =
                lyra::runtime::LocalSignalId{s.trigger_local_id},
        });
  }
}

auto LyraConstructionResultGetConnectionDescriptors(void* result_raw) -> void* {
  if (result_raw == nullptr) return nullptr;
  auto& result = *static_cast<lyra::runtime::ConstructionResult*>(result_raw);
  if (result.connection_descriptors.empty()) return nullptr;
  return result.connection_descriptors.data();
}

auto LyraConstructionResultGetConnectionDescriptorCount(void* result_raw)
    -> uint32_t {
  if (result_raw == nullptr) return 0;
  return static_cast<uint32_t>(
      static_cast<lyra::runtime::ConstructionResult*>(result_raw)
          ->connection_descriptors.size());
}
