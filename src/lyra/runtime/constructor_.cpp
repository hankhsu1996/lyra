// NOTE: See constructor_.hpp for why this file has a trailing underscore.
#include "lyra/runtime/constructor_.hpp"

#include <cstdint>
#include <cstring>
#include <format>
#include <memory>
#include <span>
#include <string_view>
#include <utility>

#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/frame_allocator.hpp"
#include "lyra/runtime/owned_storage_handle.hpp"
#include "lyra/runtime/process_frame.hpp"
#include "lyra/runtime/process_meta_abi.hpp"
#include "lyra/runtime/process_trigger_abi.hpp"
#include "lyra/runtime/slot_meta.hpp"
#include "lyra/runtime/storage_construction.hpp"

namespace lyra::runtime {

ConstructionResult::~ConstructionResult() {
  FreePackedBuffer(packed_buffer);
}

ConstructionResult::ConstructionResult(ConstructionResult&& other) noexcept
    : states(std::move(other.states)),
      packed_buffer(std::exchange(other.packed_buffer, nullptr)),
      num_total(std::exchange(other.num_total, 0)),
      num_connection(std::exchange(other.num_connection, 0)),
      instances(std::move(other.instances)),
      instance_bundles(std::move(other.instance_bundles)),
      body_desc_storage(std::move(other.body_desc_storage)),
      process_meta(std::move(other.process_meta)),
      trigger_meta(std::move(other.trigger_meta)),
      slot_meta(std::move(other.slot_meta)),
      trace_signal_meta(std::move(other.trace_signal_meta)),
      instance_paths(std::move(other.instance_paths)),
      instance_path_ptrs(std::move(other.instance_path_ptrs)),
      instance_ptrs(std::move(other.instance_ptrs)) {
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
    instance_bundles = std::move(other.instance_bundles);
    body_desc_storage = std::move(other.body_desc_storage);
    process_meta = std::move(other.process_meta);
    trigger_meta = std::move(other.trigger_meta);
    slot_meta = std::move(other.slot_meta);
    trace_signal_meta = std::move(other.trace_signal_meta);
    instance_paths = std::move(other.instance_paths);
    instance_path_ptrs = std::move(other.instance_path_ptrs);
    instance_ptrs = std::move(other.instance_ptrs);
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

void ApplyParamInitToInstance(
    RuntimeInstance& instance, std::span<const ParamInitSlotEntry> slots,
    const void* value_data, uint32_t value_total_bytes) {
  if (slots.empty()) return;
  auto src =
      std::span(static_cast<const uint8_t*>(value_data), value_total_bytes);
  uint32_t src_offset = 0;
  for (const auto& slot : slots) {
    if (src_offset + slot.byte_size > value_total_bytes) {
      throw lyra::common::InternalError(
          "ApplyParamInitToInstance",
          std::format(
              "param payload overrun: offset {} + size {} > total {}",
              src_offset, slot.byte_size, value_total_bytes));
    }
    auto* dst = ResolveInstanceStorageOffset(
        instance, slot.rel_byte_offset, slot.byte_size,
        "ApplyParamInitToInstance");
    std::memcpy(dst, &src[src_offset], slot.byte_size);
    src_offset += slot.byte_size;
  }
  if (src_offset != value_total_bytes) {
    throw lyra::common::InternalError(
        "ApplyParamInitToInstance",
        std::format(
            "param payload size mismatch: consumed {} bytes but payload has {}",
            src_offset, value_total_bytes));
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

void Constructor::AddInstance(
    const char* instance_path, const void* param_data, uint32_t param_data_size,
    uint64_t realized_inline_size, uint64_t realized_appendix_size) {
  CheckNotFinalized("Constructor::AddInstance");
  if (instance_path == nullptr) {
    throw common::InternalError(
        "Constructor::AddInstance", "null instance_path");
  }
  if (!body_.active || current_body_package_ == nullptr) {
    throw common::InternalError(
        "Constructor::AddInstance",
        "no active body package (call BeginBody first)");
  }
  if ((param_data == nullptr) != (param_data_size == 0)) {
    throw common::InternalError(
        "Constructor::AddInstance",
        "param_data and param_data_size must be both empty or both present");
  }
  connections_finalized_ = true;

  uint32_t instance_id = next_instance_id_;

  uint32_t instance_ord = next_module_instance_ordinal_;
  auto instance = std::make_unique<RuntimeInstance>();
  instance->instance_id = instance_id;
  instance->owner_ordinal = instance_ord;
  // path_c_str is patched in Finalize to point into stable instance_paths.
  instance->path_c_str = nullptr;

  instance->storage.inline_base =
      AllocateOwnedInlineStorage(realized_inline_size);
  instance->storage.inline_size = realized_inline_size;
  instance->storage.appendix_base =
      AllocateOwnedAppendixStorage(realized_appendix_size);
  instance->storage.appendix_size = realized_appendix_size;

  // Invariant: zero-slot bodies must not carry instance-state init.
  if (body_.slot_count == 0) {
    if (body_.init_recipe.num_ops != 0 ||
        body_.init_recipe_roots.num_roots != 0 ||
        body_.init_recipe.num_child_indices != 0 ||
        !body_.init_params.slots.empty() || param_data_size != 0) {
      throw common::InternalError(
          "Constructor::AddInstance",
          "zero-slot body must not carry instance-state init descriptors "
          "or param data");
    }
  }

  // Body-shaped storage construction.
  ApplyStorageConstructionRecipeToInstance(
      *instance, body_.init_recipe, body_.init_recipe_roots);
  ApplyParamInitToInstance(
      *instance, body_.init_params.slots, param_data, param_data_size);

  // Capture module_proc_base before staging non-final body processes.
  auto module_proc_base = static_cast<uint32_t>(staged_.size());

  auto instance_index = static_cast<uint32_t>(staged_instances_.size());
  staged_instances_.push_back(std::move(instance));

  for (const auto& entry : body_.entries) {
    SharedBodyFn body_fn{};
    std::memcpy(&body_fn, &entry.shared_body_fn, sizeof(body_fn));

    staged_.push_back(
        StagedProcess{
            .schema_index = entry.schema_index,
            .body = body_fn,
            .instance_index = instance_index,
            .is_module = true,
        });
  }

  ++next_module_instance_ordinal_;
  auto num_module_processes =
      static_cast<uint32_t>(staged_.size()) - module_proc_base;

  // Complete the instance's process binding fields now that staging is done.
  staged_instances_[instance_index]->module_proc_base = module_proc_base;
  staged_instances_[instance_index]->num_module_processes =
      num_module_processes;

  // R4: Module-instance process meta, trigger, and comb metadata are no
  // longer flattened here. The engine derives them from per-instance bundles
  // and body templates.
  //
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
              "Constructor::AddInstance",
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

  // Collect instance path for runtime naming ownership.
  instance_paths_.emplace_back(instance_path);

  // R4 prep: record per-instance metadata bundle alongside existing flat
  // metadata. instance_path pointer is patched in Finalize after string
  // storage is stable.
  staged_bundles_.push_back(
      InstanceMetadataBundle{
          .instance = staged_instances_[instance_index].get(),
          .body_desc = nullptr,
          .body_key = current_body_package_->desc,
          .instance_id = instance_id,
          .module_proc_base = module_proc_base,
          .num_module_processes = num_module_processes,
          .instance_path = nullptr,
      });

  uint32_t new_slot_base = next_slot_base_ + body_.slot_count;
  if (new_slot_base < next_slot_base_) {
    throw common::InternalError(
        "Constructor::AddInstance", "slot base overflow");
  }
  if (body_.slot_count > 0 && new_slot_base > slot_byte_offsets_.size()) {
    throw common::InternalError(
        "Constructor::AddInstance",
        std::format(
            "post-increment slot base {} exceeds layout oracle size {}",
            new_slot_base, slot_byte_offsets_.size()));
  }
  next_instance_id_ += 1;
  next_slot_base_ = new_slot_base;
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

namespace {

void PopulateInstancePathPtrs(ConstructionResult& result) {
  result.instance_path_ptrs.clear();
  result.instance_path_ptrs.reserve(result.instance_paths.size());
  for (const auto& p : result.instance_paths) {
    result.instance_path_ptrs.push_back(p.c_str());
  }
}

}  // namespace

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

  // Process-index identity verification.
  // Instance ordinals are dense and match staged_instances_ position:
  // staged_instances_[ordinal]->owner_ordinal == ordinal.
  for (uint32_t li = 0; li < staged_instances_.size(); ++li) {
    if (staged_instances_[li]->owner_ordinal != li) {
      throw common::InternalError(
          "Constructor::Finalize",
          std::format(
              "instance ordinal mismatch: entry {} has "
              "owner_ordinal {}",
              li, staged_instances_[li]->owner_ordinal));
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
    result.instance_paths = std::move(instance_paths_);
    PopulateInstancePathPtrs(result);
    return result;
  }

  std::vector<uint32_t> schema_indices;
  schema_indices.reserve(num_total);
  for (const auto& proc : staged_) {
    schema_indices.push_back(proc.schema_index);
  }

  std::vector<void*> states(num_total);
  void* packed_buffer = AllocateProcessFrames(
      std::span(states), std::span(schema_indices), schemas_);

  for (uint32_t i = 0; i < num_total; ++i) {
    auto* header = static_cast<ProcessFrameHeader*>(states[i]);
    header->design_ptr = design_state_.data();
    header->process_id = i;

    const auto& proc = staged_[i];
    if (proc.is_module) {
      header->body = proc.body;
      header->instance = staged_instances_[proc.instance_index].get();
    }
  }

  ConstructionResult result;
  result.states = std::move(states);
  result.packed_buffer = packed_buffer;
  result.num_total = num_total;
  result.num_connection = num_connection_;
  result.instances = std::move(staged_instances_);
  result.process_meta = std::move(realized_meta_);
  result.trigger_meta = std::move(realized_triggers_);
  result.slot_meta = std::move(realized_slot_meta_);
  result.trace_signal_meta = std::move(realized_trace_meta_);
  result.instance_paths = std::move(instance_paths_);

  // Patch path_c_str on each instance to point into the stable
  // result.instance_paths storage. Instance i corresponds to
  // instance_paths[i] by construction (AddInstance appends both
  // in the same order).
  for (uint32_t i = 0; i < result.instances.size(); ++i) {
    result.instances[i]->path_c_str = result.instance_paths[i].c_str();
  }

  PopulateInstancePathPtrs(result);

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

  if (result.instance_bundles.size() != result.instance_paths.size() ||
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
    if (bundle.instance_id != i) {
      throw common::InternalError(
          "Constructor::Finalize",
          std::format(
              "R4 bundle {} instance_id mismatch: got {}", i,
              bundle.instance_id));
    }
    if (bundle.instance != expected_instance) {
      throw common::InternalError(
          "Constructor::Finalize",
          std::format("R4 bundle {} instance pointer mismatch", i));
    }
    if (bundle.instance->instance_id != bundle.instance_id) {
      throw common::InternalError(
          "Constructor::Finalize",
          std::format(
              "R4 bundle {} pointed instance_id mismatch: "
              "bundle={}, instance={}",
              i, bundle.instance_id, bundle.instance->instance_id));
    }

    result.instance_bundles[i].instance_path = result.instance_paths[i].c_str();
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
  static_cast<lyra::runtime::Constructor*>(ctor)->AddInstance(
      instance_path, param_data, param_data_size, realized_inline_size,
      realized_appendix_size);
}

void LyraConstructorRunProgram(
    void* ctor_raw, const lyra::runtime::BodyDescriptorRef* body_descs,
    uint32_t body_desc_count, const char* path_pool, uint32_t path_pool_size,
    const uint8_t* param_pool, uint32_t param_pool_size,
    const lyra::runtime::ConstructionProgramEntry* entries,
    uint32_t entry_count) {
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

  for (uint32_t i = 0; i < entry_count; ++i) {
    const auto& e = entry_span[i];

    if (e.body_group >= body_desc_count) {
      throw lyra::common::InternalError(
          "LyraConstructorRunProgram",
          std::format(
              "entry {} body_group {} >= body_desc_count {}", i, e.body_group,
              body_desc_count));
    }

    if (e.body_group != last_body_group) {
      ctor.BeginBody(
          MakeBodyDescriptorPackageView(body_desc_span[e.body_group]));
      last_body_group = e.body_group;
    }

    if (e.path_offset >= path_pool_size) {
      throw lyra::common::InternalError(
          "LyraConstructorRunProgram",
          std::format(
              "entry {} path_offset {} >= path_pool_size {}", i, e.path_offset,
              path_pool_size));
    }
    const char* instance_path = &path_span[e.path_offset];

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
                "entry {} param range [{}, {}) exceeds param_pool_size {}", i,
                e.param_offset,
                static_cast<uint64_t>(e.param_offset) + param_size,
                param_pool_size));
      }
      param_data = &param_span[e.param_offset];
    }

    ctor.AddInstance(
        instance_path, param_data, param_size, e.realized_inline_size,
        e.realized_appendix_size);
  }
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

auto LyraConstructionResultGetInstancePaths(void* result_raw) -> const char** {
  ValidateHandle(result_raw, "LyraConstructionResultGetInstancePaths");
  auto& result = *static_cast<lyra::runtime::ConstructionResult*>(result_raw);
  if (result.instance_path_ptrs.empty()) return nullptr;
  return result.instance_path_ptrs.data();
}

auto LyraConstructionResultGetInstancePathCount(void* result_raw) -> uint32_t {
  ValidateHandle(result_raw, "LyraConstructionResultGetInstancePathCount");
  return static_cast<uint32_t>(
      static_cast<lyra::runtime::ConstructionResult*>(result_raw)
          ->instance_paths.size());
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
