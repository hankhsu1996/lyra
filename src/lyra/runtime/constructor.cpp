#include "lyra/runtime/constructor.hpp"

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

namespace lyra::runtime {

ConstructionResult::~ConstructionResult() {
  FreePackedBuffer(packed_buffer);
}

ConstructionResult::ConstructionResult(ConstructionResult&& other) noexcept
    : states(std::move(other.states)),
      packed_buffer(std::exchange(other.packed_buffer, nullptr)),
      num_total(std::exchange(other.num_total, 0)),
      num_connection(std::exchange(other.num_connection, 0)),
      process_meta(std::move(other.process_meta)),
      trigger_meta(std::move(other.trigger_meta)),
      comb_meta(std::move(other.comb_meta)),
      slot_meta(std::move(other.slot_meta)),
      trace_signal_meta(std::move(other.trace_signal_meta)),
      instance_paths(std::move(other.instance_paths)),
      instance_path_ptrs(std::move(other.instance_path_ptrs)) {
}

auto ConstructionResult::operator=(ConstructionResult&& other) noexcept
    -> ConstructionResult& {
  if (this != &other) {
    FreePackedBuffer(packed_buffer);
    states = std::move(other.states);
    packed_buffer = std::exchange(other.packed_buffer, nullptr);
    num_total = std::exchange(other.num_total, 0);
    num_connection = std::exchange(other.num_connection, 0);
    process_meta = std::move(other.process_meta);
    trigger_meta = std::move(other.trigger_meta);
    comb_meta = std::move(other.comb_meta);
    slot_meta = std::move(other.slot_meta);
    trace_signal_meta = std::move(other.trace_signal_meta);
    instance_paths = std::move(other.instance_paths);
    instance_path_ptrs = std::move(other.instance_path_ptrs);
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

void RealizedCombMeta::Init() {
  words.clear();
  words.push_back(0);
  kernel_count = 0;
}

auto RealizedCombMeta::BeginKernel(uint32_t proc_idx, uint32_t flags)
    -> uint32_t {
  if (words.empty()) {
    throw common::InternalError(
        "RealizedCombMeta::BeginKernel", "Init() not called");
  }
  words.push_back(proc_idx);
  words.push_back(flags);
  auto trigger_count_pos = static_cast<uint32_t>(words.size());
  words.push_back(0);
  ++kernel_count;
  return trigger_count_pos;
}

void RealizedCombMeta::AppendTrigger(
    uint32_t slot_id, uint32_t byte_off, uint32_t byte_size) {
  if (words.empty()) {
    throw common::InternalError(
        "RealizedCombMeta::AppendTrigger", "Init() not called");
  }
  words.push_back(slot_id);
  words.push_back(byte_off);
  words.push_back(byte_size);
}

void RealizedCombMeta::EndKernel(
    uint32_t trigger_count_pos, uint32_t trigger_count) {
  if (trigger_count_pos >= words.size()) {
    throw common::InternalError(
        "RealizedCombMeta::EndKernel",
        std::format(
            "trigger_count_pos {} >= words.size() {}", trigger_count_pos,
            words.size()));
  }
  words[trigger_count_pos] = trigger_count;
}

void RealizedCombMeta::Finalize() {
  if (!words.empty()) {
    words[0] = kernel_count;
  }
}

void RealizedSlotMeta::Init() {
  words.clear();
  slot_count = 0;
}

void RealizedSlotMeta::AppendSlot(
    uint32_t byte_offset, uint32_t total_bytes, uint32_t storage_kind,
    uint32_t value_off, uint32_t value_bytes, uint32_t unk_off,
    uint32_t unk_bytes, uint32_t storage_owner_slot_id) {
  words.push_back(byte_offset);
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

void ValidateByteBase(
    uint64_t byte_base, size_t design_state_size, const char* caller) {
  if (byte_base > design_state_size) {
    throw lyra::common::InternalError(
        caller, std::format(
                    "byte_base {} exceeds design_state size {}", byte_base,
                    design_state_size));
  }
}

void ApplyInitPatches(
    std::span<std::byte> design_state, uint64_t byte_base,
    std::span<const InitPatchEntry> patches) {
  ValidateByteBase(byte_base, design_state.size(), "ApplyInitPatches");
  auto* base = reinterpret_cast<uint8_t*>(design_state.data()) + byte_base;
  for (const auto& p : patches) {
    uint64_t end =
        byte_base + static_cast<uint64_t>(p.rel_byte_offset) + p.byte_width;
    if (end > design_state.size()) {
      throw lyra::common::InternalError(
          "ApplyInitPatches",
          std::format(
              "patch range [{}..{}) exceeds design_state size {}",
              byte_base + static_cast<uint64_t>(p.rel_byte_offset), end,
              design_state.size()));
    }
    std::memcpy(base + p.rel_byte_offset, &p.mask, p.byte_width);
  }
}

static_assert(std::is_standard_layout_v<lyra::runtime::OwnedStorageHandle>);
static_assert(
    sizeof(lyra::runtime::OwnedStorageHandle) ==
    sizeof(void*) + sizeof(uint64_t));

void ConstructInitHandles(
    std::span<std::byte> design_state, uint64_t byte_base,
    std::span<const InitHandleEntry> handles) {
  ValidateByteBase(byte_base, design_state.size(), "ConstructInitHandles");
  for (const auto& h : handles) {
    uint64_t handle_end = byte_base + h.handle_rel_byte_offset +
                          sizeof(lyra::runtime::OwnedStorageHandle);
    if (handle_end > design_state.size()) {
      throw lyra::common::InternalError(
          "ConstructInitHandles",
          std::format(
              "handle write range [{}..{}) exceeds design_state size {}",
              byte_base + h.handle_rel_byte_offset, handle_end,
              design_state.size()));
    }
    uint64_t backing_end =
        byte_base + h.backing_rel_byte_offset + h.backing_byte_size;
    if (backing_end > design_state.size()) {
      throw lyra::common::InternalError(
          "ConstructInitHandles",
          std::format(
              "backing range [{}..{}) exceeds design_state size {}",
              byte_base + h.backing_rel_byte_offset, backing_end,
              design_state.size()));
    }

    auto* handle_addr =
        design_state.data() + byte_base + h.handle_rel_byte_offset;
    auto* backing_addr =
        design_state.data() + byte_base + h.backing_rel_byte_offset;

    lyra::runtime::OwnedStorageHandle handle{
        .data = backing_addr,
        .byte_size = h.backing_byte_size,
    };
    std::memcpy(handle_addr, &handle, sizeof(handle));
  }
}

void ApplyParamInit(
    std::span<std::byte> design_state, uint64_t byte_base,
    std::span<const ParamInitSlotEntry> slots, const void* value_data,
    uint32_t value_total_bytes) {
  if (slots.empty()) return;
  ValidateByteBase(byte_base, design_state.size(), "ApplyParamInit");
  auto* base = reinterpret_cast<uint8_t*>(design_state.data()) + byte_base;
  const auto* src = static_cast<const uint8_t*>(value_data);
  uint32_t src_offset = 0;
  for (const auto& slot : slots) {
    if (src_offset + slot.byte_size > value_total_bytes) {
      throw lyra::common::InternalError(
          "ApplyParamInit",
          std::format(
              "param payload overrun: offset {} + size {} > total {}",
              src_offset, slot.byte_size, value_total_bytes));
    }
    uint64_t dst_end = byte_base + static_cast<uint64_t>(slot.rel_byte_offset) +
                       slot.byte_size;
    if (dst_end > design_state.size()) {
      throw lyra::common::InternalError(
          "ApplyParamInit",
          std::format(
              "param dest range [{}..{}) exceeds design_state size {}",
              byte_base + static_cast<uint64_t>(slot.rel_byte_offset), dst_end,
              design_state.size()));
    }
    std::memcpy(base + slot.rel_byte_offset, src + src_offset, slot.byte_size);
    src_offset += slot.byte_size;
  }
  if (src_offset != value_total_bytes) {
    throw lyra::common::InternalError(
        "ApplyParamInit",
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
    InitPatchView pkg_init_patches, InitHandleView pkg_init_handles)
    : schemas_(schemas),
      slot_byte_offsets_(slot_byte_offsets),
      design_state_(design_state),
      next_slot_base_(num_package_slots),
      conn_meta_(conn_meta),
      conn_triggers_(conn_triggers),
      pkg_observable_(pkg_observable),
      pkg_init_patches_(pkg_init_patches),
      pkg_init_handles_(pkg_init_handles) {
  ValidateMetaTemplate(conn_meta_, "Constructor");
  realized_meta_.pool.push_back('\0');
  realized_triggers_.Init();
  realized_comb_.Init();
  realized_slot_meta_.Init();
  realized_trace_meta_.Init();

  // Package/global observable descriptor prelude.
  // Realized before any body-instance expansion. All entries are absolute.
  for (const auto& entry : pkg_observable_.entries) {
    realized_slot_meta_.AppendSlot(
        entry.storage_byte_offset, entry.total_bytes, entry.storage_kind,
        entry.value_lane_offset, entry.value_lane_bytes, entry.unk_lane_offset,
        entry.unk_lane_bytes, entry.storage_owner_ref);

    std::string_view name;
    if (entry.local_name_pool_off > 0 &&
        entry.local_name_pool_off < pkg_observable_.pool_size) {
      name = &pkg_observable_.pool[entry.local_name_pool_off];
    }
    uint32_t name_off = realized_trace_meta_.AppendName(name);
    realized_trace_meta_.AppendSignal(
        name_off, entry.bit_width, entry.trace_kind, entry.storage_owner_ref);
  }

  // Package/global init prelude.
  // Applied once with arena-relative (absolute) offsets, byte_base = 0.
  ApplyInitPatches(design_state_, 0, pkg_init_patches_.entries);
  ConstructInitHandles(design_state_, 0, pkg_init_handles_.entries);

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
          .this_ptr = nullptr,
          .instance_id = 0,
          .signal_id_offset = 0,
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
      .entries = package.entries,
      .meta = package.meta,
      .triggers = package.triggers,
      .comb = package.comb,
      .observable_descriptors = package.observable_descriptors,
      .init_patches = package.init_patches,
      .init_handles = package.init_handles,
      .init_params = package.init_params,
      .active = true,
  };
}

void Constructor::AddInstance(
    const char* instance_path, const void* param_data,
    uint32_t param_data_size) {
  CheckNotFinalized("Constructor::AddInstance");
  if (instance_path == nullptr) {
    throw common::InternalError(
        "Constructor::AddInstance", "null instance_path");
  }
  if (!body_.active) {
    throw common::InternalError(
        "Constructor::AddInstance", "no active body (call BeginBody first)");
  }
  if ((param_data == nullptr) != (param_data_size == 0)) {
    throw common::InternalError(
        "Constructor::AddInstance",
        "param_data and param_data_size must be both empty or both present");
  }
  connections_finalized_ = true;

  uint32_t instance_id = next_instance_id_;
  uint32_t signal_id_offset = next_slot_base_;

  uint64_t base_byte_offset = 0;
  void* this_ptr = design_state_.data();
  if (body_.slot_count > 0) {
    if (next_slot_base_ >= slot_byte_offsets_.size()) {
      throw common::InternalError(
          "Constructor::AddInstance",
          std::format(
              "slot base {} exceeds layout oracle size {}", next_slot_base_,
              slot_byte_offsets_.size()));
    }
    base_byte_offset = slot_byte_offsets_[next_slot_base_];
    this_ptr = design_state_.subspan(base_byte_offset).data();
  }

  // Invariant: zero-slot bodies must not carry instance-state init.
  if (body_.slot_count == 0) {
    if (!body_.init_patches.entries.empty() ||
        !body_.init_handles.entries.empty() ||
        !body_.init_params.slots.empty() || param_data_size != 0) {
      throw common::InternalError(
          "Constructor::AddInstance",
          "zero-slot body must not carry instance-state init descriptors "
          "or param payload");
    }
  }

  // H6: Apply body-shaped initialization to this instance's state.
  ApplyInitPatches(design_state_, base_byte_offset, body_.init_patches.entries);
  ConstructInitHandles(
      design_state_, base_byte_offset, body_.init_handles.entries);
  ApplyParamInit(
      design_state_, base_byte_offset, body_.init_params.slots, param_data,
      param_data_size);

  // Capture module_proc_base before staging non-final body processes.
  auto module_proc_base = static_cast<uint32_t>(staged_.size());

  for (const auto& entry : body_.entries) {
    SharedBodyFn body_fn{};
    std::memcpy(&body_fn, &entry.shared_body_fn, sizeof(body_fn));

    staged_.push_back(
        StagedProcess{
            .schema_index = entry.schema_index,
            .body = body_fn,
            .this_ptr = this_ptr,
            .instance_id = instance_id,
            .signal_id_offset = signal_id_offset,
            .is_module = true,
        });
  }

  uint32_t instance_ord = next_module_instance_ordinal_;
  ++next_module_instance_ordinal_;
  auto num_nonfinal_body_procs = static_cast<uint32_t>(body_.entries.size());
  instance_ledger_.push_back(
      InstanceLedgerEntry{
          .owner_ordinal = instance_ord,
          .module_proc_base = module_proc_base,
          .num_processes = num_nonfinal_body_procs,
      });

  // Append metadata entries for all non-final body processes of this instance.
  uint32_t inst_path_off = AppendString(instance_path);
  for (const auto& meta : body_.meta.entries) {
    auto body_pool = std::span(body_.meta.pool, body_.meta.pool_size);
    uint32_t file_off = InternString(body_pool, meta.file_pool_off);
    realized_meta_.words.push_back(inst_path_off);
    realized_meta_.words.push_back(meta.kind_packed);
    realized_meta_.words.push_back(file_off);
    realized_meta_.words.push_back(meta.line);
    realized_meta_.words.push_back(meta.col);
  }

  // Trigger realization for all non-final body processes of this instance.
  // proc_within_body is a dense non-final body-local ordinal.
  if (!body_.triggers.proc_ranges.empty()) {
    if (body_.triggers.proc_ranges.size() != num_nonfinal_body_procs) {
      throw common::InternalError(
          "Constructor::AddInstance",
          std::format(
              "trigger proc_ranges size {} != body process count {}",
              body_.triggers.proc_ranges.size(), num_nonfinal_body_procs));
    }
    if (body_.triggers.proc_groupable.size() != num_nonfinal_body_procs) {
      throw common::InternalError(
          "Constructor::AddInstance",
          std::format(
              "trigger proc_groupable size {} != body process count {}",
              body_.triggers.proc_groupable.size(), num_nonfinal_body_procs));
    }
    if (body_.triggers.proc_shapes.size() != num_nonfinal_body_procs) {
      throw common::InternalError(
          "Constructor::AddInstance",
          std::format(
              "trigger proc_shapes size {} != body process count {}",
              body_.triggers.proc_shapes.size(), num_nonfinal_body_procs));
    }
    for (uint32_t pwb = 0; pwb < num_nonfinal_body_procs; ++pwb) {
      uint32_t proc_idx = module_proc_base + pwb;
      const auto& range = body_.triggers.proc_ranges[pwb];
      uint32_t flags = (body_.triggers.proc_groupable[pwb] != 0)
                           ? process_trigger_abi::kFlagGroupable
                           : 0;
      for (uint32_t t = 0; t < range.count; ++t) {
        const auto& te = body_.triggers.entries[range.start + t];
        // Relocate body-relative slot IDs, pass design-global through.
        uint32_t slot_id = (te.flags & kTriggerTemplateFlagDesignGlobal)
                               ? te.slot_id
                               : te.slot_id + signal_id_offset;
        realized_triggers_.AppendEntry(proc_idx, slot_id, te.edge, flags);
        trigger_provenance_.push_back(
            TriggerProvenanceRecord{
                .domain = TemplateDomain::kModule,
                .owner_ordinal = instance_ord,
                .local_ordinal = pwb,
                .realized_proc_idx = proc_idx,
            });
      }
    }
  }

  // Comb realization for comb kernels of this instance.
  if (!body_.comb.kernels.empty()) {
    for (const auto& kernel : body_.comb.kernels) {
      if (kernel.proc_within_body >= num_nonfinal_body_procs) {
        throw common::InternalError(
            "Constructor::AddInstance",
            std::format(
                "comb kernel proc_within_body {} >= body process count {}",
                kernel.proc_within_body, num_nonfinal_body_procs));
      }
      uint32_t proc_idx = module_proc_base + kernel.proc_within_body;
      uint32_t comb_flags = (kernel.has_self_edge != 0) ? 1U : 0U;
      uint32_t kernel_start_pos =
          realized_comb_.BeginKernel(proc_idx, comb_flags);
      uint32_t trigger_count = 0;
      for (uint32_t t = 0; t < kernel.trigger_count; ++t) {
        const auto& ce = body_.comb.entries[kernel.trigger_start + t];
        uint32_t slot_id = (ce.flags & kCombTemplateFlagDesignGlobal)
                               ? ce.slot_id
                               : ce.slot_id + signal_id_offset;
        realized_comb_.AppendTrigger(slot_id, ce.byte_offset, ce.byte_size);
        ++trigger_count;
      }
      realized_comb_.EndKernel(kernel_start_pos, trigger_count);
      comb_provenance_.push_back(
          CombProvenanceRecord{
              .owner_ordinal = instance_ord,
              .proc_within_body = kernel.proc_within_body,
              .realized_proc_idx = proc_idx,
          });
    }
  }

  // Observable descriptor realization for this instance.
  if (!body_.observable_descriptors.entries.empty()) {
    uint64_t instance_byte_base =
        (body_.slot_count > 0) ? slot_byte_offsets_[next_slot_base_] : 0;

    for (const auto& entry : body_.observable_descriptors.entries) {
      auto realized_offset =
          static_cast<uint32_t>(instance_byte_base + entry.storage_byte_offset);

      uint32_t realized_owner =
          (entry.flags & kObservableFlagOwnerAbsolute) != 0
              ? entry.storage_owner_ref
              : entry.storage_owner_ref + signal_id_offset;

      realized_slot_meta_.AppendSlot(
          realized_offset, entry.total_bytes, entry.storage_kind,
          entry.value_lane_offset, entry.value_lane_bytes,
          entry.unk_lane_offset, entry.unk_lane_bytes, realized_owner);

      std::string_view local_name;
      if (entry.local_name_pool_off > 0 &&
          entry.local_name_pool_off < body_.observable_descriptors.pool_size) {
        local_name =
            &body_.observable_descriptors.pool[entry.local_name_pool_off];
      }
      uint32_t name_off = realized_trace_meta_.AppendHierarchicalName(
          instance_path, local_name);
      realized_trace_meta_.AppendSignal(
          name_off, entry.bit_width, entry.trace_kind, realized_owner);
    }
  }

  // Collect instance path for runtime naming ownership.
  instance_paths_.emplace_back(instance_path);

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

  // Validate metadata-process ordering alignment.
  auto expected_meta_words =
      static_cast<size_t>(num_total) * process_meta_abi::kStride;
  if (realized_meta_.words.size() != expected_meta_words) {
    throw common::InternalError(
        "Constructor::Finalize",
        std::format(
            "metadata word count {} != expected {} (num_total={} * stride={})",
            realized_meta_.words.size(), expected_meta_words, num_total,
            process_meta_abi::kStride));
  }

  // Finalize trigger/comb/slot/trace realized builders.
  realized_triggers_.Finalize();
  realized_comb_.Finalize();
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
  // Instance ledger ordinals are dense and match vector position:
  // instance_ledger_[ordinal].owner_ordinal == ordinal.
  for (uint32_t li = 0; li < instance_ledger_.size(); ++li) {
    if (instance_ledger_[li].owner_ordinal != li) {
      throw common::InternalError(
          "Constructor::Finalize",
          std::format(
              "instance ledger ordinal mismatch: entry {} has "
              "owner_ordinal {}",
              li, instance_ledger_[li].owner_ordinal));
    }
  }

  // Connection triggers: realized_proc_idx must equal the connection
  // ordinal (connections are staged first, so i-th connection has
  // staged_ position i).
  // Module triggers: look up by dense ordinal in instance ledger.
  for (const auto& rec : trigger_provenance_) {
    if (rec.domain == TemplateDomain::kConnection) {
      if (rec.realized_proc_idx != rec.owner_ordinal) {
        throw common::InternalError(
            "Constructor::Finalize",
            std::format(
                "connection trigger proc_idx mismatch: "
                "realized {} != owner_ordinal {}",
                rec.realized_proc_idx, rec.owner_ordinal));
      }
    } else {
      if (rec.owner_ordinal >= instance_ledger_.size()) {
        throw common::InternalError(
            "Constructor::Finalize",
            std::format(
                "module trigger owner_ordinal {} >= ledger size {}",
                rec.owner_ordinal, instance_ledger_.size()));
      }
      const auto& le = instance_ledger_[rec.owner_ordinal];
      uint32_t expected = le.module_proc_base + rec.local_ordinal;
      if (rec.realized_proc_idx != expected) {
        throw common::InternalError(
            "Constructor::Finalize",
            std::format(
                "module trigger proc_idx mismatch: "
                "realized {} != expected {} "
                "(base {} + ordinal {})",
                rec.realized_proc_idx, expected, le.module_proc_base,
                rec.local_ordinal));
      }
    }
  }
  for (const auto& rec : comb_provenance_) {
    if (rec.owner_ordinal >= instance_ledger_.size()) {
      throw common::InternalError(
          "Constructor::Finalize",
          std::format(
              "comb owner_ordinal {} >= ledger size {}", rec.owner_ordinal,
              instance_ledger_.size()));
    }
    const auto& le = instance_ledger_[rec.owner_ordinal];
    uint32_t expected = le.module_proc_base + rec.proc_within_body;
    if (rec.realized_proc_idx != expected) {
      throw common::InternalError(
          "Constructor::Finalize",
          std::format(
              "comb proc_idx mismatch: "
              "realized {} != expected {} "
              "(base {} + pwb {})",
              rec.realized_proc_idx, expected, le.module_proc_base,
              rec.proc_within_body));
    }
  }

  if (num_total == 0) {
    ConstructionResult result;
    result.process_meta = std::move(realized_meta_);
    result.trigger_meta = std::move(realized_triggers_);
    result.comb_meta = std::move(realized_comb_);
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

    const auto& proc = staged_[i];
    if (proc.is_module) {
      header->body = proc.body;
      header->this_ptr = proc.this_ptr;
      header->instance_id = proc.instance_id;
      header->signal_id_offset = proc.signal_id_offset;
    }
  }

  ConstructionResult result;
  result.states = std::move(states);
  result.packed_buffer = packed_buffer;
  result.num_total = num_total;
  result.num_connection = num_connection_;
  result.process_meta = std::move(realized_meta_);
  result.trigger_meta = std::move(realized_triggers_);
  result.comb_meta = std::move(realized_comb_);
  result.slot_meta = std::move(realized_slot_meta_);
  result.trace_signal_meta = std::move(realized_trace_meta_);
  result.instance_paths = std::move(instance_paths_);
  PopulateInstancePathPtrs(result);
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
  if (pool[0] != '\0') {
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
    const lyra::runtime::InitPatchEntry* pkg_init_patches,
    uint32_t num_pkg_init_patches,
    const lyra::runtime::InitHandleEntry* pkg_init_handles,
    uint32_t num_pkg_init_handles) -> void* {
  ValidateMetaAbiInputs(
      conn_meta_entries, num_conn_meta, conn_meta_pool, conn_meta_pool_size,
      "LyraConstructorCreate");
  ValidateObservableDescriptorTemplate(
      std::span(pkg_obs_entries, num_pkg_obs), pkg_obs_pool, pkg_obs_pool_size,
      true, "LyraConstructorCreate");
  ValidateAbiArray(
      pkg_init_patches, num_pkg_init_patches, "pkg_init_patches",
      "LyraConstructorCreate");
  ValidateAbiArray(
      pkg_init_handles, num_pkg_init_handles, "pkg_init_handles",
      "LyraConstructorCreate");
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
  lyra::runtime::InitPatchView pkg_patches{
      .entries = std::span(pkg_init_patches, num_pkg_init_patches),
  };
  lyra::runtime::InitHandleView pkg_handles{
      .entries = std::span(pkg_init_handles, num_pkg_init_handles),
  };
  auto ctor = std::make_unique<lyra::runtime::Constructor>(
      std::span(schemas, num_schemas), std::span(slot_byte_offsets, num_slots),
      num_package_slots,
      std::span(static_cast<std::byte*>(design_state), design_state_size),
      conn_meta, conn_triggers, pkg_obs, pkg_patches, pkg_handles);
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
    const lyra::runtime::InitPatchEntry* init_patches,
    uint32_t num_init_patches,
    const lyra::runtime::InitHandleEntry* init_handles,
    uint32_t num_init_handles,
    const lyra::runtime::ParamInitSlotEntry* init_param_slots,
    uint32_t num_init_param_slots) {
  ValidateHandle(ctor, "LyraConstructorBeginBody");
  ValidateHandle(desc, "LyraConstructorBeginBody");
  ValidateMetaAbiInputs(
      meta_entries, num_meta, meta_pool, meta_pool_size,
      "LyraConstructorBeginBody");
  ValidateObservableDescriptorTemplate(
      std::span(obs_entries, num_obs), obs_pool, obs_pool_size, false,
      "LyraConstructorBeginBody");
  ValidateAbiArray(
      init_patches, num_init_patches, "init_patches",
      "LyraConstructorBeginBody");
  ValidateAbiArray(
      init_handles, num_init_handles, "init_handles",
      "LyraConstructorBeginBody");
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
      .init_patches =
          lyra::runtime::InitPatchView{
              .entries = std::span(init_patches, num_init_patches),
          },
      .init_handles =
          lyra::runtime::InitHandleView{
              .entries = std::span(init_handles, num_init_handles),
          },
      .init_params =
          lyra::runtime::ParamInitView{
              .slots = std::span(init_param_slots, num_init_param_slots),
          },
  };
  static_cast<lyra::runtime::Constructor*>(ctor)->BeginBody(package);
}

void LyraConstructorAddInstance(
    void* ctor, const char* instance_path, const void* param_data,
    uint32_t param_data_size) {
  ValidateHandle(ctor, "LyraConstructorAddInstance");
  ValidateAbiArray(
      param_data, param_data_size, "param_data", "LyraConstructorAddInstance");
  static_cast<lyra::runtime::Constructor*>(ctor)->AddInstance(
      instance_path, param_data, param_data_size);
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

auto LyraConstructionResultGetCombWords(void* result_raw) -> const uint32_t* {
  ValidateHandle(result_raw, "LyraConstructionResultGetCombWords");
  auto& meta =
      static_cast<lyra::runtime::ConstructionResult*>(result_raw)->comb_meta;
  if (meta.IsEmpty()) return nullptr;
  return meta.words.data();
}

auto LyraConstructionResultGetCombWordCount(void* result_raw) -> uint32_t {
  ValidateHandle(result_raw, "LyraConstructionResultGetCombWordCount");
  auto& meta =
      static_cast<lyra::runtime::ConstructionResult*>(result_raw)->comb_meta;
  if (meta.IsEmpty()) return 0;
  return static_cast<uint32_t>(meta.words.size());
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

void LyraConstructionResultDestroy(void* result_raw) {
  if (result_raw == nullptr) return;
  std::unique_ptr<lyra::runtime::ConstructionResult> result(
      static_cast<lyra::runtime::ConstructionResult*>(result_raw));
}
