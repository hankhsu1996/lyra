#pragma once

#include <cstddef>
#include <cstdint>
#include <type_traits>

namespace lyra::runtime {

// Per-process entry within a body realization descriptor.
//
// Associates one body-local process with its compiled artifacts:
// the shared body function and the state schema used for frame allocation.
struct BodyProcessEntry {
  void* shared_body_fn = nullptr;
  uint32_t schema_index = 0;
  uint32_t pad = 0;
};

static_assert(sizeof(BodyProcessEntry) == 16);
static_assert(offsetof(BodyProcessEntry, shared_body_fn) == 0);
static_assert(offsetof(BodyProcessEntry, schema_index) == 8);
static_assert(std::is_trivially_copyable_v<BodyProcessEntry>);
static_assert(std::is_standard_layout_v<BodyProcessEntry>);

// Body-local constructor-side definition artifact.
//
// One per compiled module body. Describes the process repertoire that the
// runtime constructor uses to realize instances of this body.
//
// This is the permanent seed object for constructor-time realization.
// Currently populated: process/schema subset, process metadata templates
// (ProcessMetaTemplateEntry), and trigger/comb templates
// (TriggerTemplateEntry, CombTemplateEntry).
//
// Separation principle: this descriptor holds body-local
// definition/repertoire only. Instance count, instance ordering, and
// design-level topology are never part of the descriptor. Those belong
// in the emitted constructor function's control flow.
//
// Codegen emits the header and process entries as separate contiguous
// LLVM globals. The runtime constructor receives them as two arguments:
// one BodyRealizationDesc header and one span of BodyProcessEntry.
struct BodyRealizationDesc {
  // Body-local module-process repertoire count (non-final processes).
  uint32_t num_processes = 0;
  // Body's slot count, used by the constructor to advance the
  // running slot-base counter per instance.
  uint32_t slot_count = 0;
};

static_assert(sizeof(BodyRealizationDesc) == 8);
static_assert(offsetof(BodyRealizationDesc, num_processes) == 0);
static_assert(offsetof(BodyRealizationDesc, slot_count) == 4);
static_assert(std::is_trivially_copyable_v<BodyRealizationDesc>);
static_assert(std::is_standard_layout_v<BodyRealizationDesc>);

// Design-level constructor-side definition artifact for connection processes.
//
// One per connection process. Associates a connection function with its
// state schema for frame allocation. Connection and module processes are
// both realized through the same constructor object.
struct ConnectionRealizationDesc {
  void* fn_ptr = nullptr;
  uint32_t schema_index = 0;
  uint32_t pad = 0;
};

static_assert(sizeof(ConnectionRealizationDesc) == 16);
static_assert(offsetof(ConnectionRealizationDesc, fn_ptr) == 0);
static_assert(offsetof(ConnectionRealizationDesc, schema_index) == 8);
static_assert(std::is_trivially_copyable_v<ConnectionRealizationDesc>);
static_assert(std::is_standard_layout_v<ConnectionRealizationDesc>);

// Per-trigger-fact body-shaped template entry.
//
// One per trigger fact per body-local process. Used for both body and
// connection process trigger templates via OwnedTriggerTemplate.
//
// The constructor combines template entries with per-instance slot-base
// relocation and process-index assignment to produce the realized
// trigger word table.
struct TriggerTemplateEntry {
  // Template-domain slot identifier.
  // Body templates: body-relative slot index when
  // kTriggerTemplateFlagDesignGlobal is clear, design-global slot id when set.
  // Constructor applies slot-base relocation only for entries without the
  // design-global flag. Connection templates: always design-global (flag always
  // set).
  uint32_t slot_id = 0;
  // common::EdgeKind packed as u32.
  uint32_t edge = 0;
  // Template-level flags.
  // kTriggerTemplateFlagHasObservedPlace: consumed at template extraction
  // time to pre-compute per-process groupability (stored in
  // OwnedTriggerTemplate::proc_groupable).
  // kTriggerTemplateFlagDesignGlobal: consumed at constructor realization
  // time to decide whether to apply slot-base relocation.
  // Neither flag appears in the realized trigger word table. The realized
  // flags word contains only process_trigger_abi::kFlagGroupable (bit 0),
  // derived from proc_groupable.
  uint32_t flags = 0;
};

static_assert(sizeof(TriggerTemplateEntry) == 12);
static_assert(offsetof(TriggerTemplateEntry, slot_id) == 0);
static_assert(offsetof(TriggerTemplateEntry, edge) == 4);
static_assert(offsetof(TriggerTemplateEntry, flags) == 8);
static_assert(std::is_trivially_copyable_v<TriggerTemplateEntry>);
static_assert(std::is_standard_layout_v<TriggerTemplateEntry>);

inline constexpr uint32_t kTriggerTemplateFlagHasObservedPlace = 1U << 0;
// Slot ID is design-global (already absolute). When set, constructor
// does not apply slot-base relocation for this entry. When clear,
// slot_id is body-relative and constructor adds signal_id_offset.
inline constexpr uint32_t kTriggerTemplateFlagDesignGlobal = 1U << 1;

// Named values for OwnedTriggerTemplate::proc_groupable entries.
// Restricts the uint8_t transport type to a binary contract.
inline constexpr uint8_t kProcNotGroupable = 0;
inline constexpr uint8_t kProcGroupable = 1;

// Per-comb-trigger body-shaped template entry.
//
// One per comb trigger observation per body-local comb kernel.
// Body-relative slot IDs.
struct CombTemplateEntry {
  // Slot identifier. Body-relative when kCombTemplateFlagDesignGlobal
  // is clear, design-global when set. Constructor applies slot-base
  // relocation only for entries without the design-global flag.
  uint32_t slot_id = 0;
  // Observation byte offset (0 if full-slot).
  uint32_t byte_offset = 0;
  // Observation byte size (0 = full-slot observation).
  uint32_t byte_size = 0;
  // Template-level flags.
  uint32_t flags = 0;
};

static_assert(sizeof(CombTemplateEntry) == 16);
static_assert(offsetof(CombTemplateEntry, slot_id) == 0);
static_assert(offsetof(CombTemplateEntry, byte_offset) == 4);
static_assert(offsetof(CombTemplateEntry, byte_size) == 8);
static_assert(offsetof(CombTemplateEntry, flags) == 12);
static_assert(std::is_trivially_copyable_v<CombTemplateEntry>);
static_assert(std::is_standard_layout_v<CombTemplateEntry>);

inline constexpr uint32_t kCombTemplateFlagDesignGlobal = 1U << 0;

// Per-comb-kernel descriptor within a body template.
struct CombKernelDesc {
  uint32_t proc_within_body = 0;
  uint32_t trigger_start = 0;
  uint32_t trigger_count = 0;
  uint8_t has_self_edge = 0;
  uint8_t pad0 = 0;
  uint8_t pad1 = 0;
  uint8_t pad2 = 0;
};

static_assert(sizeof(CombKernelDesc) == 16);
static_assert(offsetof(CombKernelDesc, proc_within_body) == 0);
static_assert(offsetof(CombKernelDesc, trigger_start) == 4);
static_assert(offsetof(CombKernelDesc, trigger_count) == 8);
static_assert(offsetof(CombKernelDesc, has_self_edge) == 12);
static_assert(std::is_trivially_copyable_v<CombKernelDesc>);
static_assert(std::is_standard_layout_v<CombKernelDesc>);

// Per-process metadata template entry.
//
// Unified type for both body-local and connection process metadata.
// One entry per body-local process (indexed by proc_within_body) or per
// connection process. Each entry carries the body-shaped metadata fields
// that are identical across all instances of the same body.
//
// The constructor combines template entries with per-instance paths to
// produce the final realized process metadata table.
struct ProcessMetaTemplateEntry {
  // runtime::ProcessKind packed into low 8 bits.
  uint32_t kind_packed = 0;
  // Offset into this template's string pool for the source file path.
  uint32_t file_pool_off = 0;
  // 1-based source line number (0 = unknown).
  uint32_t line = 0;
  // 1-based source column number (0 = unknown).
  uint32_t col = 0;
};

static_assert(sizeof(ProcessMetaTemplateEntry) == 16);
static_assert(offsetof(ProcessMetaTemplateEntry, kind_packed) == 0);
static_assert(offsetof(ProcessMetaTemplateEntry, file_pool_off) == 4);
static_assert(offsetof(ProcessMetaTemplateEntry, line) == 8);
static_assert(offsetof(ProcessMetaTemplateEntry, col) == 12);
static_assert(std::is_trivially_copyable_v<ProcessMetaTemplateEntry>);
static_assert(std::is_standard_layout_v<ProcessMetaTemplateEntry>);

}  // namespace lyra::runtime
