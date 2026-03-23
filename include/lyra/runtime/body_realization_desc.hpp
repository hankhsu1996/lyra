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
// Currently populated: process/schema subset and process metadata
// templates (ProcessMetaTemplateEntry). Future extensions: trigger/comb
// templates and slot/trace/path templates.
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
