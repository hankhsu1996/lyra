#pragma once

#include <cstdint>
#include <variant>
#include <vector>

#include "lyra/common/origin_id.hpp"
#include "lyra/common/type.hpp"
#include "lyra/mir/operand.hpp"

namespace lyra::mir {

struct PlaceRoot {
  enum class Kind {
    kLocal,         // function/process local storage (vars, params)
    kTemp,          // compiler-generated local storage
    kModuleSlot,    // body-local storage (0-based within ModuleBody)
    kDesignGlobal,  // design-global storage (packages, design-level processes)
    kObjectLocal,   // cross-instance body-local storage (object_index, id)
    // Narrow migration-only representation:
    // valid only for mutation-target plumbing of synthesized
    // expression-connection writes. Must not be produced by parsing,
    // lowering of ordinary expressions, or trigger analysis.
    kBoundChildDest,  // write-only child destination via pre-bound frame
                      // context
  };

  Kind kind;
  int id;                     // opaque handle to storage table
  TypeId type;                // type of the value stored at this root
  uint32_t object_index = 0;  // owning object index (kObjectLocal only)
};

// True for place roots that represent process-local storage (need prologue
// allocas). False for external storage (module slots, design globals).
// Shared predicate used by both PlaceCollector and layout place collection.
[[nodiscard]] inline auto IsProcessLocalRoot(PlaceRoot::Kind kind) -> bool {
  switch (kind) {
    case PlaceRoot::Kind::kLocal:
    case PlaceRoot::Kind::kTemp:
      return true;
    case PlaceRoot::Kind::kModuleSlot:
    case PlaceRoot::Kind::kDesignGlobal:
    case PlaceRoot::Kind::kObjectLocal:
    case PlaceRoot::Kind::kBoundChildDest:
      return false;
  }
  return false;
}

// True for place roots that hold a readable value (variables, slots).
// False for compiler-generated write-only staging temps.
// kBoundChildDest is write-only (expression connection child destination).
[[nodiscard]] inline auto IsReadableRoot(PlaceRoot::Kind kind) -> bool {
  switch (kind) {
    case PlaceRoot::Kind::kLocal:
    case PlaceRoot::Kind::kModuleSlot:
    case PlaceRoot::Kind::kDesignGlobal:
    case PlaceRoot::Kind::kObjectLocal:
      return true;
    case PlaceRoot::Kind::kTemp:
    case PlaceRoot::Kind::kBoundChildDest:
      return false;
  }
  return false;
}

// Projection kinds - each has its own info struct

struct FieldProjection {
  int field_index;
};

struct IndexProjection {
  Operand index;
};

struct SliceProjection {
  // For bit/range select on flat integral (future)
  Operand start;
  int width;
};

struct DerefProjection {};

// BitRange: bit-range access within a packed value.
// Address-only projection: carries only the addressing info.
// All validity checking and OOB handling is done in lowering.
struct BitRangeProjection {
  Operand bit_offset;   // Bit offset from base (dynamic expression)
  uint32_t width = 0;   // Number of bits to extract (static)
  TypeId element_type;  // Type of the extracted element

  // Guaranteed power-of-two alignment of bit_offset in bits. The offset
  // is provably always a multiple of this value at all runtime evaluations.
  // Must be a power of two; 1 means no alignment guarantee.
  //
  // Set at MIR construction time by lowering, based on the semantic
  // origin of the projection:
  //
  //   - Packed element access: alignment = element_width
  //   - Indexed part-select: alignment from the index expression's
  //     known scale factor and the subtracted constant
  //   - Constant offset: handled by the backend's constant check
  //   - Otherwise: 1 (no alignment guarantee)
  //
  // The backend uses this to classify byte-addressable subviews:
  // when guaranteed_alignment_bits >= 8 AND width % 8 == 0, the
  // subview can use localized byte-addressed access instead of
  // full-width bit manipulation.
  uint32_t guaranteed_alignment_bits = 1;
};

// UnionMember: access a union member by index.
// Unlike struct field access, this is storage reinterpretation, not selection.
// Offset is always 0; only the view type changes.
struct UnionMemberProjection {
  uint32_t member_index = 0;
};

using ProjectionInfo = std::variant<
    FieldProjection, IndexProjection, SliceProjection, DerefProjection,
    BitRangeProjection, UnionMemberProjection>;

struct Projection {
  ProjectionInfo info;
  common::OriginId origin = common::OriginId::Invalid();
};

struct Place {
  PlaceRoot root;
  std::vector<Projection> projections;
};

// Helper to check if a projection is a BitRange
inline auto IsBitRange(const Projection& proj) -> bool {
  return std::holds_alternative<BitRangeProjection>(proj.info);
}

// User-facing description of a projection kind
inline auto DescribeProjection(const ProjectionInfo& info) -> const char* {
  return std::visit(
      [](const auto& p) -> const char* {
        using T = std::decay_t<decltype(p)>;
        if constexpr (std::is_same_v<T, FieldProjection>) {
          return "struct field access";
        } else if constexpr (std::is_same_v<T, IndexProjection>) {
          return "array indexing";
        } else if constexpr (std::is_same_v<T, SliceProjection>) {
          return "bit slice";
        } else if constexpr (std::is_same_v<T, DerefProjection>) {
          return "pointer dereference";
        } else if constexpr (std::is_same_v<T, BitRangeProjection>) {
          return "part-select";
        } else if constexpr (std::is_same_v<T, UnionMemberProjection>) {
          return "union member access";
        }
      },
      info);
}

}  // namespace lyra::mir
