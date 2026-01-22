#pragma once

#include <cstdint>
#include <variant>
#include <vector>

#include "lyra/common/type.hpp"
#include "lyra/mir/operand.hpp"

namespace lyra::mir {

struct PlaceRoot {
  enum class Kind {
    kLocal,   // function/process local storage (vars, params)
    kTemp,    // compiler-generated local storage
    kDesign,  // design/runtime storage (nets, ports, hierarchy)
  };

  Kind kind;
  int id;       // opaque handle to storage table
  TypeId type;  // type of the value stored at this root
};

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
};

struct Place {
  PlaceRoot root;
  std::vector<Projection> projections;
};

// Helper to check if a projection is a BitRange
inline auto IsBitRange(const Projection& proj) -> bool {
  return std::holds_alternative<BitRangeProjection>(proj.info);
}

}  // namespace lyra::mir
