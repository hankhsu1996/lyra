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

// BitSlice: packed array element access (bit-range within an integral)
// Must be the final projection in a place.
// Bounds/direction are obtained from array_type at runtime.
struct BitSliceProjection {
  Operand index;        // Element index (may be dynamic)
  TypeId array_type;    // Packed array type (has bounds/direction)
  TypeId element_type;  // Type of the extracted element
};

using ProjectionInfo = std::variant<
    FieldProjection, IndexProjection, SliceProjection, DerefProjection,
    BitSliceProjection>;

struct Projection {
  ProjectionInfo info;
};

struct Place {
  PlaceRoot root;
  std::vector<Projection> projections;
};

// Helper to check if a projection is a BitSlice
inline auto IsBitSlice(const Projection& proj) -> bool {
  return std::holds_alternative<BitSliceProjection>(proj.info);
}

}  // namespace lyra::mir
