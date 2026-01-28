#pragma once

#include <variant>
#include <vector>

#include "lyra/common/source_span.hpp"
#include "lyra/common/symbol_types.hpp"
#include "lyra/common/type.hpp"
#include "lyra/hir/fwd.hpp"

namespace lyra::hir {

// Unpacked member access (struct.field)
struct MemberProjection {
  FieldId field;  // TypeArena-owned identity
};

// Unpacked element access (array[i], queue[i], dynarray[i])
struct IndexProjection {
  ExpressionId index;
};

// Union member reinterpretation
struct UnionMemberProjection {
  FieldId member;  // TypeArena-owned identity
};

// Packed select kinds
enum class PackedSelectKind {
  kConstantRange,   // [left:right]
  kBitSelect,       // [i]
  kPartSelectUp,    // [i +: width]
  kPartSelectDown,  // [i -: width]
};

// Packed bit selection projection
struct PackedSelectProjection {
  PackedSelectKind kind;
  // kConstantRange: left and right bounds (compile-time)
  // kBitSelect: index expression
  // kPartSelect*: index expression and static width
  std::variant<
      std::pair<int32_t, int32_t>,       // kConstantRange: (left, right)
      ExpressionId,                      // kBitSelect: index
      std::pair<ExpressionId, uint32_t>  // kPartSelect*: (index, width)
      >
      params;
};

// Packed field access (packed_struct.field)
struct PackedFieldProjection {
  FieldId field;            // TypeArena-owned identity
  uint32_t bit_offset = 0;  // Pre-computed from type
  uint32_t bit_width = 0;   // Pre-computed from type
};

using Projection = std::variant<
    MemberProjection, IndexProjection, UnionMemberProjection,
    PackedSelectProjection, PackedFieldProjection>;

// Canonical representation of an assignment target.
// Provides:
// 1. Root identity: SymbolId - the base ValueSymbol being written
// 2. Write path: sequence of projections from root to accessed element
//
// This is the single source of truth for LHS structure in assignments.
// Gates query this contract rather than re-walking expression trees.
struct AssignTarget {
  SymbolId root_symbol;          // Base ValueSymbol (variable/net)
  TypeId root_type;              // Type of root
  std::vector<Projection> path;  // Projections from root
  TypeId result_type;            // Final type after projections
  SourceSpan span;               // Source location for diagnostics
};

}  // namespace lyra::hir
