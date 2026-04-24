#pragma once

#include <vector>

#include "lyra/common/origin_id.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/statement.hpp"

namespace lyra::lowering::hir_to_mir {

// Synthesize MIR Assign statements that explicitly default-initialize a
// place according to SystemVerilog semantics (LRM 6.7).
//
// This is MIR prologue synthesis, not LLVM codegen. It produces ordinary
// Assign statements using constant operands and recursive place projection.
// The backend lowers these as regular assignments with no special init logic.
//
// Default values by type kind:
//   - 4-state integral/packed/enum: X (value=0, unknown=semantic_mask)
//   - 2-state integral/packed/enum: 0
//   - real/shortreal: 0.0
//   - string/chandle/dynarray/queue/assoc: null (zero-bits representation)
//   - unpacked struct: recursively per field
//   - unpacked array: recursively per element (constant index projection)
//   - unpacked union: zero-fill via first member
//
// Does not encode codegen policy (loop vs unroll, memset, patches).
// That remains a backend optimization concern.
// offset_type: a 32-bit unsigned 2-state TypeId for array index constants.
// Pass the same type used by other lowering paths (BuiltinTypes::offset_type).
void AppendDefaultInitStatements(
    mir::PlaceId place, TypeId type, mir::Arena& arena, const TypeArena& types,
    TypeId offset_type, common::OriginId origin,
    std::vector<mir::Statement>& out);

}  // namespace lyra::lowering::hir_to_mir
