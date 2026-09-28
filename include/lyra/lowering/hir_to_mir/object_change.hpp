#pragma once

#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

// The object a property is reached on, as the address the entries reporting a
// change to it take: the root every object shares, for the object a class
// handle names (LRM 8.3), and a method's own receiver, which converts to that
// root.
[[nodiscard]] auto ObjectRootOf(
    mir::CompilationUnit& unit, mir::Block& block, mir::ExprId receiver)
    -> mir::ExprId;

// The event source of the object `object` addresses, which a wait reaching the
// object subscribes to: one per object, covering every property (LRM 9.4.2).
[[nodiscard]] auto ObjectEventSourceOf(
    mir::CompilationUnit& unit, mir::Block& block, mir::ExprId object)
    -> mir::ExprId;

// `place`, a property of the object `object` addresses, reached through a write
// opened on that object for it. The write lasts as long as the full-expression
// that writes the place, and ending it tells the object (LRM 9.4.2).
[[nodiscard]] auto PropertyWrittenThrough(
    mir::CompilationUnit& unit, mir::Block& block, mir::ExprId object,
    mir::ExprId place) -> mir::ExprId;

// A reference to `place`, a property of the object `object` addresses (LRM
// 13.5.2). A write through it tells the object as it lands (LRM 9.4.2), which
// no write held open around the call that is lent it could do.
[[nodiscard]] auto PropertyReferred(
    mir::CompilationUnit& unit, mir::Block& block, mir::ExprId object,
    mir::ExprId place) -> mir::ExprId;

}  // namespace lyra::lowering::hir_to_mir
