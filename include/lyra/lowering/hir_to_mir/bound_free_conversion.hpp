#pragma once

#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// The body a unit states for `type`, an array holding a bounded queue below its
// outermost dimension, taking a value of it into the form of it with no queue
// bounded (LRM 6.22.2 e, 7.6, 7.10.5). Every type equivalent to `type` has that
// form, so a value is assigned across two of them by passing through it. The
// body walks the entries of the value it is handed and builds each element
// afresh at the element type of what it answers.
[[nodiscard]] auto BuildToBoundFreeBody(
    const mir::CompilationUnit& unit, mir::TypeId type) -> mir::CallableCode;

// The same, building a value of that form as one of `type`: a queue first keeps
// what its own bound admits, so what it discards is never built, and an
// associative array keeps the value its absent indexes read (LRM 7.9.11).
[[nodiscard]] auto BuildFromBoundFreeBody(
    const mir::CompilationUnit& unit, mir::TypeId type) -> mir::CallableCode;

}  // namespace lyra::lowering::hir_to_mir
