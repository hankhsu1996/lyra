#pragma once

#include <vector>

#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// The bodies a unit states for `type`, an array holding a bounded queue below
// its outermost dimension: a value of it taken into the form of it with no
// queue bounded, and a value of that form built as one of `type` (LRM 6.22.2
// e, 7.6, 7.10.5). Every type equivalent to `type` has that form, so a value is
// assigned across two of them by passing through it. Each body walks the
// entries of the value it is handed and builds each element afresh at the
// element type of what it answers; out of the form, a queue first keeps what
// its own bound admits, so what it discards is never built, and an associative
// array keeps the value its absent indexes read (LRM 7.9.11).
[[nodiscard]] auto BoundFreeConversionsOf(
    UnitLowerer& lowerer, mir::TypeId type) -> std::vector<mir::TypeOperation>;

}  // namespace lyra::lowering::hir_to_mir
