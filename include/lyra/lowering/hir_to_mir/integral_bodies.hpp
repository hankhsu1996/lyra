#pragma once

#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/code_source.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::lowering::hir_to_mir {

// The body answering an integral operator (LRM 11.4) of a type laid out as
// more words than a machine operates on at once, built at the type's width
// over the words its value lies in, a word at a time.
[[nodiscard]] auto BuildIntegralOperationBody(
    const mir::CompilationUnit& unit, const mir::TypeOperationSource& source)
    -> mir::CallableCode;

}  // namespace lyra::lowering::hir_to_mir
