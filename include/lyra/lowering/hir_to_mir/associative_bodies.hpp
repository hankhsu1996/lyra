#pragma once

#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/code_source.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::lowering::hir_to_mir {

// The body answering `source` at an associative array (LRM 7.8, 7.9), built
// from the declaration MIR states for it: an item of a library trait the
// array's type implements, one of its own methods, or an item of a language
// trait it implements. Each is a program over the element type's and the index
// type's own operations and the library's core, which keeps the entries'
// storage and the order the language fixes for the index.
[[nodiscard]] auto BuildAssociativeOperationBody(
    const mir::CompilationUnit& unit, const mir::TypeOperationSource& source)
    -> mir::CallableCode;

}  // namespace lyra::lowering::hir_to_mir
