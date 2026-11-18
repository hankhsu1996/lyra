#pragma once

#include <slang/ast/SemanticFacts.h>

#include "lyra/support/random_qualifier.hpp"

namespace lyra::lowering::ast_to_hir {

// How the source declares a class property or a structure member random (LRM
// 18.4).
[[nodiscard]] auto DeclaredRandomQualifier(slang::ast::RandMode mode)
    -> support::RandomQualifier;

}  // namespace lyra::lowering::ast_to_hir
