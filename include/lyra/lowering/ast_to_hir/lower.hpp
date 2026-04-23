#pragma once

#include <vector>

#include <slang/ast/Compilation.h>

#include "lyra/hir/module_unit.hpp"

namespace lyra::lowering::ast_to_hir {

// Lower an elaborated slang compilation into independent HIR compilation
// units, one per distinct top-level specialization body. The compilation's
// instance/elaboration identity is consumed only as discovery input; no
// instance identity survives into the returned units.
auto LowerCompilation(slang::ast::Compilation& compilation)
    -> std::vector<hir::ModuleUnit>;

}  // namespace lyra::lowering::ast_to_hir
