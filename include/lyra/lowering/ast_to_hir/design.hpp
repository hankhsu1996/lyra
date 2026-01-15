#pragma once

#include <slang/ast/Compilation.h>

#include "lyra/hir/design.hpp"

namespace lyra::lowering::ast_to_hir {

struct Context;
class SymbolRegistrar;

auto LowerDesign(
    slang::ast::Compilation& compilation, SymbolRegistrar& registrar,
    Context* ctx) -> hir::Design;

}  // namespace lyra::lowering::ast_to_hir
