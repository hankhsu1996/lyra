#pragma once

#include <slang/ast/symbols/InstanceSymbols.h>

#include "lyra/hir/module.hpp"

namespace lyra::lowering::ast_to_hir {

struct Context;
class SymbolRegistrar;

auto LowerModule(
    const slang::ast::InstanceSymbol& instance, SymbolRegistrar& registrar,
    Context* ctx) -> hir::Module;

}  // namespace lyra::lowering::ast_to_hir
