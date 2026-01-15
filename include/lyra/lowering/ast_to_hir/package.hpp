#pragma once

#include <slang/ast/symbols/CompilationUnitSymbols.h>

#include "lyra/hir/package.hpp"

namespace lyra::lowering::ast_to_hir {

struct Context;
class SymbolRegistrar;

auto LowerPackage(
    const slang::ast::PackageSymbol& package, SymbolRegistrar& registrar,
    Context* ctx) -> hir::Package;

}  // namespace lyra::lowering::ast_to_hir
