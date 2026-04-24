#pragma once

#include <slang/ast/symbols/InstanceSymbols.h>

#include "lyra/hir/module_unit.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerModule(const slang::ast::InstanceBodySymbol& body) -> hir::ModuleUnit;

}  // namespace lyra::lowering::ast_to_hir
