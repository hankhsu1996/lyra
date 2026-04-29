#pragma once

#include <slang/ast/symbols/InstanceSymbols.h>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/module_unit.hpp"
#include "lyra/lowering/ast_to_hir/facts.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerModule(
    const UnitLoweringFacts& unit_facts,
    const slang::ast::InstanceBodySymbol& body)
    -> diag::Result<hir::ModuleUnit>;

}  // namespace lyra::lowering::ast_to_hir
