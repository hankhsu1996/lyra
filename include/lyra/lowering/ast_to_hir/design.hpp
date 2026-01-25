#pragma once

#include <slang/ast/Compilation.h>

#include "lyra/hir/design.hpp"
#include "lyra/lowering/ast_to_hir/port_binding.hpp"

namespace lyra::lowering::ast_to_hir {

struct Context;
class SymbolRegistrar;

struct DesignLoweringResult {
  hir::Design design;
  DesignBindingPlan binding_plan;
};

auto LowerDesign(
    slang::ast::Compilation& compilation, SymbolRegistrar& registrar,
    Context* ctx) -> DesignLoweringResult;

}  // namespace lyra::lowering::ast_to_hir
