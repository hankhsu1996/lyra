#pragma once

#include <slang/ast/Compilation.h>

#include "lyra/common/module_identity.hpp"
#include "lyra/hir/design.hpp"
#include "lyra/lowering/ast_to_hir/port_binding.hpp"
#include "lyra/mir/instance.hpp"

namespace lyra::lowering::ast_to_hir {

struct Context;
class SymbolRegistrar;

struct DesignLoweringResult {
  hir::Design design;
  DesignBindingPlan binding_plan;
  common::SpecializationMap specialization_map;
  mir::InstanceTable instance_table;  // For %m support
};

auto LowerDesign(
    slang::ast::Compilation& compilation, SymbolRegistrar& registrar,
    Context* ctx) -> DesignLoweringResult;

}  // namespace lyra::lowering::ast_to_hir
