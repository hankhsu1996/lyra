#pragma once

#include <slang/ast/Compilation.h>

#include "lyra/common/body_timescale.hpp"
#include "lyra/common/child_coord_map.hpp"
#include "lyra/common/hierarchy_node.hpp"
#include "lyra/common/module_identity.hpp"
#include "lyra/hir/design.hpp"
#include "lyra/hir/dpi.hpp"
#include "lyra/hir/module_body.hpp"
#include "lyra/lowering/ast_to_hir/port_binding.hpp"
#include "lyra/mir/instance.hpp"

namespace lyra::lowering::ast_to_hir {

struct Context;
class SymbolRegistrar;

struct DesignLoweringResult {
  hir::Design design;
  // Per-specialization-group module bodies. Indexed by ModuleBodyId.
  // Sibling to hir::Design because bodies are per-CU semantic output,
  // not whole-design shell state.
  std::vector<hir::ModuleBody> module_bodies;
};

struct DesignCompositionMetadata {
  DesignBindingPlan binding_plan;
  common::SpecializationMap specialization_map;
  mir::InstanceTable instance_table;
  std::vector<common::BodyTimeScale> body_timescales;
  // Per-definition child instance name -> repertoire coord mapping.
  // Built from DefinitionRepertoireDesc during AST-to-HIR. Consumed by
  // design_lower.cpp for durable child-site identity.
  common::ChildCoordMap child_coord_map;
  // Full scope hierarchy including generate scopes. Built from slang AST
  // during instance collection. Threaded through to construction program.
  std::vector<common::HierarchyNode> hierarchy_nodes;
  // Preclassified DPI export signatures, keyed by export symbol.
  // Produced at AST-to-HIR time alongside lean DpiExportDecl records and
  // consumed at HIR-to-MIR time to build canonical mir::DpiSignature entries.
  hir::DpiExportSignatureCache dpi_export_signatures;
};

struct DesignLoweringOutput {
  DesignLoweringResult hir;
  DesignCompositionMetadata composition;
};

auto LowerDesign(
    slang::ast::Compilation& compilation, SymbolRegistrar& registrar,
    Context* ctx) -> DesignLoweringOutput;

}  // namespace lyra::lowering::ast_to_hir
