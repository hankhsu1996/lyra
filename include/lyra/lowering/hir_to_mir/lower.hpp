#pragma once

#include <cstdint>
#include <memory>

#include "lyra/common/body_timescale.hpp"
#include "lyra/common/child_coord_map.hpp"
#include "lyra/common/constant_arena.hpp"
#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/hierarchy_node.hpp"
#include "lyra/common/module_identity.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/dpi.hpp"
#include "lyra/hir/module.hpp"
#include "lyra/hir/module_body.hpp"
#include "lyra/hir/package.hpp"
#include "lyra/lowering/ast_to_hir/port_binding.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/origin_map.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/connection_endpoint.hpp"
#include "lyra/mir/construction_input.hpp"
#include "lyra/mir/design.hpp"
#include "lyra/mir/instance.hpp"

namespace lyra::lowering::hir_to_mir {

struct LoweringInput {
  // Per-instance HIR module records in stable elaboration order.
  // Must be non-null.
  const std::vector<hir::Module>* modules = nullptr;
  // Per-compilation-unit HIR package records. Must be non-null.
  const std::vector<hir::Package>* packages = nullptr;
  // Per-specialization-group HIR module bodies. Must be non-null.
  const std::vector<hir::ModuleBody>* module_bodies = nullptr;
  // Design-global HIR arena by default; overridden to package-local arena
  // before package lowering (design_lower.cpp, hir_to_mir/package.cpp) and
  // to body-local arena before body lowering begins (design_lower.cpp).
  const hir::Arena* hir_arena = nullptr;
  TypeArena* type_arena = nullptr;
  // Active constant domain: design-global by default, overridden to
  // package-local before package lowering and to body-local before body
  // lowering begins.
  const ConstantArena* active_constant_arena = nullptr;
  const SymbolTable* symbol_table = nullptr;
  BuiltinTypes builtin_types;
  const ast_to_hir::DesignBindingPlan* binding_plan = nullptr;
  int8_t global_precision_power =
      -9;  // Finest timeprecision across all modules
  const mir::InstanceTable* instance_table = nullptr;
  const common::SpecializationMap* specialization_map = nullptr;
  // Per-definition child instance name -> repertoire coord mapping.
  // Used by design_lower for durable child-site identity.
  const common::ChildCoordMap* child_coord_map = nullptr;
  // Per-body timescale table from AST->HIR. Parallel to spec groups.
  // Consumed during Phase 2 assembly to set timescale on mir::ModuleBody.
  const std::vector<common::BodyTimeScale>* body_timescales = nullptr;
  // Full scope hierarchy including generate scopes. Built at AST->HIR time.
  const std::vector<common::HierarchyNode>* hierarchy_nodes = nullptr;
  // Preclassified DPI export signatures from AST->HIR composition metadata.
  // Consumed by CollectDesignDeclarations to build canonical mir::DpiSignature
  // entries without reclassifying types. Must be non-null.
  const hir::DpiExportSignatureCache* dpi_export_signatures = nullptr;
};

// Statistics collected during HIR->MIR lowering (for --stats output).
struct LoweringStats {
  uint64_t place_temps = 0;
  uint64_t value_temps = 0;
  // Times a value-representation operand (UseTemp/Const) was converted to a
  // place temp via allocate+assign, so that a projection (bit-range, field,
  // element access) could address it.  Most SV selections operate on lvalues
  // that are already places, so this is expected to be 0 on simple tests;
  // real designs with complex expressions may show non-zero counts.
  uint64_t materialize_to_place = 0;
  uint64_t mir_stmts = 0;
};

struct LoweringResult {
  mir::Design design;
  mir::ConstructionInput construction;
  // Design-level arena for design-global MIR (package places, package
  // functions, init processes, connection processes). Body-local MIR
  // is in each ModuleBody's embedded arena.
  std::unique_ptr<mir::Arena> design_arena;
  // Design-global origins (package init processes, generated functions).
  OriginMap design_origins;
  // Per-body origins, indexed by ModuleBodyId. Body-local MIR origins
  // stay body-local -- not merged into design_origins.
  std::vector<std::vector<OriginEntry>> body_origins;
  LoweringStats stats;
  std::vector<mir::BoundConnection> bound_connections;
  // DPI export wrapper descriptors for LLVM backend emission.
  // Deterministically sorted by c_name.
  std::vector<mir::DpiExportWrapperDesc> dpi_export_wrappers;
  // Generated DPI header content. Empty if no DPI symbols exist.
  std::string dpi_header;
};

auto LowerHirToMir(const LoweringInput& input) -> Result<LoweringResult>;

}  // namespace lyra::lowering::hir_to_mir
