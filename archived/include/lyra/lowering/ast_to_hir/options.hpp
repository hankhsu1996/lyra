#pragma once

namespace lyra::lowering::ast_to_hir {

// Policy options for AST->HIR lowering.
// Only options that directly change AST->HIR behavior belong here.
// Constructed by the driver from CompilationInput; lowering does not
// depend on the full driver input object.
struct HirLoweringOptions {
  bool disable_assertions = false;
};

}  // namespace lyra::lowering::ast_to_hir
