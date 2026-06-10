#pragma once

#include <optional>
#include <string_view>
#include <unordered_map>

#include <slang/ast/Expression.h>
#include <slang/ast/Statement.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/statements/LoopStatements.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/inside_item.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/ast_to_hir/module_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace lyra::lowering::ast_to_hir {

// Per-process-body lowerer: builds one hir::ProceduralBody from a slang
// procedural block or subroutine body. Constructed in two contexts:
// (1) for an `initial`/`final`/`always*` block by ScopeLowerer; (2) for a
// subroutine body by ScopeLowerer's subroutine member handler. Runs once via
// Run(WalkFrame); a subroutine path lowers the body via LowerStmt+AddStmt.
class ProcessLowerer {
 public:
  ProcessLowerer(
      ModuleLowerer& module, const slang::ast::Symbol& containing_symbol);

  // Lowers a procedural block to a complete hir::Process (initial / final /
  // always / always_comb / always_latch / always_ff).
  auto Run(const slang::ast::ProceduralBlockSymbol& proc, WalkFrame frame)
      -> diag::Result<hir::Process>;

  // Finalizes and moves the accumulated body out for return.
  auto FinalizeBody(hir::StmtId root_stmt) -> hir::ProceduralBody;

  // Public body-building API used by helper passes that construct sub-exprs
  // or sub-stmts directly (foreach desugar, scan/print/file-io desugars from
  // the HIR-to-MIR side that reuse this builder, etc.).
  auto AddExpr(hir::Expr expr) -> hir::ExprId;
  auto AddStmt(hir::Stmt stmt) -> hir::StmtId;
  auto AddProceduralVar(const slang::ast::VariableSymbol& var, hir::TypeId type)
      -> hir::ProceduralVarId;
  auto AddSyntheticProceduralVar(std::string_view name, hir::TypeId type)
      -> hir::ProceduralVarId;
  [[nodiscard]] auto LookupProceduralVar(const slang::ast::VariableSymbol& var)
      const -> std::optional<hir::ProceduralVarId>;
  [[nodiscard]] auto GetProceduralVarType(hir::ProceduralVarId id) const
      -> hir::TypeId;

  // Accessors.
  [[nodiscard]] auto Module() -> ModuleLowerer& {
    return *module_;
  }
  [[nodiscard]] auto Module() const -> const ModuleLowerer& {
    return *module_;
  }
  [[nodiscard]] auto ContainingSymbol() const -> const slang::ast::Symbol& {
    return *containing_symbol_;
  }

  // Walker entries used by helper passes inside this layer.
  auto LowerExpr(const slang::ast::Expression& expr, WalkFrame frame)
      -> diag::Result<hir::Expr>;
  auto LowerStmt(const slang::ast::Statement& stmt, WalkFrame frame)
      -> diag::Result<hir::Stmt>;
  auto LowerInsideItem(const slang::ast::Expression& item_expr, WalkFrame frame)
      -> diag::Result<hir::InsideItem>;
  auto LowerForeachStmt(
      const slang::ast::ForeachLoopStatement& fs, WalkFrame frame)
      -> diag::Result<hir::Stmt>;
  auto ValidateAssignableProcedural(const slang::ast::Expression& expr)
      -> diag::Result<void>;

 private:
  // Facts.
  ModuleLowerer* module_;
  const slang::ast::Symbol* containing_symbol_;

  // Builder.
  hir::ProceduralBody body_;

  // Registry.
  std::unordered_map<const slang::ast::VariableSymbol*, hir::ProceduralVarId>
      procedural_var_bindings_;
};

}  // namespace lyra::lowering::ast_to_hir
