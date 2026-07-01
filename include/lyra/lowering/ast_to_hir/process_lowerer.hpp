#pragma once

#include <optional>
#include <unordered_map>
#include <unordered_set>

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
#include "lyra/hir/type_id.hpp"
#include "lyra/lowering/ast_to_hir/module_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace lyra::lowering::ast_to_hir {

// Per-process-body lowerer: walks a slang procedural block or subroutine body
// into a hir::ProceduralBody. Constructed once per process / subroutine; runs
// once via Run for a procedural block, or used as a helper for subroutine
// lowering by StructuralScopeLowerer (which owns the body stack allocation in
// that case). Holds the procedural-var binding registry; the in-flight body is
// reached through `frame.current_procedural_body`.
class ProcessLowerer {
 public:
  ProcessLowerer(
      ModuleLowerer& module, const slang::ast::Symbol& containing_symbol);

  // Lowers a procedural block to a complete hir::Process (initial / final /
  // always / always_comb / always_latch / always_ff). Stack-allocates the
  // body on entry, walks the slang body into it, and returns the assembled
  // Process by value.
  auto Run(
      const slang::ast::ProceduralBlockSymbol& proc, WalkFrame parent_frame)
      -> diag::Result<hir::Process>;

  // Computes which automatic locals a detached fork branch borrows and can
  // outlive (LRM 6.21), as a set of slang symbols. Run once over the body
  // before it is lowered, so `AddProceduralVar` knows each decl's status at
  // creation rather than back-patching it at a later reference.
  void AnalyzeLifetimeExtended(const slang::ast::Statement& body);

  // Composite: appends a `ProceduralVarDecl` to `body`, registers the
  // slang-to-HIR binding for `var` in the procedural-var registry, and
  // attaches the new id to the lexical scope the walk frame is currently
  // building. The three halves stay atomic so no caller forgets to register
  // a binding it later looks up, and no var slips out of its declaring
  // scope's downward-ownership list.
  auto AddProceduralVar(
      const WalkFrame& frame, hir::ProceduralBody& body,
      const slang::ast::VariableSymbol& var, hir::TypeId type)
      -> hir::ProceduralVarId;

  [[nodiscard]] auto LookupProceduralVar(const slang::ast::VariableSymbol& var)
      const -> std::optional<hir::ProceduralVarId>;

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

  // Registry.
  std::unordered_map<const slang::ast::VariableSymbol*, hir::ProceduralVarId>
      procedural_var_bindings_;
  std::unordered_set<const slang::ast::VariableSymbol*> lifetime_extended_;
};

}  // namespace lyra::lowering::ast_to_hir
