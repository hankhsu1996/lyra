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
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace slang::ast {
class ConcurrentAssertionStatement;
class StatementBlockSymbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// A set of slang AST expressions an enclosing lowering has already consumed as
// a class- or scope-level semantic fact, so the body walker must not lower them
// a second time. It elides them by matching pointer identity, which needs no
// per-kind knowledge of which SV construct a node names -- and an entry the
// body never reaches simply matches nothing, so a caller adds what it consumed
// without first working out whether the source also wrote it inside a body.
// The single current user is the base construction (LRM 8.7), which the source
// may write as a `super.new(...)` statement in the ctor body or not write at
// all.
using ConsumedBodyExpressions =
    std::unordered_set<const slang::ast::Expression*>;

// Per-process-body lowerer: walks a slang procedural block or subroutine body
// into a hir::ProceduralBody. Constructed once per process / subroutine; runs
// once via Run for a procedural block, or used as a helper for subroutine
// lowering by StructuralScopeLowerer (which owns the body stack allocation in
// that case). Holds the procedural-var binding registry; the in-flight body is
// reached through `frame.current_procedural_body`.
class ProcessLowerer {
 public:
  ProcessLowerer(
      UnitLowerer& unit_lowerer, const slang::ast::Symbol& containing_symbol,
      ConsumedBodyExpressions consumed_body_exprs = {});

  // Lowers a procedural block to a complete hir::Process (initial / final /
  // always / always_comb / always_latch / always_ff). Stack-allocates the
  // body on entry, walks the slang body into it, and returns the assembled
  // Process by value.
  auto Run(
      const slang::ast::ProceduralBlockSymbol& proc, WalkFrame parent_frame)
      -> diag::Result<hir::Process>;

  // Lowers a procedure that is one concurrent assertion under no enabling
  // condition (LRM 16.14.5). The body it stack-allocates holds only the
  // statements an outcome selects, because the assertion itself is not
  // something the body runs. `named_block` is the block a statement label put
  // around the assertion, null where the source wrote no label; it is a scope
  // of the body like any other, and the name it carries is what the assertion
  // is reported under.
  auto RunConcurrentAssertion(
      const slang::ast::ProceduralBlockSymbol& proc,
      const slang::ast::ConcurrentAssertionStatement& as,
      const slang::ast::StatementBlockSymbol* named_block,
      WalkFrame parent_frame) -> diag::Result<hir::ConcurrentAssertionDecl>;

  // Computes which automatic locals a detached fork branch borrows and can
  // outlive (LRM 6.21), as a set of slang symbols. Run once over the body
  // before it is lowered, so each declaration's status is settled when the
  // declaration is created rather than back-patched at a later reference.
  void AnalyzeLifetimeExtended(const slang::ast::Statement& body);

  // Mints the local's identity and registers it: the slang-to-HIR binding for
  // `var`, and membership in the lexical scope the walk frame is building. A
  // static a hierarchical path can name already has an identity from the
  // compilation unit's declaration pass, so the body binds that one rather than
  // minting a second. The declaration's content is filled in a second step,
  // because the initializer is an expression of this body that may name this
  // very identity -- so the identity has to exist before its content does.
  auto DeclareProceduralVar(
      const WalkFrame& frame, hir::ProceduralBody& body,
      const slang::ast::VariableSymbol& var) -> hir::ProceduralVarId;

  void DefineProceduralVar(
      hir::ProceduralBody& body, hir::ProceduralVarId id,
      const slang::ast::VariableSymbol& var, hir::TypeId type,
      std::optional<hir::ExprId> init);

  // Both steps for a declaration whose initializer is already known -- which is
  // every one but a source declaration statement, whose initializer has to
  // lower against the identity this mints.
  auto AddProceduralVar(
      const WalkFrame& frame, hir::ProceduralBody& body,
      const slang::ast::VariableSymbol& var, hir::TypeId type,
      std::optional<hir::ExprId> init = std::nullopt) -> hir::ProceduralVarId;

  [[nodiscard]] auto LookupProceduralVar(const slang::ast::VariableSymbol& var)
      const -> std::optional<hir::ProceduralVarId>;

  [[nodiscard]] auto Owner() -> UnitLowerer& {
    return *owner_;
  }
  [[nodiscard]] auto Owner() const -> const UnitLowerer& {
    return *owner_;
  }
  [[nodiscard]] auto ContainingSymbol() const -> const slang::ast::Symbol& {
    return *containing_symbol_;
  }

  // Slang AST expressions the enclosing lowering has already consumed as a
  // higher-level semantic fact; the body walker elides any statement whose
  // whole expression matches by pointer identity. Empty for a walk with
  // no consumed residues.
  [[nodiscard]] auto ConsumedBodyExprs() const
      -> const ConsumedBodyExpressions& {
    return consumed_body_exprs_;
  }

  auto LowerExpr(const slang::ast::Expression& expr, WalkFrame frame)
      -> diag::Result<hir::Expr>;
  auto LowerStmt(const slang::ast::Statement& stmt, WalkFrame frame)
      -> diag::Result<hir::Stmt>;
  auto LowerForeachStmt(
      const slang::ast::ForeachLoopStatement& fs, WalkFrame frame)
      -> diag::Result<hir::Stmt>;
  auto ValidateAssignableProcedural(const slang::ast::Expression& expr)
      -> diag::Result<void>;

 private:
  UnitLowerer* owner_;
  const slang::ast::Symbol* containing_symbol_;
  ConsumedBodyExpressions consumed_body_exprs_;

  std::unordered_map<const slang::ast::VariableSymbol*, hir::ProceduralVarId>
      procedural_var_bindings_;
  std::unordered_set<const slang::ast::VariableSymbol*> lifetime_extended_;
};

}  // namespace lyra::lowering::ast_to_hir
