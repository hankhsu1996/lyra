#pragma once

#include <cstdint>
#include <optional>
#include <span>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/Scope.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/MemberSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/ValueSymbol.h>
#include <slang/ast/symbols/VariableSymbols.h>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/continuous_assign.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/loop_var.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/structural_var.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/lowering/ast_to_hir/module_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace slang::ast {
class TypeAliasType;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// Per-structural-scope lowerer: fills one hir::StructuralScope with the
// members of a slang scope (variables, subroutines, processes, continuous
// assigns, generates). Constructed once per structural scope (the unit root
// plus one per nested generate scope); runs once via Run(WalkFrame).
class ScopeLowerer {
 public:
  ScopeLowerer(
      ModuleLowerer& module, hir::StructuralScope& scope,
      const slang::ast::Scope& slang_scope,
      std::vector<ScopeEntryLoopVarBinding> entry_loop_var_bindings = {});

  // Lowers every member of slang_scope_ into scope_. `parent_frame` is the
  // caller's walk frame; this scope's own ScopeFrameId is pushed by Run before
  // dispatching to per-member helpers, so callers do not stack the frame
  // themselves.
  auto Run(WalkFrame parent_frame) -> diag::Result<void>;

  // Public builder API: scope-level Add*. Called by inner Lowerers (e.g.,
  // ProcessLowerer adds the lowered Process via AddProcess).
  auto AddStructuralVar(
      const slang::ast::VariableSymbol& var, hir::TypeId type,
      std::optional<hir::ExprId> initializer = std::nullopt)
      -> hir::StructuralVarId;
  void AddTypeAlias(hir::TypeAliasDecl decl);
  auto AddLoopVarDecl(const slang::ast::ValueSymbol& sym, hir::TypeId type)
      -> hir::LoopVarDeclId;
  auto AddExpr(hir::Expr expr) -> hir::ExprId;
  auto AddProcess(hir::Process process) -> hir::ProcessId;
  auto AddContinuousAssign(hir::ContinuousAssign ca) -> hir::ContinuousAssignId;
  auto AddGenerate(hir::Generate generate) -> hir::GenerateId;
  [[nodiscard]] auto NextGenerateId() const -> hir::GenerateId;
  auto AddInstanceMember(hir::InstanceMemberDecl decl) -> hir::InstanceMemberId;

  // Forward-declares a subroutine binding to a stable id before any body is
  // lowered, so calls resolve regardless of source order (LRM 13.4.2).
  void ReserveSubroutineBinding(const slang::ast::SubroutineSymbol& sym);
  void AddStructuralSubroutine(
      const slang::ast::SubroutineSymbol& sym,
      hir::StructuralSubroutineDecl decl);

  // Accessors for inner Lowerers and helpers.
  [[nodiscard]] auto Module() -> ModuleLowerer& {
    return *module_;
  }
  [[nodiscard]] auto Module() const -> const ModuleLowerer& {
    return *module_;
  }

  // Walker entry for structural-context expression lowering (continuous
  // assigns, structural var initializers).
  auto LowerExpr(const slang::ast::Expression& expr, WalkFrame frame)
      -> diag::Result<hir::Expr>;

  // Rejects an assignment-target expression that is not addressable in
  // structural context (continuous assignment LHS).
  auto ValidateAssignableStructural(const slang::ast::Expression& expr)
      -> diag::Result<void>;

 private:
  // Per-member dispatch and per-kind helpers used by Run.
  auto PopulateMember(const slang::ast::Symbol& member, WalkFrame frame)
      -> diag::Result<void>;
  auto PopulateTypeAliasMember(const slang::ast::TypeAliasType& alias)
      -> diag::Result<void>;
  auto PopulateVariableMember(
      const slang::ast::VariableSymbol& var, WalkFrame frame)
      -> diag::Result<void>;
  auto PopulateSubroutineMember(
      const slang::ast::SubroutineSymbol& sym, WalkFrame frame)
      -> diag::Result<void>;
  auto PopulateProceduralBlockMember(
      const slang::ast::ProceduralBlockSymbol& proc, WalkFrame frame)
      -> diag::Result<void>;
  auto PopulateContinuousAssignMember(
      const slang::ast::ContinuousAssignSymbol& sym, WalkFrame frame)
      -> diag::Result<void>;
  auto PopulateLoopGenerateMember(
      const slang::ast::GenerateBlockArraySymbol& array, WalkFrame frame)
      -> diag::Result<void>;
  auto PopulateIfOrCaseGenerateMember(
      const slang::ast::GenerateBlockSymbol& block, WalkFrame frame)
      -> diag::Result<void>;
  auto PopulateInstanceMember(const slang::ast::InstanceSymbol& inst)
      -> diag::Result<void>;
  auto PopulateInstanceArrayMember(const slang::ast::InstanceArraySymbol& array)
      -> diag::Result<void>;
  auto PopulatePortConnections(
      const slang::ast::Scope& slang_scope, WalkFrame frame)
      -> diag::Result<void>;
  auto LowerContinuousAssign(
      const slang::ast::ContinuousAssignSymbol& sym, WalkFrame frame)
      -> diag::Result<hir::ContinuousAssign>;
  auto BuildIfGenerate(
      std::span<const slang::ast::GenerateBlockSymbol* const> siblings,
      WalkFrame frame) -> diag::Result<hir::Generate>;
  auto BuildCaseGenerate(
      std::span<const slang::ast::GenerateBlockSymbol* const> siblings,
      WalkFrame frame) -> diag::Result<hir::Generate>;
  auto BuildLoopGenerate(
      const slang::ast::GenerateBlockArraySymbol& array, WalkFrame frame)
      -> diag::Result<hir::Generate>;

  // Facts.
  ModuleLowerer* module_;
  const slang::ast::Scope* slang_scope_;
  ScopeFrameId frame_;
  std::vector<ScopeEntryLoopVarBinding> entry_loop_var_bindings_;

  // Builder ref.
  hir::StructuralScope* scope_;

  // Registry (counter).
  std::uint32_t reserved_subroutine_count_ = 0;
};

}  // namespace lyra::lowering::ast_to_hir
