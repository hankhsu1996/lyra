#pragma once

#include <optional>
#include <span>
#include <unordered_map>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/Scope.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/MemberSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/continuous_assign.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/ast_to_hir/module_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace slang::ast {
class TypeAliasType;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// Per-structural-scope lowerer: produces one hir::StructuralScope populated
// with the members of a slang scope (variables, subroutines, processes,
// continuous assigns, generates). Constructed once per structural scope (the
// unit root plus one per nested generate scope); runs once via Run(frame).
//
// Run stack-allocates the output scope, threads `&scope` through the WalkFrame
// so per-member handlers write to it via `frame.current_structural_scope`, and
// returns the scope by value when the walk completes.
class StructuralScopeLowerer {
 public:
  StructuralScopeLowerer(
      ModuleLowerer& module, const slang::ast::Scope& slang_scope,
      std::vector<ScopeEntryLoopVarBinding> entry_loop_var_bindings = {});

  // Stack-allocates the output `hir::StructuralScope`, walks every member of
  // `slang_scope_` into it, and returns it. `parent_frame` is the caller's walk
  // frame; this scope's own ScopeFrameId and `&scope` are pushed by Run before
  // dispatching to per-member helpers.
  auto Run(WalkFrame parent_frame) -> diag::Result<hir::StructuralScope>;

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
  auto PopulateTypeAliasMember(
      const slang::ast::TypeAliasType& alias, WalkFrame frame)
      -> diag::Result<void>;
  auto PopulateVariableMember(
      const slang::ast::VariableSymbol& var, WalkFrame frame)
      -> diag::Result<void>;
  // The reference binding of `var` if it is the internal variable of a `ref` /
  // `const ref` port of this module body (LRM 23.3.3.2); nullopt otherwise.
  [[nodiscard]] auto ReferenceBindingFor(const slang::ast::VariableSymbol& var)
      const -> std::optional<hir::ReferenceBinding>;
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
  auto PopulateInstanceMember(
      const slang::ast::InstanceSymbol& inst, WalkFrame frame)
      -> diag::Result<void>;
  auto PopulateInstanceArrayMember(
      const slang::ast::InstanceArraySymbol& array, WalkFrame frame)
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
  // The internal variables of this body's `ref` / `const ref` ports, keyed by
  // symbol; built once at construction from the module body's port list, empty
  // for a scope (generate block) that declares no ports.
  std::unordered_map<const slang::ast::Symbol*, hir::ReferenceBinding>
      ref_port_internals_;
};

}  // namespace lyra::lowering::ast_to_hir
