#pragma once

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
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

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
      UnitLowerer& unit_lowerer, const slang::ast::Scope& slang_scope)
      : owner_(&unit_lowerer),
        slang_scope_(&slang_scope),
        frame_(unit_lowerer.LookupScopeFrame(slang_scope)) {
  }

  // A parameter of the scope whose value whoever constructs the scope supplies,
  // rather than one folded to what a single elaboration gave it (LRM 27.4). It
  // is declared before any member is walked, so a name reaching it resolves to
  // the declaration; `declared` comes back so the construction can say which
  // declaration it fills.
  struct ConstructionValue {
    const slang::ast::ValueSymbol* parameter = nullptr;
    hir::StructuralDataObjectId declared{};
  };

  // Stack-allocates the output `hir::StructuralScope`, walks every member of
  // `slang_scope_` into it, and returns it. `parent_frame` is the caller's walk
  // frame; this scope's own ScopeFrameId and `&scope` are pushed by Run before
  // dispatching to per-member helpers.
  auto Run(WalkFrame parent_frame, ConstructionValue* construction_value)
      -> diag::Result<hir::StructuralScope>;
  auto Run(WalkFrame parent_frame) -> diag::Result<hir::StructuralScope> {
    return Run(parent_frame, nullptr);
  }

  // This scope's identity on the walk, which a declaration added to it is
  // bound against.
  [[nodiscard]] auto Frame() const -> ScopeFrameId {
    return frame_;
  }

  [[nodiscard]] auto Owner() -> UnitLowerer& {
    return *owner_;
  }
  [[nodiscard]] auto Owner() const -> const UnitLowerer& {
    return *owner_;
  }

  // Walker entry for structural-context expression lowering (continuous
  // assigns, structural var initializers).
  auto LowerExpr(const slang::ast::Expression& expr, WalkFrame frame)
      -> diag::Result<hir::Expr>;

 private:
  auto DeclareBlockParameters(hir::StructuralScope& scope, WalkFrame frame)
      -> diag::Result<void>;
  auto PopulateMember(const slang::ast::Symbol& member, WalkFrame frame)
      -> diag::Result<void>;
  auto PopulateInterfacePortMember(
      const slang::ast::InterfacePortSymbol& port, WalkFrame frame)
      -> diag::Result<void>;
  auto PopulateVariableMember(
      const slang::ast::VariableSymbol& var, WalkFrame frame)
      -> diag::Result<void>;
  auto PopulateNetMember(const slang::ast::NetSymbol& net, WalkFrame frame)
      -> diag::Result<void>;
  auto PopulateSubroutineMember(
      const slang::ast::SubroutineSymbol& sym, WalkFrame frame)
      -> diag::Result<void>;
  auto PopulateForeignImportMember(const slang::ast::SubroutineSymbol& sym)
      -> diag::Result<void>;
  auto PopulateModportMember(
      const slang::ast::ModportSymbol& modport, WalkFrame frame)
      -> diag::Result<void>;
  auto PopulateProceduralBlockMember(
      const slang::ast::ProceduralBlockSymbol& proc, WalkFrame frame)
      -> diag::Result<void>;
  auto PopulateContinuousAssignMember(
      const slang::ast::ContinuousAssignSymbol& sym, WalkFrame frame)
      -> diag::Result<void>;
  auto PopulateGenerateArrayMember(
      const slang::ast::GenerateBlockArraySymbol& array, WalkFrame frame)
      -> diag::Result<void>;
  auto PopulateGenerateBlockMember(
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
  auto PopulateNetAliasMember(
      const slang::ast::NetAliasSymbol& alias, WalkFrame frame)
      -> diag::Result<void>;
  auto LowerContinuousAssign(
      const slang::ast::ContinuousAssignSymbol& sym, WalkFrame frame)
      -> diag::Result<hir::ContinuousAssign>;
  auto BuildGenerateFromArray(
      const slang::ast::GenerateBlockArraySymbol& array, WalkFrame frame)
      -> diag::Result<hir::Generate>;
  auto BuildGenerateFromBlock(
      const slang::ast::GenerateBlockSymbol& block, WalkFrame frame)
      -> diag::Result<hir::Generate>;

  UnitLowerer* owner_;
  const slang::ast::Scope* slang_scope_;
  ScopeFrameId frame_;
};

}  // namespace lyra::lowering::ast_to_hir
