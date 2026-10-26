#pragma once

#include <span>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/Scope.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/TimingControl.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/MemberSymbols.h>
#include <slang/ast/symbols/ParameterSymbols.h>
#include <slang/ast/symbols/PortSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/continuous_assign.hpp"
#include "lyra/hir/dumped_declaration.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/scope_construction.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"
#include "lyra/support/strength_level.hpp"

namespace lyra::lowering::ast_to_hir {

// A scope as it is lowered, and how the object it is lowered from builds its
// children.
struct BuiltScope {
  hir::StructuralScope scope;
  hir::ScopeConstruction construction;
};

// A generate construct as its scope holds it, and the block the object being
// lowered builds at each of its places.
struct BuiltGenerate {
  hir::Generate generate;
  std::vector<hir::BlockConstruction> blocks;
};

// An instance member as its scope holds it, and the specialization the object
// being lowered makes at each object it stands for, in row-major order.
struct BuiltInstanceMember {
  hir::InstanceMemberDecl member;
  std::vector<hir::SpecializationRef> made;
};

// The ports a scope declares, by the net or variable each connects inside it.
using DumpedPorts =
    std::unordered_map<const slang::ast::Symbol*, hir::DumpedDeclaration::Port>;

// Per-structural-scope lowerer: produces one hir::StructuralScope populated
// with the members of a slang scope (variables, subroutines, processes,
// continuous assigns, generates), and how the object it is lowered from builds
// each block those generates hold. Constructed once per elaborated structural
// scope -- the unit root, and each block instance of a generate, which is
// lowered from its own elaboration (LRM 27.4, 27.5); runs once via Run(frame).
//
// Run stack-allocates the output scope, threads `&scope` through the WalkFrame
// so per-member handlers write to it via `frame.current_structural_scope`, and
// returns the scope by value when the walk completes.
class StructuralScopeLowerer {
 public:
  StructuralScopeLowerer(
      UnitLowerer& unit_lowerer, const slang::ast::Scope& scope)
      : owner_(&unit_lowerer),
        slang_scope_(&scope),
        frame_(unit_lowerer.LookupScopeFrame(scope)) {
  }

  // Stack-allocates the output `hir::StructuralScope`, walks every member of
  // the elaboration it is lowered from into it, and returns it beside how that
  // object builds its children. `parent_frame` is the caller's walk frame; this
  // scope's own ScopeFrameId and `&scope` are pushed by Run before dispatching
  // to per-member helpers.
  auto Run(WalkFrame parent_frame) -> diag::Result<BuiltScope>;

  [[nodiscard]] auto LoweredFrom() const -> const slang::ast::Scope& {
    return *slang_scope_;
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
  auto DeclareDifferingParameters(hir::StructuralScope& scope, WalkFrame frame)
      -> diag::Result<void>;
  auto SuppliedDeclarationOf(
      const slang::ast::ParameterSymbol& parameter, WalkFrame frame)
      -> diag::Result<hir::StructuralDataObjectKind>;
  auto DeclareSettledValue(
      hir::StructuralScope& scope, const slang::ast::ValueSymbol& value,
      hir::StructuralDataObjectKind kind) -> diag::Result<void>;
  auto BuildInstanceMember(
      const slang::ast::Symbol& declared,
      std::span<const slang::ast::InstanceSymbol* const> elements,
      std::vector<hir::UnpackedRange> dims, WalkFrame frame)
      -> diag::Result<BuiltInstanceMember>;
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
  auto DefineEvaluator(
      const slang::ast::Symbol& holder, std::string name,
      const slang::ast::Expression& expr, WalkFrame frame)
      -> diag::Result<void>;
  auto PopulateModportMember(
      const slang::ast::ModportSymbol& modport, WalkFrame frame)
      -> diag::Result<void>;
  auto PopulatePortMember(const slang::ast::PortSymbol& port, WalkFrame frame)
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
  // LRM 21.7.1.2: what a value dump records of an instance of this scope, the
  // data objects `members` declares and the named blocks, tasks and functions
  // among them, each named as a body of the scope names it, and of those
  // `ports` connects, the port each is (LRM 21.7.3.1).
  auto CollectDumped(
      const slang::ast::Scope& members, const DumpedPorts& ports,
      WalkFrame frame) -> diag::Result<hir::DumpedMembers>;
  auto LowerContinuousAssign(
      const slang::ast::ContinuousAssignSymbol& sym, WalkFrame frame)
      -> diag::Result<hir::ContinuousAssign>;
  auto BuildContinuousAssign(
      diag::SourceSpan span, hir::Expr lhs, hir::Expr rhs,
      const slang::ast::Expression& driving,
      const slang::ast::Symbol& containing, support::StrengthLevel strength,
      const slang::ast::TimingControl* delay, WalkFrame frame)
      -> diag::Result<hir::ContinuousAssign>;
  auto BuildGenerateFromArray(
      const slang::ast::GenerateBlockArraySymbol& array, WalkFrame frame)
      -> diag::Result<BuiltGenerate>;
  auto BuildGenerateFromBlock(
      const slang::ast::GenerateBlockSymbol& block, WalkFrame frame)
      -> diag::Result<BuiltGenerate>;

  UnitLowerer* owner_;
  const slang::ast::Scope* slang_scope_;
  ScopeFrameId frame_;
  hir::ScopeConstruction construction_;
};

}  // namespace lyra::lowering::ast_to_hir
