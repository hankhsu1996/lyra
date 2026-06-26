#pragma once

#include <cstdint>
#include <map>
#include <optional>
#include <unordered_map>
#include <vector>

#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/ValueSymbol.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/Type.h>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/frontend/slang_source_mapper.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/loop_var.hpp"
#include "lyra/hir/module_unit.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/structural_var.hpp"
#include "lyra/lowering/ast_to_hir/sensitivity.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace slang::ast {
class Expression;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

struct StructuralVarBinding {
  ScopeFrameId home_frame{};
  hir::StructuralVarId var_id{};
  hir::TypeId type{};
};

using StructuralVarBindings =
    std::unordered_map<const slang::ast::VariableSymbol*, StructuralVarBinding>;

struct SubroutineBinding {
  ScopeFrameId owner_frame{};
  hir::StructuralSubroutineId subroutine_id{};
};

using SubroutineBindings =
    std::unordered_map<const slang::ast::SubroutineSymbol*, SubroutineBinding>;

struct LoopVarBinding {
  ScopeFrameId home_frame{};
  hir::LoopVarDeclId loop_var_id{};
  hir::TypeId type{};
};

using LoopVarBindings =
    std::unordered_map<const slang::ast::ValueSymbol*, LoopVarBinding>;

// A downward reference's leading component names an owned child this scope
// declares: an instance / instance-array member (`c.x`, `c[1].x`), or a
// generate block (`g[1].x`, LRM 27). The child's slang symbol maps to the
// head the reference navigates from, so the reference resolves regardless of
// whether it precedes the child in source.
struct OwnedChildBinding {
  ScopeFrameId home_frame{};
  hir::DownwardHead head;
};

using OwnedChildBindings =
    std::unordered_map<const slang::ast::Symbol*, OwnedChildBinding>;

// A loop-generate body lowering carries forward the loop variable's binding
// from the parent so the body's references compute correct hops up to its
// home scope. `home_frame` is the parent's frame, not the body's.
struct ScopeEntryLoopVarBinding {
  const slang::ast::ValueSymbol* symbol = nullptr;
  ScopeFrameId home_frame{};
  hir::LoopVarDeclId loop_var{};
  hir::TypeId type{};
};

// Shared lowering-pass facts threaded into every ModuleLowerer. SourceMapper
// translates slang source locations; SensitivityAnalyzer is shared across
// every module's analysis (caches reads); disable_assertions is the policy
// that elides assertion constructs instead of rejecting them. Subset of
// `LowerCompilationFacts` that excludes the slang Compilation handle (which
// only the driver-level CompilationLowerer needs to walk top instances).
class LoweringFacts {
 public:
  LoweringFacts(
      const frontend::SlangSourceMapper& source_mapper,
      SensitivityAnalyzer& sensitivity_analyzer, bool disable_assertions)
      : source_mapper_(&source_mapper),
        sensitivity_analyzer_(&sensitivity_analyzer),
        disable_assertions_(disable_assertions) {
  }

  [[nodiscard]] auto SourceMapper() const
      -> const frontend::SlangSourceMapper& {
    return *source_mapper_;
  }

  [[nodiscard]] auto Sensitivity() const -> SensitivityAnalyzer& {
    return *sensitivity_analyzer_;
  }

  [[nodiscard]] auto DisableAssertions() const -> bool {
    return disable_assertions_;
  }

 private:
  const frontend::SlangSourceMapper* source_mapper_;
  SensitivityAnalyzer* sensitivity_analyzer_;
  bool disable_assertions_;
};

// Per-module lowerer.
//
// Facts: the shared lowering facts (source mapper + sensitivity analyzer) and
// a reference to the slang instance body being walked.
// Registries: type cache (slang canonical type -> hir TypeId), structural-var
// / subroutine / loop-var / owned-child binding tables, per-frame cross-unit
// ref slot tables, and a scope-frame counter.
// Root output: the in-progress `hir::ModuleUnit`. Constructed in the ctor with
// the module's name and an initial builtins table; populated by `Run`; moved
// out by `Run`'s return. After `Run` returns the class holds no IR pointer.
class ModuleLowerer {
 public:
  ModuleLowerer(
      const LoweringFacts& facts, const slang::ast::InstanceBodySymbol& body);

  auto Run() -> diag::Result<hir::ModuleUnit>;

  // Read access to the in-progress unit. Handlers reach the unit's type vocab
  // and builtins through this accessor; downstream consumers post-Run use the
  // same `hir::ModuleUnit` interface.
  [[nodiscard]] auto Unit() const -> const hir::ModuleUnit& {
    return unit_;
  }

  // Lowers a slang type to a HIR TypeId, memoizing by slang canonical pointer.
  // The slang-keyed cache and the unit's type table are coordinated together:
  // the dedup invariant (same slang canonical -> same HIR TypeId) is enforced
  // structurally here, so callers cannot bypass it by writing to the unit
  // directly.
  auto InternType(const slang::ast::Type& type, diag::SourceSpan span)
      -> diag::Result<hir::TypeId>;

  // Facts.
  [[nodiscard]] auto SourceMapper() const
      -> const frontend::SlangSourceMapper& {
    return facts_.SourceMapper();
  }
  [[nodiscard]] auto Sensitivity() const -> SensitivityAnalyzer& {
    return facts_.Sensitivity();
  }
  [[nodiscard]] auto DisableAssertions() const -> bool {
    return facts_.DisableAssertions();
  }

  // Binding registries. Each Map* asserts no double-mapping for the same
  // slang symbol.
  void MapStructuralVarBinding(
      const slang::ast::VariableSymbol& var, ScopeFrameId home_frame,
      hir::StructuralVarId local, hir::TypeId type);
  [[nodiscard]] auto LookupStructuralVarBinding(
      const slang::ast::VariableSymbol& var) const
      -> std::optional<StructuralVarBinding>;

  void MapSubroutineBinding(
      const slang::ast::SubroutineSymbol& sym, ScopeFrameId owner_frame,
      hir::StructuralSubroutineId local);
  [[nodiscard]] auto LookupSubroutineBinding(
      const slang::ast::SubroutineSymbol& sym) const
      -> std::optional<SubroutineBinding>;

  void MapLoopVarBinding(
      const slang::ast::ValueSymbol& sym, ScopeFrameId home_frame,
      hir::LoopVarDeclId id, hir::TypeId type);
  [[nodiscard]] auto LookupLoopVarBinding(const slang::ast::ValueSymbol& sym)
      const -> std::optional<LoopVarBinding>;

  void MapOwnedChildBinding(
      const slang::ast::Symbol& child, ScopeFrameId home_frame,
      hir::DownwardHead head);
  [[nodiscard]] auto LookupOwnedChildBinding(const slang::ast::Symbol& child)
      const -> std::optional<OwnedChildBinding>;

  // Cross-unit reference dedup. The slot itself is accumulated keyed by its
  // owning scope's frame; the owning StructuralScopeLowerer drains it via
  // TakeCrossUnitRefsForFrame at scope finalization.
  auto MapOrGetCrossUnitRef(
      const slang::ast::ValueSymbol& target, ScopeFrameId home_frame,
      hir::CrossUnitRefHead head, std::vector<hir::PathStep> path,
      hir::TypeId type) -> hir::CrossUnitRefId;
  auto TakeCrossUnitRefsForFrame(ScopeFrameId frame)
      -> std::vector<hir::CrossUnitRefDecl>;

  // Frame minting for scope entry.
  [[nodiscard]] auto NextScopeFrameId() -> ScopeFrameId;

  // Identity minting for an array-method `with` clause (LRM 7.12). Unique
  // within the unit, so HIR-to-MIR can key its iteration-binding registry on
  // it.
  [[nodiscard]] auto NextWithClauseId() -> hir::WithClauseId;

  // Builds a HIR Expr referring to a leaf reached by navigating `path` down
  // from `head`. `target` is the leaf value symbol (cross-unit dedup key);
  // `home_frame` is the owning structural scope's frame.
  auto MakeCrossUnitMemberRef(
      const slang::ast::ValueSymbol& target, ScopeFrameId home_frame,
      hir::CrossUnitRefHead head, std::vector<hir::PathStep> path,
      hir::TypeId type, diag::SourceSpan span) -> hir::Expr;

  // Translates slang-side reads to HIR SensitivityEntries via this module's
  // binding tables, using the current walk frame to compute hops.
  [[nodiscard]] auto TranslateSensitivityReads(
      const std::vector<SensitivityRead>& reads, const WalkFrame& frame) const
      -> std::vector<hir::SensitivityEntry>;

 private:
  // Cross-unit ref dedup lookup. Private: TranslateSensitivityReads is the
  // only caller; external code uses MapOrGetCrossUnitRef.
  [[nodiscard]] auto LookupCrossUnitRef(
      ScopeFrameId frame, const slang::ast::ValueSymbol& target) const
      -> std::optional<hir::CrossUnitRefId>;

  // Facts.
  LoweringFacts facts_;
  const slang::ast::InstanceBodySymbol* body_;

  // Root output.
  hir::ModuleUnit unit_;

  // Registries.
  std::unordered_map<const slang::ast::Type*, hir::TypeId> type_cache_;
  StructuralVarBindings structural_var_bindings_;
  SubroutineBindings subroutine_bindings_;
  LoopVarBindings loop_var_bindings_;
  OwnedChildBindings owned_child_bindings_;
  // Dedup by (home_frame, target): the slot id is an index within a scope's
  // own `cross_unit_refs`, so two scopes referencing the same member each need
  // their own slot.
  std::map<
      ScopeFrameId,
      std::unordered_map<const slang::ast::ValueSymbol*, hir::CrossUnitRefId>>
      cross_unit_ref_dedup_;
  std::map<ScopeFrameId, std::vector<hir::CrossUnitRefDecl>>
      cross_unit_refs_by_frame_;
  std::uint32_t next_scope_frame_ = 0;
  std::uint32_t next_with_clause_ = 0;
};

}  // namespace lyra::lowering::ast_to_hir
