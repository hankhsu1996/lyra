#pragma once

#include "lyra/base/internal_error.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/procedural_hops.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/structural_hops.hpp"
#include "lyra/mir/structural_scope.hpp"

namespace lyra::backend::cpp {

// The rendering fold's walk position
// (docs/architecture/lowering_organization.md "The Walk Position"). The
// MIR-to-C++ emit is a fold, not a construction pass: it accumulates nothing
// and owns no output, so this carries only what reading a node needs -- the
// unit (for type and arena lookups) and the chain of enclosing scopes. A
// hops-relative reference resolves by climbing that chain, exactly as the
// construction-side `WalkFrame` resolves it; this is the read-only twin of that
// frame, thin because every decision is already baked into the MIR it reads.
// Immutable and copied on descent (`WithProceduralScope` /
// `WithStructuralScope`); it grows no member per concept, so it is not the
// forbidden growing `*Context`.
class ScopeView {
 public:
  static auto ForRoot(
      const mir::CompilationUnit& unit,
      const mir::StructuralScope& structural_scope,
      const mir::ProceduralScope& procedural_scope) -> ScopeView {
    return ScopeView{unit, structural_scope, procedural_scope};
  }

  [[nodiscard]] auto WithProceduralScope(
      const mir::ProceduralScope& child) const -> ScopeView {
    return ScopeView{*unit_, *scope_, child, this, structural_parent_};
  }

  [[nodiscard]] auto WithStructuralScope(
      const mir::StructuralScope& child_scope,
      const mir::ProceduralScope& root_proc_scope) const -> ScopeView {
    return ScopeView{*unit_, child_scope, root_proc_scope, nullptr, this};
  }

  ScopeView(const ScopeView&) = delete;
  auto operator=(const ScopeView&) -> ScopeView& = delete;
  ScopeView(ScopeView&&) = delete;
  auto operator=(ScopeView&&) -> ScopeView& = delete;
  ~ScopeView() = default;

  [[nodiscard]] auto Unit() const -> const mir::CompilationUnit& {
    return *unit_;
  }

  [[nodiscard]] auto StructuralScope() const -> const mir::StructuralScope& {
    return *scope_;
  }

  [[nodiscard]] auto ProceduralScope() const -> const mir::ProceduralScope& {
    return *proc_scope_;
  }

  [[nodiscard]] auto ProceduralScopeAtHops(mir::ProceduralHops hops) const
      -> const mir::ProceduralScope& {
    if (hops.value == 0) {
      return *proc_scope_;
    }
    if (procedural_parent_ == nullptr) {
      throw InternalError(
          "ScopeView::ProceduralScopeAtHops: hops out of range");
    }
    return procedural_parent_->ProceduralScopeAtHops(
        mir::ProceduralHops{.value = hops.value - 1});
  }

  [[nodiscard]] auto StructuralScopeAtHops(mir::StructuralHops hops) const
      -> const mir::StructuralScope& {
    if (hops.value == 0) {
      return *scope_;
    }
    if (structural_parent_ == nullptr) {
      throw InternalError(
          "ScopeView::StructuralScopeAtHops: hops out of range");
    }
    return structural_parent_->StructuralScopeAtHops(
        mir::StructuralHops{.value = hops.value - 1});
  }

  [[nodiscard]] auto Expr(mir::ExprId id) const -> const mir::Expr& {
    return proc_scope_->GetExpr(id);
  }

 private:
  ScopeView(
      const mir::CompilationUnit& unit, const mir::StructuralScope& scope,
      const mir::ProceduralScope& proc_scope)
      : unit_(&unit),
        scope_(&scope),
        proc_scope_(&proc_scope),
        procedural_parent_(nullptr),
        structural_parent_(nullptr) {
  }

  ScopeView(
      const mir::CompilationUnit& unit, const mir::StructuralScope& scope,
      const mir::ProceduralScope& proc_scope,
      const ScopeView* procedural_parent, const ScopeView* structural_parent)
      : unit_(&unit),
        scope_(&scope),
        proc_scope_(&proc_scope),
        procedural_parent_(procedural_parent),
        structural_parent_(structural_parent) {
  }

  const mir::CompilationUnit* unit_;
  const mir::StructuralScope* scope_;
  const mir::ProceduralScope* proc_scope_;
  const ScopeView* procedural_parent_;
  const ScopeView* structural_parent_;
};

}  // namespace lyra::backend::cpp
