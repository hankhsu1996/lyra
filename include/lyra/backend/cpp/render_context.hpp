#pragma once

#include <cstddef>
#include <format>
#include <string>
#include <string_view>

#include "lyra/base/internal_error.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/procedural_hops.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/structural_hops.hpp"
#include "lyra/mir/structural_scope.hpp"

namespace lyra::backend::cpp {

class RenderContext {
 public:
  static auto ForRoot(
      const mir::CompilationUnit& unit,
      const mir::StructuralScope& structural_scope,
      const mir::ProceduralScope& procedural_scope) -> RenderContext {
    return RenderContext{unit, structural_scope, procedural_scope};
  }

  [[nodiscard]] auto WithProceduralScope(
      const mir::ProceduralScope& child) const -> RenderContext {
    return RenderContext{*unit_,       *scope_, child, this, structural_parent_,
                         temp_counter_};
  }

  [[nodiscard]] auto WithStructuralScope(
      const mir::StructuralScope& child_scope,
      const mir::ProceduralScope& root_proc_scope) const -> RenderContext {
    return RenderContext{*unit_,  child_scope, root_proc_scope,
                         nullptr, this,        temp_counter_};
  }

  RenderContext(const RenderContext&) = delete;
  auto operator=(const RenderContext&) -> RenderContext& = delete;
  RenderContext(RenderContext&&) = delete;
  auto operator=(RenderContext&&) -> RenderContext& = delete;
  ~RenderContext() = default;

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
          "RenderContext::ProceduralScopeAtHops: hops out of range");
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
          "RenderContext::StructuralScopeAtHops: hops out of range");
    }
    return structural_parent_->StructuralScopeAtHops(
        mir::StructuralHops{.value = hops.value - 1});
  }

  [[nodiscard]] auto Expr(mir::ExprId id) const -> const mir::Expr& {
    return proc_scope_->GetExpr(id);
  }

  [[nodiscard]] auto AllocateTemp(std::string_view prefix) const
      -> std::string {
    return std::format("{}_{}", prefix, (*temp_counter_)++);
  }

 private:
  RenderContext(
      const mir::CompilationUnit& unit, const mir::StructuralScope& scope,
      const mir::ProceduralScope& proc_scope)
      : unit_(&unit),
        scope_(&scope),
        proc_scope_(&proc_scope),
        procedural_parent_(nullptr),
        structural_parent_(nullptr),
        temp_counter_(&owned_temp_counter_) {
  }

  RenderContext(
      const mir::CompilationUnit& unit, const mir::StructuralScope& scope,
      const mir::ProceduralScope& proc_scope,
      const RenderContext* procedural_parent,
      const RenderContext* structural_parent, std::size_t* temp_counter)
      : unit_(&unit),
        scope_(&scope),
        proc_scope_(&proc_scope),
        procedural_parent_(procedural_parent),
        structural_parent_(structural_parent),
        temp_counter_(temp_counter) {
  }

  const mir::CompilationUnit* unit_;
  const mir::StructuralScope* scope_;
  const mir::ProceduralScope* proc_scope_;
  const RenderContext* procedural_parent_;
  const RenderContext* structural_parent_;
  std::size_t* temp_counter_;
  mutable std::size_t owned_temp_counter_ = 0;
};

}  // namespace lyra::backend::cpp
