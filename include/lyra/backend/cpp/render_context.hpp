#pragma once

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
    return RenderContext{
        *unit_,
        *scope_,
        child,
        this,
        structural_parent_,
        static_frame_,
        in_coroutine_,
        in_class_member_init_};
  }

  [[nodiscard]] auto WithStructuralScope(
      const mir::StructuralScope& child_scope,
      const mir::ProceduralScope& root_proc_scope) const -> RenderContext {
    return RenderContext{*unit_, child_scope, root_proc_scope, nullptr,
                         this,   {},          false,           false};
  }

  // A subroutine body renders its static locals through this per-instance frame
  // member (LRM 13.3.1). Empty when the current callable has no static locals.
  [[nodiscard]] auto WithStaticFrame(std::string_view accessor) const
      -> RenderContext {
    return RenderContext{
        *unit_,
        *scope_,
        *proc_scope_,
        procedural_parent_,
        structural_parent_,
        accessor,
        in_coroutine_,
        in_class_member_init_};
  }

  // Marks the current body as a coroutine (a process or a task), where `return`
  // is spelled `co_return`. A function body leaves this false and uses plain
  // `return`.
  [[nodiscard]] auto WithCoroutine(bool in_coroutine) const -> RenderContext {
    return RenderContext{
        *unit_,
        *scope_,
        *proc_scope_,
        procedural_parent_,
        structural_parent_,
        static_frame_,
        in_coroutine,
        in_class_member_init_};
  }

  // A class-member initializer is rendered without `self`: there is no instance
  // in scope at C++ class body init time, so structural-param / structural-var
  // accesses spell the field name directly. Method bodies leave this false and
  // qualify accesses with `self->`.
  [[nodiscard]] auto WithClassMemberInit() const -> RenderContext {
    return RenderContext{
        *unit_,
        *scope_,
        *proc_scope_,
        procedural_parent_,
        structural_parent_,
        static_frame_,
        in_coroutine_,
        true};
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

  [[nodiscard]] auto StaticFrame() const -> std::string_view {
    return static_frame_;
  }

  [[nodiscard]] auto InCoroutine() const -> bool {
    return in_coroutine_;
  }

  [[nodiscard]] auto Expr(mir::ExprId id) const -> const mir::Expr& {
    return proc_scope_->GetExpr(id);
  }

  [[nodiscard]] auto InClassMemberInit() const -> bool {
    return in_class_member_init_;
  }

 private:
  RenderContext(
      const mir::CompilationUnit& unit, const mir::StructuralScope& scope,
      const mir::ProceduralScope& proc_scope)
      : unit_(&unit),
        scope_(&scope),
        proc_scope_(&proc_scope),
        procedural_parent_(nullptr),
        structural_parent_(nullptr) {
  }

  RenderContext(
      const mir::CompilationUnit& unit, const mir::StructuralScope& scope,
      const mir::ProceduralScope& proc_scope,
      const RenderContext* procedural_parent,
      const RenderContext* structural_parent, std::string_view static_frame,
      bool in_coroutine, bool in_class_member_init)
      : unit_(&unit),
        scope_(&scope),
        proc_scope_(&proc_scope),
        procedural_parent_(procedural_parent),
        structural_parent_(structural_parent),
        static_frame_(static_frame),
        in_coroutine_(in_coroutine),
        in_class_member_init_(in_class_member_init) {
  }

  const mir::CompilationUnit* unit_;
  const mir::StructuralScope* scope_;
  const mir::ProceduralScope* proc_scope_;
  const RenderContext* procedural_parent_;
  const RenderContext* structural_parent_;
  std::string_view static_frame_;
  bool in_coroutine_ = false;
  bool in_class_member_init_ = false;
};

}  // namespace lyra::backend::cpp
