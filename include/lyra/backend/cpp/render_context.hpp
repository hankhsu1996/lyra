#pragma once

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
    return RenderContext{
        *unit_,        *scope_,       child,    this, structural_parent_,
        static_frame_, in_coroutine_, receiver_};
  }

  [[nodiscard]] auto WithStructuralScope(
      const mir::StructuralScope& child_scope,
      const mir::ProceduralScope& root_proc_scope) const -> RenderContext {
    return RenderContext{*unit_, child_scope, root_proc_scope, nullptr,
                         this,   {},          false,           "this"};
  }

  // A subroutine body renders its static locals through this per-instance frame
  // member (LRM 13.3.1). Empty when the current callable has no static locals.
  [[nodiscard]] auto WithStaticFrame(std::string_view accessor) const
      -> RenderContext {
    return RenderContext{*unit_,
                         *scope_,
                         *proc_scope_,
                         procedural_parent_,
                         structural_parent_,
                         accessor,
                         in_coroutine_,
                         receiver_};
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
        receiver_};
  }

  // Names the object the body reaches module state and Services() through. A
  // method body leaves this "this" and emits implicit-`this` access; a fork
  // branch (LRM 9.3.2) renders as an inline coroutine closure with no implicit
  // `this`, so its body names the frame-copied receiver parameter `self`
  // explicitly.
  [[nodiscard]] auto WithReceiver(std::string_view object) const
      -> RenderContext {
    return RenderContext{
        *unit_,
        *scope_,
        *proc_scope_,
        procedural_parent_,
        structural_parent_,
        static_frame_,
        in_coroutine_,
        object};
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

  // The object the current body reaches the enclosing scope instance through:
  // `this` in a method body, `self` in a fork-branch closure body. Named where
  // the object itself is spelled -- a spawned inner closure is invoked with it
  // and captures it, so a nested fork passes `self` on down.
  [[nodiscard]] auto ReceiverObject() const -> std::string_view {
    return receiver_;
  }

  // The prefix every scope-member access carries. Always explicit -- `this->`
  // in a method body, `self->` in a fork-branch closure body -- so a process
  // body and a branch body render identically up to the receiver name.
  [[nodiscard]] auto MemberPrefix() const -> std::string {
    return std::string(receiver_) + "->";
  }

  // The C++ expression naming the RuntimeServices the body submits effects
  // through (`this->Services()` / `self->Services()`).
  [[nodiscard]] auto ServicesRef() const -> std::string {
    return MemberPrefix() + "Services()";
  }

  // The capture clause of a `[=]`-default deferred lambda (NBA / `$strobe`)
  // that also grabs the receiver. A `self` pointer is a local captured by the
  // bare `[=]`; the `this` keyword must be named explicitly because C++20
  // deprecates `[=]` capture of `this`.
  [[nodiscard]] auto DeferredByValueCapture() const -> std::string_view {
    return receiver_ == "this" ? "[=, this]" : "[=]";
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
      bool in_coroutine, std::string_view receiver)
      : unit_(&unit),
        scope_(&scope),
        proc_scope_(&proc_scope),
        procedural_parent_(procedural_parent),
        structural_parent_(structural_parent),
        static_frame_(static_frame),
        in_coroutine_(in_coroutine),
        receiver_(receiver) {
  }

  const mir::CompilationUnit* unit_;
  const mir::StructuralScope* scope_;
  const mir::ProceduralScope* proc_scope_;
  const RenderContext* procedural_parent_;
  const RenderContext* structural_parent_;
  std::string_view static_frame_;
  bool in_coroutine_ = false;
  std::string_view receiver_ = "this";
};

}  // namespace lyra::backend::cpp
