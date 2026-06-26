#pragma once

#include "lyra/base/internal_error.hpp"
#include "lyra/mir/block_hops.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/enclosing_hops.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::backend::cpp {

// The rendering fold's walk position. The MIR-to-C++ emit is a fold, not a
// construction pass: it accumulates nothing and owns no output, so this
// carries only what reading a node needs -- the unit (for type and arena
// lookups) and the chain of enclosing scopes. A hops-relative reference
// resolves by climbing that chain, exactly as the construction-side
// `WalkFrame` resolves it; this is the read-only twin of that frame, thin
// because every decision is already baked into the MIR it reads. Immutable
// and copied on descent (`WithBlock` / `WithClass`); it grows no member per
// concept, so it is not the forbidden growing `*Context`.
class ScopeView {
 public:
  static auto ForRoot(
      const mir::CompilationUnit& unit, const mir::Class& cls,
      const mir::Block& block) -> ScopeView {
    return ScopeView{unit, cls, block};
  }

  [[nodiscard]] auto WithBlock(const mir::Block& child) const -> ScopeView {
    return ScopeView{*unit_, *class_, child, this, class_parent_};
  }

  [[nodiscard]] auto WithClass(
      const mir::Class& child_class, const mir::Block& root_block) const
      -> ScopeView {
    return ScopeView{*unit_, child_class, root_block, nullptr, this};
  }

  ScopeView(const ScopeView&) = delete;
  auto operator=(const ScopeView&) -> ScopeView& = delete;
  ScopeView(ScopeView&&) = delete;
  auto operator=(ScopeView&&) -> ScopeView& = delete;
  ~ScopeView() = default;

  [[nodiscard]] auto Unit() const -> const mir::CompilationUnit& {
    return *unit_;
  }

  [[nodiscard]] auto Class() const -> const mir::Class& {
    return *class_;
  }

  [[nodiscard]] auto Block() const -> const mir::Block& {
    return *block_;
  }

  [[nodiscard]] auto BlockAtHops(mir::BlockHops hops) const
      -> const mir::Block& {
    if (hops.value == 0) {
      return *block_;
    }
    if (block_parent_ == nullptr) {
      throw InternalError("ScopeView::BlockAtHops: hops out of range");
    }
    return block_parent_->BlockAtHops(mir::BlockHops{.value = hops.value - 1});
  }

  [[nodiscard]] auto EnclosingClassAtHops(mir::EnclosingHops hops) const
      -> const mir::Class& {
    if (hops.value == 0) {
      return *class_;
    }
    if (class_parent_ == nullptr) {
      throw InternalError("ScopeView::EnclosingClassAtHops: hops out of range");
    }
    return class_parent_->EnclosingClassAtHops(
        mir::EnclosingHops{.value = hops.value - 1});
  }

  // The class a member access reaches, resolved from the receiver's object
  // type: the member belongs to the receiver's class. A receiver is `self`
  // (the current class), an `(Outer*)self->Parent()` navigation (an enclosing
  // class), or a handle to a nested object (a nested class). Searched among the
  // current class, its nested classes, and its enclosing chain -- the classes
  // reachable from this render position.
  [[nodiscard]] auto ClassByObjectType(mir::TypeId object_type) const
      -> const mir::Class& {
    if (ObjectTypeOf(*class_) == object_type) {
      return *class_;
    }
    for (const auto& nested : class_->nested_classes) {
      if (ObjectTypeOf(nested) == object_type) {
        return nested;
      }
    }
    if (class_parent_ != nullptr) {
      return class_parent_->ClassByObjectType(object_type);
    }
    throw InternalError("ScopeView::ClassByObjectType: no class for type");
  }

  [[nodiscard]] auto Expr(mir::ExprId id) const -> const mir::Expr& {
    return block_->exprs.Get(id);
  }

 private:
  // The object type a class instance has -- the pointee of its self pointer.
  [[nodiscard]] auto ObjectTypeOf(const mir::Class& cls) const -> mir::TypeId {
    return std::get<mir::PointerType>(
               unit_->types.Get(cls.self_pointer_type).data)
        .pointee;
  }

  ScopeView(
      const mir::CompilationUnit& unit, const mir::Class& cls,
      const mir::Block& block)
      : unit_(&unit),
        class_(&cls),
        block_(&block),
        block_parent_(nullptr),
        class_parent_(nullptr) {
  }

  ScopeView(
      const mir::CompilationUnit& unit, const mir::Class& cls,
      const mir::Block& block, const ScopeView* block_parent,
      const ScopeView* class_parent)
      : unit_(&unit),
        class_(&cls),
        block_(&block),
        block_parent_(block_parent),
        class_parent_(class_parent) {
  }

  const mir::CompilationUnit* unit_;
  const mir::Class* class_;
  const mir::Block* block_;
  const ScopeView* block_parent_;
  const ScopeView* class_parent_;
};

}  // namespace lyra::backend::cpp
