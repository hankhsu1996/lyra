#pragma once

#include <string_view>

#include "lyra/base/internal_error.hpp"
#include "lyra/mir/block_hops.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/enclosing_hops.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"

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
    return ScopeView{*unit_, *class_,       child,
                     this,   class_parent_, self_spelling_};
  }

  [[nodiscard]] auto WithClass(
      const mir::Class& child_class, const mir::Block& root_block) const
      -> ScopeView {
    return ScopeView{*unit_,  child_class, root_block,
                     nullptr, this,        self_spelling_};
  }

  // How the receiver `self` (the body's `vars[0]`, MIR invariant 11) is spelled
  // in C++. A static method receives `self` as its first parameter, so it is
  // spelled `self`; a virtual instance method takes the receiver implicitly, so
  // it is spelled `this`. A nested block inherits the enclosing method's
  // spelling.
  [[nodiscard]] auto WithSelfSpelling(std::string_view spelling) const
      -> ScopeView {
    return ScopeView{*unit_,        *class_,       *block_,
                     block_parent_, class_parent_, spelling};
  }

  [[nodiscard]] auto SelfSpelling() const -> std::string_view {
    return self_spelling_;
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

  [[nodiscard]] auto Expr(mir::ExprId id) const -> const mir::Expr& {
    return block_->exprs.Get(id);
  }

 private:
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
      const ScopeView* class_parent, std::string_view self_spelling)
      : unit_(&unit),
        class_(&cls),
        block_(&block),
        block_parent_(block_parent),
        class_parent_(class_parent),
        self_spelling_(self_spelling) {
  }

  const mir::CompilationUnit* unit_;
  const mir::Class* class_;
  const mir::Block* block_;
  const ScopeView* block_parent_;
  const ScopeView* class_parent_;
  std::string_view self_spelling_ = "self";
};

}  // namespace lyra::backend::cpp
