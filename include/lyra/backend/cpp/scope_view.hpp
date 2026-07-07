#pragma once

#include "lyra/base/internal_error.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/enclosing_hops.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/local_ref.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::backend::cpp {

// The rendering fold's walk position. The MIR-to-C++ emit is a fold, not a
// construction pass: it accumulates nothing and owns no output, so this
// carries only what reading a node needs -- the unit (for type and arena
// lookups), the enclosing class chain (for object-graph navigation), and the
// current callable code. A local reference resolves directly against the code's
// `locals` arena (a captured binding is a field access over the closure
// receiver, `locals[0]`), the read-only twin of how the construction-side
// `CallableBindings` declared them. Immutable and copied on
// descent (`WithBlock` / `WithClass` / `WithClosure`); it grows no member per
// concept, so it is not the forbidden growing `*Context`.
class ScopeView {
 public:
  static auto ForRoot(
      const mir::CompilationUnit& unit, const mir::Class& cls,
      const mir::CallableCode& code) -> ScopeView {
    return ScopeView{unit, cls, code, code.body, nullptr};
  }

  [[nodiscard]] auto WithBlock(const mir::Block& child) const -> ScopeView {
    return ScopeView{*unit_, *class_, *code_, child, class_parent_};
  }

  // Descend into a child class's callable. The child's body and locals replace
  // the current ones; this view becomes the child's enclosing-class link.
  [[nodiscard]] auto WithClass(
      const mir::Class& child_class, const mir::CallableCode& child_code) const
      -> ScopeView {
    return ScopeView{*unit_, child_class, child_code, child_code.body, this};
  }

  // Enter a closure's own code while staying in the same class context: a
  // closure runs against the same object, so the enclosing-class chain is
  // unchanged; only the local / capture arenas and the body block swap.
  [[nodiscard]] auto WithClosure(const mir::CallableCode& closure_code) const
      -> ScopeView {
    return ScopeView{
        *unit_, *class_, closure_code, closure_code.body, class_parent_};
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

  [[nodiscard]] auto Code() const -> const mir::CallableCode& {
    return *code_;
  }

  [[nodiscard]] auto Block() const -> const mir::Block& {
    return *block_;
  }

  // An activation local / parameter of the current callable, named directly by
  // its id in the callable's one `locals` arena.
  [[nodiscard]] auto Local(const mir::LocalRef& ref) const
      -> const mir::LocalDecl& {
    return code_->locals.Get(ref.var);
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
  // type: an object type names a local object declaration by identity, and the
  // unit's registry maps that identity to the declaration.
  [[nodiscard]] auto ClassByObjectType(mir::TypeId object_type) const
      -> const mir::Class& {
    const auto& obj =
        std::get<mir::ObjectType>(unit_->types.Get(object_type).data);
    return unit_->GetClass(obj.class_id);
  }

  [[nodiscard]] auto Expr(mir::ExprId id) const -> const mir::Expr& {
    return block_->exprs.Get(id);
  }

 private:
  ScopeView(
      const mir::CompilationUnit& unit, const mir::Class& cls,
      const mir::CallableCode& code, const mir::Block& block,
      const ScopeView* class_parent)
      : unit_(&unit),
        class_(&cls),
        code_(&code),
        block_(&block),
        class_parent_(class_parent) {
  }

  const mir::CompilationUnit* unit_;
  const mir::Class* class_;
  const mir::CallableCode* code_;
  const mir::Block* block_;
  const ScopeView* class_parent_;
};

}  // namespace lyra::backend::cpp
