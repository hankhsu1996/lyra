#pragma once

#include "lyra/base/internal_error.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/compilation_unit.hpp"
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
// lookups), the class the current code belongs to, and the current callable
// code. A local reference resolves directly against the code's `locals` arena
// (a captured binding is a field access over the closure receiver,
// `locals[0]`), the read-only twin of how the construction side declared them.
// Immutable and copied on descent; it grows no member per concept, so it is not
// the forbidden growing `*Context`.
//
// The class is absent for a callable the unit's namespace owns rather than a
// class (a receiver-less package callable): it has no object graph to navigate,
// so the object-navigation accessors do not apply. Type-name resolution does
// not go through the class at all -- a nominal type name resolves against the
// unit -- so it works with or without one.
class ScopeView {
 public:
  static auto ForRoot(
      const mir::CompilationUnit& unit, const mir::Class& cls,
      const mir::CallableCode& code) -> ScopeView {
    return ScopeView{unit, &cls, &code, code.Body()};
  }

  // A callable the unit's namespace owns directly, belonging to no class.
  static auto ForNamespace(
      const mir::CompilationUnit& unit, const mir::CallableCode& code)
      -> ScopeView {
    return ScopeView{unit, nullptr, &code, code.Body()};
  }

  // A constant's initializer: an expression tree with no enclosing callable,
  // so it names no local. One a class owns still resolves names against that
  // class, which its initializer may reach; one the unit owns reaches no
  // class.
  static auto ForClassConstant(
      const mir::CompilationUnit& unit, const mir::Class& cls,
      const mir::Block& block) -> ScopeView {
    return ScopeView{unit, &cls, nullptr, block};
  }

  static auto ForUnitConstant(
      const mir::CompilationUnit& unit, const mir::Block& block) -> ScopeView {
    return ScopeView{unit, nullptr, nullptr, block};
  }

  [[nodiscard]] auto WithBlock(const mir::Block& child) const -> ScopeView {
    return ScopeView{*unit_, class_, code_, child};
  }

  // Enter a closure's own code while staying in the same class context: a
  // closure runs against the same object, so the class is unchanged; only the
  // local / capture arenas and the body block swap.
  [[nodiscard]] auto WithClosure(const mir::CallableCode& closure_code) const
      -> ScopeView {
    return ScopeView{*unit_, class_, &closure_code, closure_code.Body()};
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
    if (class_ == nullptr) {
      throw InternalError(
          "ScopeView::Class: a namespace-owned callable belongs to no class");
    }
    return *class_;
  }

  [[nodiscard]] auto Code() const -> const mir::CallableCode& {
    if (code_ == nullptr) {
      throw InternalError(
          "ScopeView::Code: a constant initializer has no enclosing callable");
    }
    return *code_;
  }

  [[nodiscard]] auto Block() const -> const mir::Block& {
    return *block_;
  }

  // An activation local / parameter of the current callable, named directly by
  // its id in the callable's one `locals` arena.
  [[nodiscard]] auto Local(const mir::LocalRef& ref) const
      -> const mir::LocalDecl& {
    return Code().locals.Get(ref.var);
  }

  // The class a member access reaches, resolved from the receiver's object
  // type: an object type names a local object declaration by identity, and the
  // unit's registry maps that identity to the declaration.
  [[nodiscard]] auto ClassByObjectType(mir::TypeId object_type) const
      -> const mir::Class& {
    const auto& obj = unit_->types.Get(object_type).Get<mir::ObjectType>();
    return unit_->GetClass(obj.class_id);
  }

  [[nodiscard]] auto Expr(mir::ExprId id) const -> const mir::Expr& {
    return block_->exprs.Get(id);
  }

 private:
  ScopeView(
      const mir::CompilationUnit& unit, const mir::Class* cls,
      const mir::CallableCode* code, const mir::Block& block)
      : unit_(&unit), class_(cls), code_(code), block_(&block) {
  }

  const mir::CompilationUnit* unit_;
  const mir::Class* class_;
  const mir::CallableCode* code_;
  const mir::Block* block_;
};

}  // namespace lyra::backend::cpp
