#pragma once

#include "lyra/base/internal_error.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::backend::cpp {

// Where the render is in the MIR: the unit, the class the current function
// belongs to, the function, and the current block. It holds what an arm needs
// to look up a child node and nothing else, and a new one is made for each
// block or closure entered.
//
// A function of the unit's namespace belongs to no class, and a constant's
// value belongs to no function; asking for the missing one is a compiler bug.
class ScopeView {
 public:
  static auto ForRoot(
      const mir::CompilationUnit& unit, mir::ClassId cls_id,
      const mir::Class& cls, const mir::CallableCode& code) -> ScopeView {
    return ScopeView{unit, cls_id, &cls, &code, code.Body()};
  }

  // A function of the unit's namespace, which belongs to no class.
  static auto ForNamespace(
      const mir::CompilationUnit& unit, const mir::CallableCode& code)
      -> ScopeView {
    return ScopeView{unit, mir::ClassId{}, nullptr, &code, code.Body()};
  }

  // A constant's value: one expression, in no function, so it uses no local.
  // A class's constant may still name its class; a unit's names none.
  static auto ForClassConstant(
      const mir::CompilationUnit& unit, mir::ClassId cls_id,
      const mir::Class& cls, const mir::Block& block) -> ScopeView {
    return ScopeView{unit, cls_id, &cls, nullptr, block};
  }

  static auto ForUnitConstant(
      const mir::CompilationUnit& unit, const mir::Block& block) -> ScopeView {
    return ScopeView{unit, mir::ClassId{}, nullptr, nullptr, block};
  }

  [[nodiscard]] auto WithBlock(const mir::Block& child) const -> ScopeView {
    return ScopeView{*unit_, class_id_, class_, code_, child};
  }

  // A closure's body. It stays in the same class, since a closure runs against
  // the same object, and switches to the closure's own locals and block.
  [[nodiscard]] auto WithClosure(const mir::CallableCode& closure_code) const
      -> ScopeView {
    return ScopeView{
        *unit_, class_id_, class_, &closure_code, closure_code.Body()};
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

  // The id of the current class, which is what names a class the source did
  // not declare, `sv_scope_<n>`.
  [[nodiscard]] auto ClassId() const -> mir::ClassId {
    if (class_ == nullptr) {
      throw InternalError(
          "ScopeView::ClassId: a namespace-owned callable belongs to no class");
    }
    return class_id_;
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

  [[nodiscard]] auto Expr(mir::ExprId id) const -> const mir::Expr& {
    return block_->exprs.Get(id);
  }

 private:
  ScopeView(
      const mir::CompilationUnit& unit, mir::ClassId cls_id,
      const mir::Class* cls, const mir::CallableCode* code,
      const mir::Block& block)
      : unit_(&unit),
        class_id_(cls_id),
        class_(cls),
        code_(code),
        block_(&block) {
  }

  const mir::CompilationUnit* unit_;
  mir::ClassId class_id_;
  const mir::Class* class_;
  const mir::CallableCode* code_;
  const mir::Block* block_;
};

}  // namespace lyra::backend::cpp
