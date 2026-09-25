#pragma once

#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::backend::cpp {

// Where the render is in the MIR: the unit, the function, and the current
// block. It holds what an arm needs to look up a child node, and where to
// report a node this target has no form for; a new one is made for each block
// entered.
//
// A constant's value belongs to no function; asking for one is a compiler bug.
class ScopeView {
 public:
  // A function's body, whatever owns the function.
  static auto ForCode(
      const mir::CompilationUnit& unit, const mir::CallableCode& code,
      diag::DiagnosticSink& refusals) -> ScopeView {
    return ScopeView{unit, &code, code.Body(), refusals};
  }

  // A constant's value: one expression, in no function, so it uses no local.
  static auto ForConstant(
      const mir::CompilationUnit& unit, const mir::Block& block,
      diag::DiagnosticSink& refusals) -> ScopeView {
    return ScopeView{unit, nullptr, block, refusals};
  }

  [[nodiscard]] auto WithBlock(const mir::Block& child) const -> ScopeView {
    return ScopeView{*unit_, code_, child, *refusals_};
  }

  ScopeView(const ScopeView&) = delete;
  auto operator=(const ScopeView&) -> ScopeView& = delete;
  ScopeView(ScopeView&&) = delete;
  auto operator=(ScopeView&&) -> ScopeView& = delete;
  ~ScopeView() = default;

  [[nodiscard]] auto Unit() const -> const mir::CompilationUnit& {
    return *unit_;
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

  // Reports a node this target has no form for, and goes on: no entry reads
  // another's text, so nothing written after it depends on what this one did
  // not write, and a unit that reported anything is not built.
  void Refuse(diag::Diagnostic refusal) const {
    refusals_->Report(std::move(refusal));
  }

 private:
  ScopeView(
      const mir::CompilationUnit& unit, const mir::CallableCode* code,
      const mir::Block& block, diag::DiagnosticSink& refusals)
      : unit_(&unit), code_(code), block_(&block), refusals_(&refusals) {
  }

  const mir::CompilationUnit* unit_;
  const mir::CallableCode* code_;
  const mir::Block* block_;
  diag::DiagnosticSink* refusals_;
};

}  // namespace lyra::backend::cpp
