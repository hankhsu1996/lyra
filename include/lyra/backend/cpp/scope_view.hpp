#pragma once

#include <set>
#include <utility>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/failure_context.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/constant_id.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/linkage.hpp"

namespace lyra::backend::cpp {

// What writing a unit's bodies leaves behind beside their text: each node this
// target has no form for, and each constant of the unit the text names. A name
// needs a definition in the file that uses it, so the named ones are the ones
// the unit's code file defines.
struct UnitRenderReport {
  diag::DiagnosticSink* refusals;
  // The piece of source the innermost node being written was lowered from,
  // which a refusal naming none of its own stands at.
  diag::SourceSpan span{};
  std::set<mir::ConstantId> constants_named;
};

// Where the render is in the MIR: the unit, the function, the current block,
// and how what is being written is linked. It holds what an arm needs to look
// up a child node, and where to report what writing a node leaves behind; a new
// one is made for each block entered.
class ScopeView {
 public:
  // A function's body, whatever owns the function, defined under `linkage`.
  static auto ForCode(
      const mir::CompilationUnit& unit, const mir::CallableCode& code,
      support::Linkage linkage, UnitRenderReport& report) -> ScopeView {
    return ScopeView{unit, code, code.Body(), linkage, report};
  }

  [[nodiscard]] auto WithBlock(const mir::Block& child) const -> ScopeView {
    return ScopeView{*unit_, *code_, child, linkage_, *report_};
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
    return *code_;
  }

  [[nodiscard]] auto Block() const -> const mir::Block& {
    return *block_;
  }

  [[nodiscard]] auto Expr(mir::ExprId id) const -> const mir::Expr& {
    return block_->exprs.Get(id);
  }

  // How the definition being written is linked, which decides whether a
  // constant it reads is named by the code file or spelled in place.
  [[nodiscard]] auto Linkage() const -> support::Linkage {
    return linkage_;
  }

  // Reports a node this target has no form for, and goes on: no entry reads
  // another's text, so nothing written after it depends on what this one did
  // not write, and a unit that reported anything is not built.
  void Refuse(diag::Diagnostic refusal) const {
    report_->refusals->Report(
        diag::PlacedAt(std::move(refusal), report_->span));
  }

  // Writes `span`'s node: what is refused or fails meanwhile stands there.
  [[nodiscard]] auto WritingFrom(diag::SourceSpan span) const
      -> diag::ApplySourceSpan {
    return {report_->span, span};
  }

  // Where to report what writing a node leaves behind.
  [[nodiscard]] auto Report() const -> UnitRenderReport& {
    return *report_;
  }

 private:
  ScopeView(
      const mir::CompilationUnit& unit, const mir::CallableCode& code,
      const mir::Block& block, support::Linkage linkage,
      UnitRenderReport& report)
      : unit_(&unit),
        code_(&code),
        block_(&block),
        linkage_(linkage),
        report_(&report) {
  }

  const mir::CompilationUnit* unit_;
  const mir::CallableCode* code_;
  const mir::Block* block_;
  support::Linkage linkage_;
  UnitRenderReport* report_;
};

}  // namespace lyra::backend::cpp
