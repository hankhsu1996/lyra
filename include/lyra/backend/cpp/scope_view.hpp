#pragma once

#include <set>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/failure_context.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/enum_table_id.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/integral_constant_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/linkage.hpp"

namespace lyra::backend::cpp {

// What writing a unit's bodies leaves behind beside their text: each node this
// target has no form for, and each constant and each enumeration member table
// of the unit the text names. A name needs a definition in the file that uses
// it, so the named ones are the ones the unit's code file defines.
struct UnitRenderReport {
  diag::DiagnosticSink* refusals;
  // The piece of source the innermost node being written was lowered from,
  // which a refusal naming none of its own stands at.
  diag::SourceSpan span{};
  std::set<mir::IntegralConstantId> constants_named;
  std::set<mir::EnumTableId> enum_tables_named;
};

// Where the render is in the MIR: the unit, the function, the current block,
// and how what is being written is linked. It holds what an arm needs to look
// up a child node, and where to report what writing a node leaves behind; a new
// one is made for each block entered.
//
// A constant's value belongs to no function; asking for one is a compiler bug.
class ScopeView {
 public:
  // A function's body, whatever owns the function, defined under `linkage`.
  static auto ForCode(
      const mir::CompilationUnit& unit, const mir::CallableCode& code,
      support::Linkage linkage, UnitRenderReport& report) -> ScopeView {
    return ScopeView{unit, &code, code.Body(), linkage, report};
  }

  // A constant's value: one expression, in no function, so it uses no local.
  static auto ForConstant(
      const mir::CompilationUnit& unit, const mir::Block& block,
      support::Linkage linkage, UnitRenderReport& report) -> ScopeView {
    return ScopeView{unit, nullptr, block, linkage, report};
  }

  [[nodiscard]] auto WithBlock(const mir::Block& child) const -> ScopeView {
    return ScopeView{*unit_, code_, child, linkage_, *report_};
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

  // How the definition being written is linked, which decides whether a
  // constant it reads is named or written as its value.
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

  // Reports that the text being written names `constant`.
  void NameConstant(mir::IntegralConstantId constant) const {
    report_->constants_named.insert(constant);
  }

  // Reports that the text being written names `table`.
  void NameEnumTable(mir::EnumTableId table) const {
    report_->enum_tables_named.insert(table);
  }

 private:
  ScopeView(
      const mir::CompilationUnit& unit, const mir::CallableCode* code,
      const mir::Block& block, support::Linkage linkage,
      UnitRenderReport& report)
      : unit_(&unit),
        code_(code),
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
