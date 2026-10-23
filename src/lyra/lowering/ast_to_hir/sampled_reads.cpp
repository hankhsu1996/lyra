#include "lyra/lowering/ast_to_hir/sampled_reads.hpp"

#include <expected>
#include <utility>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/ParameterSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/ast_to_hir/expression/references.hpp"
#include "lyra/lowering/ast_to_hir/sensitivity.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// How one read answers for its part of an expression's sampled value (LRM
// 16.5.1).
enum class SampledAnswer {
  // An automatic variable answers with what it holds now, and so does a `ref`
  // formal that is not `ref static`, being usable only where an automatic
  // variable is (LRM 13.5.2).
  kCurrent,
  // A constant is no variable: its value is the same at every time, so it is
  // its own sampled value and nothing has to answer for it.
  kFixed,
  // Anything else answers from a cell this scope arms, and so does a constant
  // read from a cell, which is read as a variable is.
  kArmed,
};

auto SampledAnswerOf(
    const UnitLowerer& unit_lowerer, const slang::ast::ValueSymbol& symbol,
    diag::SourceSpan span) -> diag::Result<SampledAnswer> {
  const auto* var = symbol.as_if<slang::ast::VariableSymbol>();
  // A property of an object has a lifetime too, but it is the object's rather
  // than a call's, and the clause does not reach it.
  if (var != nullptr && symbol.kind != slang::ast::SymbolKind::ClassProperty) {
    if (var->flags.has(slang::ast::VariableFlags::RefStatic)) {
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedExpressionForm,
          "a sampled value of a 'ref static' argument is not yet supported");
    }
    if (var->lifetime == slang::ast::VariableLifetime::Automatic) {
      return SampledAnswer::kCurrent;
    }
  }
  auto resolved = ResolveReferent(symbol, span);
  if (!resolved) return std::unexpected(std::move(resolved.error()));
  switch (resolved->kind) {
    case Referent::kParameterConstant:
      return unit_lowerer.ReadsParameterFromCell(
                 resolved->symbol->as<slang::ast::ParameterSymbol>())
                 ? SampledAnswer::kArmed
                 : SampledAnswer::kFixed;
    case Referent::kEnumConstant:
    case Referent::kSpecparam:
      return SampledAnswer::kFixed;
    case Referent::kVariableStorage:
    case Referent::kNetStorage:
    // What names no cell this scope can reach is still a read that has to
    // answer, so it goes to arming and is refused there.
    case Referent::kClassProperty:
    case Referent::kThisHandle:
    case Referent::kPatternBinding:
    case Referent::kViewDefinedName:
    case Referent::kPrimitivePort:
    case Referent::kClockingSignal:
    case Referent::kAssertionLocal:
    case Referent::kStructureMember:
      return SampledAnswer::kArmed;
    case Referent::kNotAValue:
      break;
  }
  throw InternalError(
      "SampledAnswerOf: a read resolved to a declaration that denotes no "
      "value");
}

}  // namespace

auto ArmSampledReads(
    UnitLowerer& unit_lowerer, const WalkFrame& frame,
    const slang::ast::Expression& expr, diag::SourceSpan span)
    -> diag::Result<SampledReads> {
  if (frame.reader_scope == nullptr) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "a sampled value read outside a design element's scope is not yet "
        "supported");
  }
  const auto& reads = unit_lowerer.Sensitivity().AnalyzeReads(
      expr, frame.reader_scope->asSymbol());
  std::vector<AccessedPart> armed;
  SampledReads counted;
  for (const AccessedPart& read : reads) {
    auto answer = SampledAnswerOf(unit_lowerer, *read.symbol, span);
    if (!answer) return std::unexpected(std::move(answer.error()));
    switch (*answer) {
      case SampledAnswer::kCurrent:
        ++counted.current;
        break;
      case SampledAnswer::kFixed:
        break;
      case SampledAnswer::kArmed:
        armed.push_back(read);
        break;
    }
  }
  counted.armed = armed.size();
  if (armed.empty()) return counted;
  if (frame.current_structural_scope == nullptr) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "a sampled value read outside a design element's scope is not yet "
        "supported");
  }
  auto cells = unit_lowerer.CellsRead(armed, frame);
  if (!cells) return std::unexpected(std::move(cells.error()));
  // Every read left here has to answer from a cell, so a read the translation
  // could not name as a cell of this design leaves one that can never answer.
  // The reachable case is a property of an object (LRM 8.4), which no scope
  // holds a cell of.
  if (cells->size() < armed.size()) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "a sampled value of storage this design element does not hold is "
        "not yet supported");
  }
  std::vector<hir::ValueTarget>& sampled =
      frame.current_structural_scope->sampled_cells;
  for (hir::ValueTarget& cell : *cells) {
    sampled.push_back(std::move(cell));
  }
  return counted;
}

}  // namespace lyra::lowering::ast_to_hir
