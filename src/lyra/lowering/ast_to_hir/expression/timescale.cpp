#include "lyra/lowering/ast_to_hir/expression/timescale.hpp"

#include <cstdint>
#include <expected>
#include <format>
#include <optional>
#include <string>
#include <utility>
#include <variant>

#include <slang/ast/HierarchicalReference.h>
#include <slang/ast/Scope.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/SystemSubroutine.h>
#include <slang/ast/expressions/CallExpression.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/numeric/ConstantValue.h>
#include <slang/numeric/SVInt.h>
#include <slang/numeric/Time.h>
#include <slang/parsing/KnownSystemName.h>
#include <slang/syntax/AllSyntax.h>
#include <slang/syntax/SyntaxPrinter.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/time.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/lowering/ast_to_hir/constant_value.hpp"
#include "lyra/lowering/ast_to_hir/time_resolution.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

using slang::parsing::KnownSystemName;

// The name the call wrote for the instance it names, which is what the
// clause's own example prints for one named by a path (LRM 20.4.2).
auto WrittenName(const slang::ast::CallExpression& call) -> std::string {
  const auto* invocation =
      call.syntax == nullptr
          ? nullptr
          : call.syntax->as_if<slang::syntax::InvocationExpressionSyntax>();
  if (invocation == nullptr || invocation->arguments == nullptr ||
      invocation->arguments->parameters.empty()) {
    throw InternalError(
        "LowerTimescaleCall: a `$printtimescale` naming a design element is a "
        "call written with that name");
  }
  const auto* written = invocation->arguments->parameters[0]
                            ->as_if<slang::syntax::OrderedArgumentSyntax>();
  if (written == nullptr) {
    throw InternalError(
        "LowerTimescaleCall: a `$printtimescale` names its design element by "
        "position");
  }
  return slang::syntax::SyntaxPrinter{}
      .setIncludeTrivia(false)
      .print(*written->expr)
      .str();
}

struct NamedTimeScale {
  std::string name;
  slang::TimeScale scale;
};

// The element a timescale function or task names and the time unit and
// precision its declaration states (LRM 20.4.1, 20.4.2): the design element the
// call is written in where it names none, `$unit`, or an instance this one's
// own hierarchy reaches downward. Each is fixed by the source of the unit the
// call is compiled in, so every elaboration of that unit answers alike. A scope
// that states none has the default.
//
// `$root` answers the finest precision of the whole design (LRM 3.14.3), and an
// instance reached upward or from the top is whichever the design puts there;
// neither is fixed by this unit, so each is read where the run knows it, which
// is not yet supported.
auto TimeScaleNamedBy(
    const UnitLowerer& unit_lowerer, const slang::ast::CallExpression& call,
    diag::SourceSpan span) -> diag::Result<NamedTimeScale> {
  const auto& info =
      std::get<slang::ast::CallExpression::SystemCallInfo>(call.subroutine);
  if (call.arguments().empty()) {
    return NamedTimeScale{
        .name = unit_lowerer.Unit().source_name,
        .scale = info.scope->getTimeScale().value_or(slang::TimeScale{})};
  }
  const auto& named =
      call.arguments().front()->as<slang::ast::ArbitrarySymbolExpression>();
  if (const auto* unit =
          named.symbol->as_if<slang::ast::CompilationUnitSymbol>()) {
    return NamedTimeScale{
        .name = "$unit", .scale = unit->timeScale.value_or(slang::TimeScale{})};
  }
  const auto* instance = named.symbol->as_if<slang::ast::InstanceSymbol>();
  const bool from_the_top =
      !named.hierRef.path.empty() &&
      named.hierRef.path.front().symbol->as_if<slang::ast::RootSymbol>() !=
          nullptr;
  if (instance != nullptr && !named.hierRef.isUpward() && !from_the_top) {
    return NamedTimeScale{
        .name = WrittenName(call),
        .scale = instance->body.getTimeScale().value_or(slang::TimeScale{})};
  }
  if (instance != nullptr ||
      named.symbol->as_if<slang::ast::RootSymbol>() != nullptr) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedSubroutineArgument,
        "'{}' naming '$root', or an instance reached upward or from the top, "
        "is not yet supported",
        call.getSubroutineName());
  }
  throw InternalError(
      "LowerTimescaleCall: the front end binds a timescale argument to an "
      "instance, `$unit` or `$root`");
}

// `$timeunit` or `$timeprecision` as the power of ten its element's declaration
// states (Table 20-2), in every context alike (LRM 11.2.1).
auto LowerTimescaleQuery(
    UnitLowerer& unit_lowerer, WalkFrame frame,
    const slang::ast::CallExpression& call, bool unit, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto named = TimeScaleNamedBy(unit_lowerer, call, span);
  if (!named) return std::unexpected(std::move(named.error()));
  const TimeResolution resolution = ResolveTimeResolution(named->scale);
  const std::int8_t power =
      unit ? resolution.unit_power : resolution.precision_power;
  auto type_id = unit_lowerer.InternExpressionType(call, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return MakeConstantValueExpr(
      unit_lowerer, frame,
      slang::ConstantValue{
          slang::SVInt(32, static_cast<std::uint64_t>(power), true)},
      *type_id, span);
}

// `$printtimescale` as the write of its line, whose every part the
// elaboration settles (LRM 20.4.2).
auto LowerPrintTimescale(
    UnitLowerer& unit_lowerer, WalkFrame frame,
    const slang::ast::CallExpression& call, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto named = TimeScaleNamedBy(unit_lowerer, call, span);
  if (!named) return std::unexpected(std::move(named.error()));
  const std::string line = std::format(
      "Time scale of ({}) is {}", named->name, named->scale.toString());
  const hir::TypeId string_type =
      unit_lowerer.Unit().types.Intern(hir::Type{hir::StringType{}});
  auto line_expr = MakeConstantValueExpr(
      unit_lowerer, frame, slang::ConstantValue{line}, string_type, span);
  if (!line_expr) return std::unexpected(std::move(line_expr.error()));
  auto type_id = unit_lowerer.InternExpressionType(call, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  const auto* desc = support::FindSystemSubroutine(call.getSubroutineName());
  if (desc == nullptr) {
    throw InternalError(
        "LowerTimescaleCall: `$printtimescale` is in the system subroutine "
        "registry");
  }
  return hir::Expr{
      .type = *type_id,
      .data =
          hir::CallExpr{
              .callee = hir::SystemSubroutineRef{.id = desc->id},
              .arguments = {frame.Exprs().Add(*std::move(line_expr))}},
      .span = span,
  };
}

}  // namespace

auto LowerTimescaleCall(
    UnitLowerer& unit_lowerer, WalkFrame frame,
    const slang::ast::CallExpression& call, diag::SourceSpan span)
    -> diag::Result<std::optional<hir::Expr>> {
  if (!call.isSystemCall()) return std::optional<hir::Expr>{};
  const KnownSystemName known =
      std::get<slang::ast::CallExpression::SystemCallInfo>(call.subroutine)
          .subroutine->knownNameId;
  const bool queries = known == KnownSystemName::TimeUnit ||
                       known == KnownSystemName::TimePrecision;
  if (!queries && known != KnownSystemName::PrintTimeScale) {
    return std::optional<hir::Expr>{};
  }
  auto lowered = queries ? LowerTimescaleQuery(
                               unit_lowerer, frame, call,
                               known == KnownSystemName::TimeUnit, span)
                         : LowerPrintTimescale(unit_lowerer, frame, call, span);
  if (!lowered) return std::unexpected(std::move(lowered.error()));
  return std::optional<hir::Expr>{*std::move(lowered)};
}

}  // namespace lyra::lowering::ast_to_hir
