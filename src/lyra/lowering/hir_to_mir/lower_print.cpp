#include "lyra/lowering/hir_to_mir/lower_print.hpp"

#include <cstddef>
#include <expected>
#include <format>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/runtime_print.hpp"
#include "lyra/support/format.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto SpecCharFor(support::FormatDirectiveKind k) -> std::string_view {
  switch (k) {
    case support::FormatDirectiveKind::kReal:
      return "f";
    case support::FormatDirectiveKind::kTime:
      return "t";
    case support::FormatDirectiveKind::kChar:
      return "c";
    default:
      return "?";
  }
}

auto ToMirPrintKind(const support::PrintSystemSubroutineInfo& info)
    -> mir::PrintKind {
  if (info.sink_kind == support::PrintSinkKind::kStdout) {
    return info.append_newline ? mir::PrintKind::kDisplay
                               : mir::PrintKind::kWrite;
  }
  return info.append_newline ? mir::PrintKind::kFDisplay
                             : mir::PrintKind::kFWrite;
}

auto ToMirFormatKind(support::FormatDirectiveKind k) -> mir::FormatKind {
  switch (k) {
    case support::FormatDirectiveKind::kDecimal:
      return mir::FormatKind::kDecimal;
    case support::FormatDirectiveKind::kHex:
      return mir::FormatKind::kHex;
    case support::FormatDirectiveKind::kBinary:
      return mir::FormatKind::kBinary;
    case support::FormatDirectiveKind::kOctal:
      return mir::FormatKind::kOctal;
    case support::FormatDirectiveKind::kString:
      return mir::FormatKind::kString;
    default:
      throw InternalError("ToMirFormatKind: not an integral/string kind");
  }
}

auto ToMirFormatModifiers(const support::FormatDirectiveModifiers& m)
    -> mir::FormatModifiers {
  return mir::FormatModifiers{
      .width = m.width,
      .precision = m.precision,
      .zero_pad = m.zero_pad,
      .left_align = m.left_align};
}

auto BuildPrintValueItem(
    const BodyLoweringState& body_state, hir::ExprId hir_arg,
    mir::FormatSpec spec) -> mir::RuntimePrintItem {
  const mir::ExprId value = body_state.TranslateExpr(hir_arg);
  const mir::TypeId type = body_state.Body().GetExprType(value);
  return mir::RuntimePrintValue(value, type, std::move(spec));
}

auto BuildPrintItemFromDirective(
    const BodyLoweringState& body_state,
    const support::ParsedFormatDirective& directive,
    std::span<const hir::ExprId> args, std::size_t& value_index,
    diag::SourceSpan span) -> diag::Result<mir::RuntimePrintItem> {
  switch (directive.kind) {
    case support::FormatDirectiveKind::kLiteral:
      return mir::RuntimePrintLiteral{.text = directive.literal};

    case support::FormatDirectiveKind::kModulePath:
      return diag::Unsupported(
          span, diag::DiagCode::kFormatModulePathNotImplemented,
          "format specifier %m is not implemented in this build",
          diag::UnsupportedCategory::kFeature);

    case support::FormatDirectiveKind::kReal:
    case support::FormatDirectiveKind::kTime:
    case support::FormatDirectiveKind::kChar:
      return diag::Unsupported(
          span, diag::DiagCode::kFormatSpecifierNotImplemented,
          std::format(
              "format specifier %{} is not implemented in this build",
              SpecCharFor(directive.kind)),
          diag::UnsupportedCategory::kFeature);

    case support::FormatDirectiveKind::kDecimal:
    case support::FormatDirectiveKind::kHex:
    case support::FormatDirectiveKind::kBinary:
    case support::FormatDirectiveKind::kOctal:
    case support::FormatDirectiveKind::kString: {
      if (value_index >= args.size()) {
        return diag::Error(
            span, diag::DiagCode::kDisplayMissingArg,
            "format string consumes more arguments than provided");
      }
      const hir::ExprId hir_arg = args[value_index++];
      return BuildPrintValueItem(
          body_state, hir_arg,
          mir::FormatSpec(
              ToMirFormatKind(directive.kind),
              ToMirFormatModifiers(directive.modifiers)));
    }
  }
  throw InternalError(
      "BuildPrintItemFromDirective: unreachable directive kind");
}

struct LiteralFormatStringRef {
  std::string_view text;
  diag::SourceSpan span;
};

auto TryGetHirStringLiteral(const hir::Process& proc, hir::ExprId expr_id)
    -> std::optional<LiteralFormatStringRef> {
  const auto& expr = proc.exprs.at(expr_id.value);
  const auto* primary = std::get_if<hir::PrimaryExpr>(&expr.data);
  if (primary == nullptr) return std::nullopt;
  const auto* sl = std::get_if<hir::StringLiteral>(&primary->data);
  if (sl == nullptr) return std::nullopt;
  return LiteralFormatStringRef{.text = sl->value, .span = expr.span};
}

auto BuildRuntimePrintItemsFromCallArgs(
    const BodyLoweringState& body_state, const hir::Process& hir_proc,
    const hir::CallExpr& call, diag::SourceSpan call_span)
    -> diag::Result<std::vector<mir::RuntimePrintItem>> {
  std::vector<mir::RuntimePrintItem> items;
  const auto& args = call.arguments;
  std::size_t cursor = 0;

  if (cursor < args.size()) {
    if (auto literal = TryGetHirStringLiteral(hir_proc, args[cursor])) {
      auto parsed_or =
          support::ParseLiteralFormatString(literal->text, literal->span);
      if (!parsed_or) return std::unexpected(std::move(parsed_or.error()));
      ++cursor;
      auto value_index = cursor;
      for (const auto& directive : *parsed_or) {
        auto item_or = BuildPrintItemFromDirective(
            body_state, directive, args, value_index, literal->span);
        if (!item_or) return std::unexpected(std::move(item_or.error()));
        items.push_back(std::move(*item_or));
      }
      cursor = value_index;
    }
  }

  while (cursor < args.size()) {
    if (!items.empty()) {
      items.emplace_back(mir::RuntimePrintLiteral{.text = " "});
    }
    items.push_back(BuildPrintValueItem(
        body_state, args[cursor],
        mir::FormatSpec(mir::FormatKind::kDecimal, mir::FormatModifiers{})));
    ++cursor;
  }
  (void)call_span;
  return items;
}

}  // namespace

auto LowerPrintSystemSubroutineCall(
    const UnitLoweringState& unit_state, const hir::Process& hir_proc,
    const BodyLoweringState& body_state, const hir::CallExpr& call,
    const support::SystemSubroutineDesc& desc,
    const support::PrintSystemSubroutineInfo& print, diag::SourceSpan span)
    -> diag::Result<mir::Expr> {
  if (print.sink_kind == support::PrintSinkKind::kFile) {
    return diag::Unsupported(
        span, diag::DiagCode::kFileDisplayNotImplemented,
        std::format(
            "{} is not implemented in this build", std::string{desc.name}),
        diag::UnsupportedCategory::kFeature);
  }
  auto items_or =
      BuildRuntimePrintItemsFromCallArgs(body_state, hir_proc, call, span);
  if (!items_or) return std::unexpected(std::move(items_or.error()));

  return mir::Expr{
      .data =
          mir::RuntimeCallExpr{
              .print = mir::RuntimePrintCall(
                  ToMirPrintKind(print), std::nullopt, std::move(*items_or))},
      .type = unit_state.Builtins().void_type};
}

}  // namespace lyra::lowering::hir_to_mir
