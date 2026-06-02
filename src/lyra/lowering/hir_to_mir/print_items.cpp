#include "lyra/lowering/hir_to_mir/print_items.hpp"

#include <cstddef>
#include <expected>
#include <format>
#include <optional>
#include <span>
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
#include "lyra/hir/procedural_body.hpp"
#include "lyra/lowering/hir_to_mir/lower_expr.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/conversion.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/runtime_print.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/format.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto SpecCharFor(support::FormatDirectiveKind k) -> std::string_view {
  switch (k) {
    case support::FormatDirectiveKind::kTime:
      return "t";
    default:
      return "?";
  }
}

auto ToValueFormatKind(support::FormatDirectiveKind k) -> value::FormatKind {
  switch (k) {
    case support::FormatDirectiveKind::kDecimal:
      return value::FormatKind::kDecimal;
    case support::FormatDirectiveKind::kHex:
      return value::FormatKind::kHex;
    case support::FormatDirectiveKind::kBinary:
      return value::FormatKind::kBinary;
    case support::FormatDirectiveKind::kOctal:
      return value::FormatKind::kOctal;
    case support::FormatDirectiveKind::kString:
      return value::FormatKind::kString;
    case support::FormatDirectiveKind::kChar:
      return value::FormatKind::kChar;
    case support::FormatDirectiveKind::kRealDecimal:
      return value::FormatKind::kRealDecimal;
    case support::FormatDirectiveKind::kRealExponential:
      return value::FormatKind::kRealExponential;
    case support::FormatDirectiveKind::kRealGeneral:
      return value::FormatKind::kRealGeneral;
    case support::FormatDirectiveKind::kAssignmentPattern:
      return value::FormatKind::kAssignmentPattern;
    default:
      throw InternalError("ToValueFormatKind: not a value-format kind");
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
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_proc, hir::ExprId hir_arg,
    mir::FormatSpec spec) -> diag::Result<mir::RuntimePrintItem> {
  auto lowered_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
      hir_proc.exprs.at(hir_arg.value));
  if (!lowered_or) return std::unexpected(std::move(lowered_or.error()));
  mir::Expr lowered = *std::move(lowered_or);

  // %s consumes a runtime String. Slang types SV string literals as packed
  // bit vectors (LRM 5.9), so a `$display("%s", "hello")` arg arrives here
  // with a packed-array type. Insert an implicit ConversionExpr targeting
  // StringType so the backend's RuntimePrintValue rendering stays purely
  // type-driven. Non-literal integral operands also flow through this same
  // path -- when slang's bit-vector-to-string conversion lands at the
  // operand level, this becomes a no-op; until then the conversion is the
  // single chokepoint that records the semantic.
  if (spec.kind == value::FormatKind::kString &&
      unit_state.GetType(lowered.type).Kind() != mir::TypeKind::kString) {
    const mir::ExprId inner = proc_scope_state.AddExpr(std::move(lowered));
    lowered = mir::Expr{
        .data =
            mir::ConversionExpr{
                .operand = inner, .kind = mir::ConversionKind::kImplicit},
        .type = unit_state.Builtins().string};
  }

  const mir::TypeId type = lowered.type;
  const mir::ExprId value = proc_scope_state.AddExpr(std::move(lowered));
  return mir::RuntimePrintValue(value, type, std::move(spec));
}

auto BuildPrintItemFromDirective(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_proc,
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

    case support::FormatDirectiveKind::kTime:
      return diag::Unsupported(
          span, diag::DiagCode::kFormatSpecifierNotImplemented,
          std::format(
              "format specifier %{} is not implemented in this build",
              SpecCharFor(directive.kind)),
          diag::UnsupportedCategory::kFeature);

    case support::FormatDirectiveKind::kChar:
    case support::FormatDirectiveKind::kDecimal:
    case support::FormatDirectiveKind::kHex:
    case support::FormatDirectiveKind::kBinary:
    case support::FormatDirectiveKind::kOctal:
    case support::FormatDirectiveKind::kString:
    case support::FormatDirectiveKind::kRealDecimal:
    case support::FormatDirectiveKind::kRealExponential:
    case support::FormatDirectiveKind::kRealGeneral:
    case support::FormatDirectiveKind::kAssignmentPattern: {
      if (value_index >= args.size()) {
        return diag::Error(
            span, diag::DiagCode::kDisplayMissingArg,
            "format string consumes more arguments than provided");
      }
      const hir::ExprId hir_arg = args[value_index++];
      return BuildPrintValueItem(
          unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
          hir_arg,
          mir::FormatSpec(
              ToValueFormatKind(directive.kind),
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

auto TryGetHirStringLiteral(
    const hir::ProceduralBody& proc, hir::ExprId expr_id)
    -> std::optional<LiteralFormatStringRef> {
  const auto& expr = proc.exprs.at(expr_id.value);
  const auto* primary = std::get_if<hir::PrimaryExpr>(&expr.data);
  if (primary == nullptr) return std::nullopt;
  const auto* sl = std::get_if<hir::StringLiteral>(&primary->data);
  if (sl == nullptr) return std::nullopt;
  return LiteralFormatStringRef{.text = sl->value, .span = expr.span};
}

}  // namespace

auto BuildRuntimePrintItemsFromCallArgs(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_proc, const hir::CallExpr& call,
    diag::SourceSpan call_span)
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
            unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
            directive, args, value_index, literal->span);
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
    auto item_or = BuildPrintValueItem(
        unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
        args[cursor],
        mir::FormatSpec(value::FormatKind::kDecimal, mir::FormatModifiers{}));
    if (!item_or) return std::unexpected(std::move(item_or.error()));
    items.push_back(*std::move(item_or));
    ++cursor;
  }
  (void)call_span;
  return items;
}

}  // namespace lyra::lowering::hir_to_mir
