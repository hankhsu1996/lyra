#include "lyra/lowering/hir_to_mir/print_items.hpp"

#include <cstddef>
#include <expected>
#include <optional>
#include <span>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/runtime_print.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/format.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

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
    case support::FormatDirectiveKind::kTime:
      return value::FormatKind::kTime;
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
    ProcessLowerer& process, WalkFrame frame, hir::ExprId hir_arg,
    mir::FormatSpec spec) -> diag::Result<mir::RuntimePrintItem> {
  const auto& hir_proc = process.HirBody();
  auto& block = *frame.current_block;
  auto lowered_or = process.LowerExpr(hir_proc.exprs.Get(hir_arg), frame);
  if (!lowered_or) return std::unexpected(std::move(lowered_or.error()));
  mir::Expr lowered = *std::move(lowered_or);

  // %s formats by operand type (LRM 21.2.1.7): a String and a packed value
  // each format directly, without building a string value. Only an unpacked
  // byte array is not directly formattable, so it lifts to a string value
  // here.
  const auto& value_type = process.Module().Unit().types.Get(lowered.type);
  if (spec.kind == value::FormatKind::kString &&
      value_type.Kind() != mir::TypeKind::kString &&
      !value_type.IsIntegralPacked()) {
    const mir::ExprId inner = block.exprs.Add(std::move(lowered));
    lowered = BuildValueConversion(
        process.Module().Unit(), block, inner,
        process.Module().Unit().builtins.string);
  }

  const mir::TypeId type = lowered.type;
  const mir::ExprId value = block.exprs.Add(std::move(lowered));
  return mir::RuntimePrintValue(value, type, std::move(spec));
}

auto BuildPrintItemFromDirective(
    ProcessLowerer& process, WalkFrame frame,
    const support::ParsedFormatDirective& directive,
    std::span<const hir::ExprId> args, std::size_t& value_index,
    diag::SourceSpan span) -> diag::Result<mir::RuntimePrintItem> {
  switch (directive.kind) {
    case support::FormatDirectiveKind::kLiteral:
      return mir::RuntimePrintLiteral{.text = directive.literal};

    case support::FormatDirectiveKind::kModulePath: {
      // LRM 21.2.1.1: the hierarchical name of the enclosing scope. Reach it
      // through `self`, the body's first binding -- the same receiver pattern
      // every other scope-method call uses. A deferred caller ($strobe, NBA)
      // captures `self` via the closure builder, so a delayed fire still
      // reads the issuing scope's path. The directive carries no operand, so
      // the print loop's value_index is untouched; the receiver's modifiers
      // ride through the string-format spec like an ordinary `%s` argument.
      auto& block = *frame.current_block;
      const auto& builtins = process.Module().Unit().builtins;
      const mir::ExprId self_id = block.exprs.Add(
          MakeSelfRefExpr(frame, frame.current_class->self_pointer_type));
      const mir::ExprId path_id = block.exprs.Add(
          mir::Expr{
              .data =
                  mir::CallExpr{
                      .callee =
                          mir::Direct{
                              .target = support::BuiltinFn::kHierarchicalPath},
                      .arguments = {self_id}},
              .type = builtins.string});
      return mir::RuntimePrintValue(
          path_id, builtins.string,
          mir::FormatSpec(
              value::FormatKind::kString,
              ToMirFormatModifiers(directive.modifiers)));
    }

    case support::FormatDirectiveKind::kTime:
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
        return diag::Fail(
            span, diag::DiagCode::kErrorDisplayMissingArg,
            "format string consumes more arguments than provided");
      }
      const hir::ExprId hir_arg = args[value_index++];
      return BuildPrintValueItem(
          process, frame, hir_arg,
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
  const auto& expr = proc.exprs.Get(expr_id);
  const auto* primary = std::get_if<hir::PrimaryExpr>(&expr.data);
  if (primary == nullptr) return std::nullopt;
  const auto* sl = std::get_if<hir::StringLiteral>(&primary->data);
  if (sl == nullptr) return std::nullopt;
  return LiteralFormatStringRef{.text = sl->value, .span = expr.span};
}

// LRM 21.2.1.3: a %t directive scales by the enclosing scope's time unit, known
// only at lowering -- so its power is materialized here as the spec's sixth
// field rather than read from the directive like the others. Fields pass as
// `int` literals the runtime FormatSpec constructor converts; the modifier
// fields are omitted when none differs from its default.
auto BuildFormatSpecExpr(
    mir::CompilationUnit& unit, mir::Block& block, const mir::FormatSpec& spec,
    std::int64_t time_unit_power) -> mir::Expr {
  const auto int_lit = [&](std::int64_t v) {
    return block.exprs.Add(mir::MakeInt32Literal(unit.builtins.int32, v));
  };
  std::vector<mir::ExprId> args;
  args.push_back(int_lit(static_cast<std::int64_t>(spec.kind)));
  const bool is_time = spec.kind == value::FormatKind::kTime;
  const bool all_default =
      spec.modifiers.width == -1 && spec.modifiers.precision == -1 &&
      !spec.modifiers.zero_pad && !spec.modifiers.left_align && !is_time;
  if (!all_default) {
    args.push_back(int_lit(spec.modifiers.width));
    args.push_back(int_lit(spec.modifiers.precision));
    args.push_back(int_lit(spec.modifiers.zero_pad ? 1 : 0));
    args.push_back(int_lit(spec.modifiers.left_align ? 1 : 0));
    args.push_back(int_lit(is_time ? time_unit_power : 0));
  }
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Construct{}, .arguments = std::move(args)},
      .type = unit.builtins.format_spec};
}

auto BuildPrintItemExpr(
    mir::CompilationUnit& unit, mir::Block& block,
    const mir::RuntimePrintItem& item, std::int64_t time_unit_power)
    -> mir::Expr {
  return std::visit(
      Overloaded{
          [&](const mir::RuntimePrintLiteral& lit) -> mir::Expr {
            const mir::ExprId text_lit = block.exprs.Add(
                mir::Expr{
                    .data = mir::StringLiteral{.value = lit.text},
                    .type = unit.builtins.string});
            const mir::ExprId text = block.exprs.Add(
                mir::Expr{
                    .data =
                        mir::CallExpr{
                            .callee = mir::Construct{},
                            .arguments = {text_lit}},
                    .type = unit.builtins.string});
            return mir::Expr{
                .data =
                    mir::CallExpr{
                        .callee = mir::Construct{}, .arguments = {text}},
                .type = unit.builtins.print_literal_item};
          },
          [&](const mir::RuntimePrintValue& v) -> mir::Expr {
            const mir::ExprId spec = block.exprs.Add(
                BuildFormatSpecExpr(unit, block, v.spec, time_unit_power));
            return mir::Expr{
                .data =
                    mir::CallExpr{
                        .callee = mir::Construct{},
                        .arguments = {v.value, spec}},
                .type = unit.builtins.print_value_item};
          }},
      item);
}

}  // namespace

auto RadixToFormatKind(support::PrintRadix r) -> value::FormatKind {
  switch (r) {
    case support::PrintRadix::kDecimal:
      return value::FormatKind::kDecimal;
    case support::PrintRadix::kBinary:
      return value::FormatKind::kBinary;
    case support::PrintRadix::kOctal:
      return value::FormatKind::kOctal;
    case support::PrintRadix::kHex:
      return value::FormatKind::kHex;
  }
  throw InternalError("RadixToFormatKind: unknown PrintRadix");
}

auto BuildRuntimePrintItemsFromCallArgs(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call,
    support::PrintRadix default_radix, std::size_t arg_offset,
    FormatStringRequirement fmt_req, diag::SourceSpan call_span)
    -> diag::Result<std::vector<mir::RuntimePrintItem>> {
  const auto& hir_proc = process.HirBody();
  std::vector<mir::RuntimePrintItem> items;
  // The print family does not permit positional elision; flatten the
  // optional-bearing arg list once so the per-directive loop can pass plain
  // ExprIds around. An elided slot here indicates a frontend or HIR-lowering
  // bug -- elision is only legal at the $fread mem-form's start position.
  std::vector<hir::ExprId> args;
  args.reserve(call.arguments.size());
  for (const auto& slot : call.arguments) {
    if (!slot.has_value()) {
      throw InternalError("print-family call argument is unexpectedly elided");
    }
    args.push_back(*slot);
  }
  std::size_t cursor = arg_offset;

  std::optional<LiteralFormatStringRef> literal;
  if (cursor < args.size()) {
    literal = TryGetHirStringLiteral(hir_proc, args[cursor]);
  }
  if (!literal.has_value() && fmt_req == FormatStringRequirement::kRequired) {
    // LRM 21.3.3 NOTE permits a non-literal format string; not yet supported.
    const diag::SourceSpan span = cursor < args.size()
                                      ? hir_proc.exprs.Get(args[cursor]).span
                                      : call_span;
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedSubroutineArgument,
        "format string must be a string literal; a runtime-evaluated format "
        "string is not yet supported (LRM 21.3.3)");
  }
  if (literal.has_value()) {
    auto parsed_or =
        support::ParseLiteralFormatString(literal->text, literal->span);
    if (!parsed_or) return std::unexpected(std::move(parsed_or.error()));
    ++cursor;
    auto value_index = cursor;
    for (const auto& directive : *parsed_or) {
      auto item_or = BuildPrintItemFromDirective(
          process, frame, directive, args, value_index, literal->span);
      if (!item_or) return std::unexpected(std::move(item_or.error()));
      items.push_back(std::move(*item_or));
    }
    cursor = value_index;
  }

  const value::FormatKind default_kind = RadixToFormatKind(default_radix);
  while (cursor < args.size()) {
    if (!items.empty()) {
      items.emplace_back(mir::RuntimePrintLiteral{.text = " "});
    }
    auto item_or = BuildPrintValueItem(
        process, frame, args[cursor],
        mir::FormatSpec(default_kind, mir::FormatModifiers{}));
    if (!item_or) return std::unexpected(std::move(item_or.error()));
    items.push_back(*std::move(item_or));
    ++cursor;
  }
  return items;
}

auto BuildPrintItemsArray(
    mir::CompilationUnit& unit, mir::Block& block,
    const std::vector<mir::RuntimePrintItem>& items,
    std::int64_t time_unit_power) -> mir::Expr {
  std::vector<mir::ExprId> elements;
  elements.reserve(items.size());
  for (const mir::RuntimePrintItem& item : items) {
    elements.push_back(block.exprs.Add(
        BuildPrintItemExpr(unit, block, item, time_unit_power)));
  }
  const mir::TypeId array_type = unit.types.Intern(
      mir::UnpackedArrayType{
          .element_type = unit.builtins.print_item, .size = items.size()});
  return mir::Expr{
      .data = mir::ArrayLiteralExpr{.elements = std::move(elements)},
      .type = array_type};
}

}  // namespace lyra::lowering::hir_to_mir
