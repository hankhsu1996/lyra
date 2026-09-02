#include "lyra/lowering/hir_to_mir/print_items.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
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
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/runtime_print.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/value/format_parse.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// LRM 6.14 permits a chandle only in the equality family and a boolean test, so
// a chandle -- alone or nested inside an aggregate whose `%p` would print it --
// is never a legal format operand. slang does not filter the form, so lowering
// does, exactly as it does for a `real` case-equality (LRM Table 11-1). The
// runtime consequently carries no chandle formatter, and no simulation prints a
// host pointer.
auto TypeContainsChandle(const mir::CompilationUnit& unit, mir::TypeId type)
    -> bool {
  return unit.types.Get(type).Visit(
      Overloaded{
          [](const mir::ChandleType&) { return true; },
          [&](const mir::UnpackedArrayType& t) {
            return TypeContainsChandle(unit, t.element_type);
          },
          [&](const mir::DynamicArrayType& t) {
            return TypeContainsChandle(unit, t.element_type);
          },
          [&](const mir::QueueType& t) {
            return TypeContainsChandle(unit, t.element_type);
          },
          [&](const mir::AssociativeArrayType& t) {
            return TypeContainsChandle(unit, t.key_type) ||
                   TypeContainsChandle(unit, t.element_type);
          },
          [&](const mir::TupleType& t) {
            return std::ranges::any_of(t.elements, [&](mir::TypeId e) {
              return TypeContainsChandle(unit, e);
            });
          },
          [&](const mir::UnionType& t) {
            return std::ranges::any_of(t.elements, [&](mir::TypeId e) {
              return TypeContainsChandle(unit, e);
            });
          },
          [&](const mir::TaggedUnionType& t) {
            return std::ranges::any_of(t.elements, [&](mir::TypeId e) {
              return TypeContainsChandle(unit, e);
            });
          },
          [](const auto&) { return false; }});
}

auto ToMirFormatModifiers(const value::FormatModifiers& m)
    -> mir::FormatModifiers {
  return mir::FormatModifiers{
      .width = m.width,
      .precision = m.precision,
      .zero_pad = m.zero_pad,
      .left_align = m.left_align};
}

// The print family does not permit positional elision, so the optional-bearing
// argument list flattens to plain ids once and every walk over it works from
// those. An elided slot indicates a frontend or HIR-lowering bug -- elision is
// only legal at the $fread mem-form's start position.
auto FlattenCallArgs(const hir::CallExpr& call) -> std::vector<hir::ExprId> {
  std::vector<hir::ExprId> args;
  args.reserve(call.arguments.size());
  for (const auto& slot : call.arguments) {
    if (!slot.has_value()) {
      throw InternalError("print-family call argument is unexpectedly elided");
    }
    args.push_back(*slot);
  }
  return args;
}

// Lowers one operand of a format, whichever way its conversion is chosen. The
// returned expression is detached for the caller to intern.
template <ExprLowerer Lowerer>
auto LowerFormatOperand(Lowerer& lowerer, WalkFrame frame, hir::ExprId hir_arg)
    -> diag::Result<mir::Expr> {
  const hir::Expr& hir_expr = lowerer.HirExprs().Get(hir_arg);
  auto lowered_or = lowerer.LowerExpr(hir_expr, frame);
  if (!lowered_or) return std::unexpected(std::move(lowered_or.error()));
  mir::Expr lowered = *std::move(lowered_or);
  if (TypeContainsChandle(lowerer.Owner().Unit(), lowered.type)) {
    return diag::Fail(
        hir_expr.span, diag::DiagCode::kUnsupportedExpressionForm,
        "a chandle is not a legal format argument (LRM 6.14 permits a chandle "
        "only in an equality comparison and a boolean test)");
  }
  return lowered;
}

template <ExprLowerer Lowerer>
auto BuildPrintValueItem(
    Lowerer& lowerer, WalkFrame frame, hir::ExprId hir_arg,
    mir::FormatSpec spec) -> diag::Result<mir::RuntimePrintItem> {
  auto& block = *frame.current_block;
  auto lowered_or = LowerFormatOperand(lowerer, frame, hir_arg);
  if (!lowered_or) return std::unexpected(std::move(lowered_or.error()));
  mir::Expr lowered = *std::move(lowered_or);

  // %s formats by operand type (LRM 21.2.1.7): a String and a packed value
  // each format directly, without building a string value. Only an unpacked
  // byte array is not directly formattable, so it lifts to a string value
  // here.
  const mir::Type& value_type = lowerer.Owner().Unit().types.Get(lowered.type);
  if (spec.kind == value::FormatKind::kString &&
      !value_type.Is<mir::StringType>() && !value_type.IsIntegralPacked()) {
    const mir::ExprId inner = block.exprs.Add(std::move(lowered));
    lowered = BuildValueConversion(
        lowerer.Owner().Unit(), block, inner,
        lowerer.Owner().Unit().builtins.string);
  }

  const mir::TypeId type = lowered.type;
  const mir::ExprId value = block.exprs.Add(std::move(lowered));
  return mir::RuntimePrintValue(value, type, std::move(spec));
}

// The string LRM 21.2.1.5 `%m` names: the hierarchical name of the scope the
// directive was written in. That scope is the innermost named block around it
// when there is one (LRM 9.3.5), a task or function when the directive sits in
// one (LRM 23.9), and the body's own scope otherwise. Each of those is a
// runtime object that knows its own path, so the whole answer is asking the
// object the frame's scope names -- or the enclosing object itself where the
// scope owns none. A deferred caller ($strobe, an NBA) captures `self` along
// with the rest of its operands, so a delayed fire still reads the issuing
// scope's name.
template <ExprLowerer Lowerer>
auto BuildHierarchicalNameExpr(Lowerer& lowerer, const WalkFrame& frame)
    -> mir::ExprId {
  auto& unit = lowerer.Owner().Unit();
  auto& block = *frame.current_block;
  const mir::ExprId receiver_id =
      frame.scope_name_borrowed_handle.has_value()
          ? block.exprs.Add(BuildStructuralFieldAccessExpr(
                frame, unit, mir::EnclosingHops{},
                *frame.scope_name_borrowed_handle))
          : block.exprs.Add(
                MakeSelfRefExpr(frame, frame.current_class->self_pointer_type));
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kHierarchicalPath},
                  .arguments = {receiver_id}},
          .type = unit.builtins.string});
}

template <ExprLowerer Lowerer>
auto LowerPrintItemForDirective(
    Lowerer& lowerer, WalkFrame frame, const value::FormatDirective& directive,
    std::span<const hir::ExprId> args, std::size_t& value_index,
    diag::SourceSpan span) -> diag::Result<mir::RuntimePrintItem> {
  switch (directive.role) {
    case value::FormatDirective::Role::kLiteral:
      return mir::RuntimePrintLiteral{.text = directive.literal};

    case value::FormatDirective::Role::kModulePath:
      // The directive takes no operand, so it leaves the operand cursor where
      // it found it; its modifiers ride through the string-format spec like an
      // ordinary `%s` argument.
      return mir::RuntimePrintValue(
          BuildHierarchicalNameExpr(lowerer, frame),
          lowerer.Owner().Unit().builtins.string,
          mir::FormatSpec(
              value::FormatKind::kString,
              ToMirFormatModifiers(directive.modifiers)));

    case value::FormatDirective::Role::kValue: {
      if (value_index >= args.size()) {
        return diag::Fail(
            span, diag::DiagCode::kErrorDisplayMissingArg,
            "format string consumes more arguments than provided");
      }
      const hir::ExprId hir_arg = args[value_index++];
      return BuildPrintValueItem(
          lowerer, frame, hir_arg,
          mir::FormatSpec(
              directive.kind, ToMirFormatModifiers(directive.modifiers)));
    }
  }
  throw InternalError("LowerPrintItemForDirective: unreachable directive role");
}

// The format grammar is span-free so the runtime can share it (LRM 21.3.3
// allows a format string known only at simulation time). A compile-time parse
// rejects the same malformed directives, so it pins the failure to the format
// string's source span here.
auto FailFormatParse(
    const value::FormatParseResult& parsed, diag::SourceSpan span)
    -> std::unexpected<diag::Diagnostic> {
  switch (parsed.error) {
    case value::FormatParseError::kMissingPrecision:
      return diag::Fail(
          span, diag::DiagCode::kErrorFormatStringMissingPrecision,
          "format directive '.' is missing precision digits");
    case value::FormatParseError::kTrailingPercent:
      return diag::Fail(
          span, diag::DiagCode::kErrorFormatStringTrailingPercent,
          "format string ends with unfinished '%' directive");
    case value::FormatParseError::kUnknownSpecifier:
      return diag::Fail(
          span, diag::DiagCode::kErrorFormatStringUnknownSpecifier,
          std::format("unknown format specifier '%{}'", parsed.spec_char));
    case value::FormatParseError::kWidthOverflow:
      return diag::Fail(
          span, diag::DiagCode::kErrorFormatStringWidthOverflow,
          "format directive width does not fit in int32");
    case value::FormatParseError::kPrecisionOverflow:
      return diag::Fail(
          span, diag::DiagCode::kErrorFormatStringWidthOverflow,
          "format directive precision does not fit in int32");
    case value::FormatParseError::kModifierNotPermitted:
      return diag::Fail(
          span, diag::DiagCode::kErrorFormatStringModifierNotPermitted,
          std::format(
              "'%{}' takes a non-negative field width and nothing else; a "
              "left-justifying '-' and a precision are defined only for the "
              "real conversions %e / %f / %g (LRM 21.2.1.2)",
              parsed.spec_char));
    case value::FormatParseError::kNone:
      break;
  }
  throw InternalError("FailFormatParse: format string parsed without error");
}

struct LiteralFormatStringRef {
  std::string_view text;
  diag::SourceSpan span;
};

auto TryGetHirStringLiteral(
    const base::Arena<hir::Expr, hir::ExprId>& exprs, hir::ExprId expr_id)
    -> std::optional<LiteralFormatStringRef> {
  const auto& expr = exprs.Get(expr_id);
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
    return BuildIntLiteral(unit, block, v);
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

template <ExprLowerer Lowerer>
auto BuildRuntimePrintItemsFromCallArgs(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& call,
    support::PrintRadix default_radix, std::size_t arg_offset)
    -> diag::Result<std::vector<mir::RuntimePrintItem>> {
  const auto& hir_exprs = lowerer.HirExprs();
  std::vector<mir::RuntimePrintItem> items;
  const std::vector<hir::ExprId> args = FlattenCallArgs(call);
  std::size_t cursor = arg_offset;

  std::optional<LiteralFormatStringRef> literal;
  if (cursor < args.size()) {
    literal = TryGetHirStringLiteral(hir_exprs, args[cursor]);
  }
  if (literal.has_value()) {
    const value::FormatParseResult parsed =
        value::ParseFormatString(literal->text);
    if (parsed.error != value::FormatParseError::kNone) {
      return FailFormatParse(parsed, literal->span);
    }
    ++cursor;
    auto value_index = cursor;
    for (const auto& directive : parsed.directives) {
      auto item_or = LowerPrintItemForDirective(
          lowerer, frame, directive, args, value_index, literal->span);
      if (!item_or) return std::unexpected(std::move(item_or.error()));
      items.push_back(*std::move(item_or));
    }
    cursor = value_index;
  }

  const value::FormatKind default_kind = RadixToFormatKind(default_radix);
  while (cursor < args.size()) {
    if (!items.empty()) {
      items.emplace_back(mir::RuntimePrintLiteral{.text = " "});
    }
    auto item_or = BuildPrintValueItem(
        lowerer, frame, args[cursor],
        mir::FormatSpec(default_kind, mir::FormatModifiers{}));
    if (!item_or) return std::unexpected(std::move(item_or.error()));
    items.push_back(*std::move(item_or));
    ++cursor;
  }
  return items;
}

template <ExprLowerer Lowerer>
auto HasLiteralFormatString(
    const Lowerer& lowerer, const hir::CallExpr& call, std::size_t arg_offset)
    -> bool {
  if (arg_offset >= call.arguments.size()) return false;
  const auto& slot = call.arguments[arg_offset];
  if (!slot.has_value()) return false;
  return TryGetHirStringLiteral(lowerer.HirExprs(), *slot).has_value();
}

template <ExprLowerer Lowerer>
auto BuildRuntimeFormatCallExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& call,
    std::size_t arg_offset) -> diag::Result<mir::Expr> {
  auto& unit = lowerer.Owner().Unit();
  auto& block = *frame.current_block;
  const std::vector<hir::ExprId> args = FlattenCallArgs(call);

  if (arg_offset >= args.size()) {
    throw InternalError(
        "BuildRuntimeFormatCallExpr: the format-string slot is absent; the "
        "subroutine's argument-count policy should have rejected the call");
  }

  // An integral or unpacked-byte-array format string carries its text as bytes
  // (LRM 21.3.3), so it reaches the parse as a string value through the same
  // conversion any other bits-to-text operand takes.
  auto format_or = LowerFormatOperand(lowerer, frame, args[arg_offset]);
  if (!format_or) return std::unexpected(std::move(format_or.error()));
  const mir::ExprId lowered_format = block.exprs.Add(*std::move(format_or));
  const mir::ExprId format_id =
      ConvertToType(unit, block, lowered_format, unit.builtins.string);

  std::vector<mir::ExprId> operands;
  operands.reserve(args.size() - arg_offset - 1);
  for (std::size_t i = arg_offset + 1; i < args.size(); ++i) {
    auto lowered_or = LowerFormatOperand(lowerer, frame, args[i]);
    if (!lowered_or) return std::unexpected(std::move(lowered_or.error()));
    const mir::ExprId value = block.exprs.Add(*std::move(lowered_or));
    operands.push_back(block.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{.callee = mir::Construct{}, .arguments = {value}},
            .type = unit.builtins.format_arg}));
  }
  const mir::TypeId operands_type = mir::MachineArrayOf(
      unit.types, unit.builtins.format_arg, operands.size());
  const mir::ExprId operands_array = block.exprs.Add(
      mir::Expr{
          .data = mir::ArrayLiteralExpr{.elements = std::move(operands)},
          .type = operands_type});

  // The hierarchical name a `%m` renders and the scope's time unit a `%t`
  // scales against are facts of the call site, not of the format text, so they
  // reach the parse as operands.
  const mir::ExprId path_id = BuildHierarchicalNameExpr(lowerer, frame);

  const mir::ExprId runtime_id =
      block.exprs.Add(BuildCurrentRuntimeCallExpr(lowerer.Owner()));
  const mir::ExprId time_format_id = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{.target = support::BuiltinFn::kTimeFormat},
                  .arguments = {runtime_id}},
          .type = unit.builtins.time_format});
  const mir::ExprId time_unit_power = BuildIntLiteral(
      unit, block, static_cast<std::int64_t>(lowerer.Resolution().unit_power));

  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{.target = support::BuiltinFn::kFormatRuntime},
              .arguments =
                  {format_id, operands_array, path_id, time_format_id,
                   time_unit_power}},
      .type = unit.builtins.string};
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
  const mir::TypeId array_type =
      mir::MachineArrayOf(unit.types, unit.builtins.print_item, items.size());
  return mir::Expr{
      .data = mir::ArrayLiteralExpr{.elements = std::move(elements)},
      .type = array_type};
}

template auto HasLiteralFormatString(
    const ProcessLowerer&, const hir::CallExpr&, std::size_t) -> bool;
template auto HasLiteralFormatString(
    const StructuralScopeLowerer&, const hir::CallExpr&, std::size_t) -> bool;
template auto BuildRuntimePrintItemsFromCallArgs(
    ProcessLowerer&, WalkFrame, const hir::CallExpr&, support::PrintRadix,
    std::size_t) -> diag::Result<std::vector<mir::RuntimePrintItem>>;
template auto BuildRuntimePrintItemsFromCallArgs(
    const StructuralScopeLowerer&, WalkFrame, const hir::CallExpr&,
    support::PrintRadix, std::size_t)
    -> diag::Result<std::vector<mir::RuntimePrintItem>>;
template auto BuildRuntimeFormatCallExpr(
    ProcessLowerer&, WalkFrame, const hir::CallExpr&, std::size_t)
    -> diag::Result<mir::Expr>;
template auto BuildRuntimeFormatCallExpr(
    const StructuralScopeLowerer&, WalkFrame, const hir::CallExpr&, std::size_t)
    -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
