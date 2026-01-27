#include "lyra/lowering/ast_to_hir/system_call.hpp"

#include <cctype>
#include <cstddef>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <vector>

#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/ConversionExpression.h>
#include <slang/ast/expressions/LiteralExpressions.h>
#include <slang/ast/expressions/OperatorExpressions.h>

#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/common/format.hpp"
#include "lyra/common/system_function.hpp"
#include "lyra/common/timescale_format.hpp"
#include "lyra/common/type.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/expression.hpp"
#include "lyra/hir/system_call.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"
#include "lyra/lowering/ast_to_hir/detail/expression_lowering.hpp"
#include "lyra/lowering/ast_to_hir/module_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// Get module timeunit power from view, with default fallback.
auto GetModuleTimeunitPower(const ExpressionLoweringView& view) -> int8_t {
  if (view.frame != nullptr) {
    return static_cast<int8_t>(view.frame->unit_power);
  }
  return static_cast<int8_t>(kDefaultTimeScalePower);
}

// Convert PrintRadix to default FormatKind
auto RadixToFormatKind(PrintRadix radix) -> FormatKind {
  switch (radix) {
    case PrintRadix::kDecimal:
      return FormatKind::kDecimal;
    case PrintRadix::kBinary:
      return FormatKind::kBinary;
    case PrintRadix::kOctal:
      return FormatKind::kOctal;
    case PrintRadix::kHex:
      return FormatKind::kHex;
  }
  return FormatKind::kDecimal;
}

// Check if a type is eligible for $monitor (can be snapshotted).
// Supported: integral, packed array/struct, enum, real, shortreal.
// Unsupported: string, unpacked array/struct/union, dynamic array, queue.
auto IsMonitorEligibleType(const Type& ty) -> bool {
  return IsPacked(ty) || ty.Kind() == TypeKind::kReal ||
         ty.Kind() == TypeKind::kShortReal;
}

// Create a FormatOp for auto-formatting an argument.
// String types use kString; other types use the provided default.
auto MakeAutoFormatOp(
    hir::ExpressionId arg_id, FormatKind default_format, Context* ctx)
    -> hir::FormatOp {
  const hir::Expression& expr = (*ctx->hir_arena)[arg_id];
  const Type& ty = (*ctx->type_arena)[expr.type];
  FormatKind kind = default_format;
  if (ty.Kind() == TypeKind::kString) {
    kind = FormatKind::kString;
  }
  return hir::FormatOp{
      .kind = kind, .value = arg_id, .literal = {}, .mods = {}};
}

// Convert format specifier character to FormatKind
auto SpecToFormatKind(char spec) -> std::optional<FormatKind> {
  switch (spec) {
    case 'd':
      return FormatKind::kDecimal;
    case 'h':
    case 'x':
      return FormatKind::kHex;
    case 'b':
      return FormatKind::kBinary;
    case 'o':
      return FormatKind::kOctal;
    case 's':
      return FormatKind::kString;
    case 'f':
      return FormatKind::kReal;
    case 't':
      return FormatKind::kTime;
    default:
      return std::nullopt;
  }
}

// Parse format string and create FormatOps.
// Returns the FormatOps and the number of arguments consumed.
struct ParseResult {
  std::vector<hir::FormatOp> ops;
  size_t args_consumed = 0;
};

auto ParseFormatString(
    std::string_view fmt, std::span<const hir::ExpressionId> args,
    FormatKind /*default_format*/, DiagnosticSink& diag, SourceSpan span,
    int8_t module_timeunit_power) -> ParseResult {
  ParseResult result;
  size_t arg_idx = 0;
  size_t pos = 0;
  std::string current_literal;

  auto flush_literal = [&]() {
    if (!current_literal.empty()) {
      result.ops.push_back(
          hir::FormatOp{
              .kind = FormatKind::kLiteral,
              .value = std::nullopt,
              .literal = std::move(current_literal),
              .mods = {}});
      current_literal.clear();
    }
  };

  while (pos < fmt.size()) {
    if (fmt[pos] != '%') {
      current_literal += fmt[pos];
      ++pos;
      continue;
    }

    // Found '%'
    if (pos + 1 >= fmt.size()) {
      // Trailing '%' with nothing after
      current_literal += '%';
      ++pos;
      continue;
    }

    // Check for %%
    if (fmt[pos + 1] == '%') {
      current_literal += '%';
      pos += 2;
      continue;
    }

    // Parse the format specifier
    // SV format: %[-][0][width][.precision]specifier
    size_t spec_pos = pos + 1;

    // Parse flags
    bool left_align = false;
    bool zero_pad = false;
    if (spec_pos < fmt.size() && fmt[spec_pos] == '-') {
      left_align = true;
      ++spec_pos;
    }

    // Parse width (including leading 0 which means minimal/zero-pad)
    // Per LRM 21.2.1.2: %0h means width=0 (minimal format)
    // %08h means width=8 with zero padding
    std::optional<int> width;
    if (spec_pos < fmt.size() && std::isdigit(fmt[spec_pos]) != 0) {
      // Check for zero-padding: 0 followed by more digits
      if (fmt[spec_pos] == '0' && spec_pos + 1 < fmt.size() &&
          std::isdigit(fmt[spec_pos + 1]) != 0) {
        zero_pad = true;
        ++spec_pos;
      }
      int w = 0;
      while (spec_pos < fmt.size() && std::isdigit(fmt[spec_pos]) != 0) {
        w = w * 10 + (fmt[spec_pos] - '0');
        ++spec_pos;
      }
      width = w;
    }

    // Parse precision
    std::optional<int> precision;
    if (spec_pos < fmt.size() && fmt[spec_pos] == '.') {
      ++spec_pos;
      int p = 0;
      while (spec_pos < fmt.size() && std::isdigit(fmt[spec_pos]) != 0) {
        p = p * 10 + (fmt[spec_pos] - '0');
        ++spec_pos;
      }
      precision = p;
    }

    if (spec_pos >= fmt.size()) {
      // Incomplete format spec - treat as literal
      current_literal += fmt.substr(pos);
      break;
    }

    char spec_char = fmt[spec_pos];
    auto format_kind = SpecToFormatKind(spec_char);
    pos = spec_pos + 1;

    if (!format_kind.has_value()) {
      diag.Error(
          span, std::format("unknown format specifier '%{}'", spec_char));
      // Treat as literal to continue parsing
      current_literal +=
          fmt.substr(pos - (spec_pos - pos + 1), spec_pos - pos + 2);
      continue;
    }

    // Flush any pending literal
    flush_literal();

    // Create format op for this specifier
    if (arg_idx < args.size()) {
      result.ops.push_back(
          hir::FormatOp{
              .kind = *format_kind,
              .value = args[arg_idx],
              .literal = {},
              .mods =
                  {.width = width,
                   .precision = precision,
                   .zero_pad = zero_pad,
                   .left_align = left_align},
              .module_timeunit_power = module_timeunit_power});
      ++arg_idx;
    } else {
      diag.Error(
          span, std::format(
                    "format specifier '%{}' has no corresponding argument",
                    spec_char));
    }
  }

  // Flush any remaining literal
  flush_literal();

  result.args_consumed = arg_idx;
  return result;
}

// Lower call arguments to HIR expressions.
// Returns nullopt on failure; diagnostics are emitted by LowerExpression.
auto LowerArguments(
    const slang::ast::CallExpression& call, ExpressionLoweringView view)
    -> std::optional<std::vector<hir::ExpressionId>> {
  std::vector<hir::ExpressionId> args;
  for (const slang::ast::Expression* arg : call.arguments()) {
    hir::ExpressionId arg_id = LowerExpression(*arg, view);
    if (!arg_id) {
      return std::nullopt;
    }
    args.push_back(arg_id);
  }
  return args;
}

struct LowerVisitor {
  const slang::ast::CallExpression* call = nullptr;
  ExpressionLoweringView view{};
  TypeId result_type;

  [[nodiscard]] auto Ctx() const -> Context* {
    return view.context;
  }

  auto operator()(const DisplayFunctionInfo& info) const -> hir::ExpressionId {
    SourceSpan span = Ctx()->SpanOf(call->sourceRange);

    auto args = LowerArguments(*call, view);
    if (!args) {
      return hir::kInvalidExpressionId;
    }

    PrintKind print_kind =
        info.append_newline ? PrintKind::kDisplay : PrintKind::kWrite;
    FormatKind default_format = RadixToFormatKind(info.radix);

    std::vector<hir::FormatOp> ops;

    if (args->empty()) {
      // No arguments - just print newline if display
      // ops is empty, which is fine
    } else {
      // Check if first argument is a string literal with '%'
      const auto& slang_args = call->arguments();
      const slang::ast::Expression* first_arg = slang_args[0];

      // Unwrap conversion if present (slang wraps string literals)
      if (first_arg->kind == slang::ast::ExpressionKind::Conversion) {
        const auto& conv = first_arg->as<slang::ast::ConversionExpression>();
        first_arg = &conv.operand();
      }

      bool has_format_string = false;
      std::string format_str;

      if (first_arg->kind == slang::ast::ExpressionKind::StringLiteral) {
        const auto& literal = first_arg->as<slang::ast::StringLiteral>();
        format_str = std::string(literal.getValue());
        has_format_string = true;
      }

      if (has_format_string && format_str.find('%') != std::string::npos) {
        // Parse format string with remaining arguments
        std::span<const hir::ExpressionId> all_args(*args);
        auto remaining_args = all_args.subspan(1);
        auto parse_result = ParseFormatString(
            format_str, remaining_args, default_format, *Ctx()->sink, span,
            GetModuleTimeunitPower(view));
        ops = std::move(parse_result.ops);

        // Auto-format any remaining arguments not consumed by format string
        size_t next_arg = 1 + parse_result.args_consumed;
        while (next_arg < args->size()) {
          ops.push_back(
              MakeAutoFormatOp((*args)[next_arg], default_format, Ctx()));
          ++next_arg;
        }
      } else if (has_format_string) {
        // String without '%' - output as literal prefix + auto-format rest
        ops.push_back(
            hir::FormatOp{
                .kind = FormatKind::kLiteral,
                .value = std::nullopt,
                .literal = format_str,
                .mods = {}});
        for (size_t i = 1; i < args->size(); ++i) {
          ops.push_back(MakeAutoFormatOp((*args)[i], default_format, Ctx()));
        }
      } else {
        // First arg is not a string - auto-format all arguments
        for (hir::ExpressionId arg_id : *args) {
          ops.push_back(MakeAutoFormatOp(arg_id, default_format, Ctx()));
        }
      }
    }

    return Ctx()->hir_arena->AddExpression(
        hir::Expression{
            .kind = hir::ExpressionKind::kSystemCall,
            .type = result_type,
            .span = span,
            .data = hir::DisplaySystemCallData{
                .print_kind = print_kind,
                .ops = std::move(ops),
                .descriptor = std::nullopt,
                .is_strobe = info.is_strobe}});
  }

  auto operator()(const TerminationFunctionInfo& /*info*/) const
      -> hir::ExpressionId {
    // Termination calls should be handled in statement.cpp, not here.
    // If we reach here, something is wrong.
    SourceSpan span = Ctx()->SpanOf(call->sourceRange);
    Ctx()->sink->Error(
        span,
        "termination calls ($finish/$stop/$exit) should not be used as "
        "expressions");
    return hir::kInvalidExpressionId;
  }

  auto operator()(const SeverityFunctionInfo& info) const -> hir::ExpressionId {
    SourceSpan span = Ctx()->SpanOf(call->sourceRange);

    auto args = LowerArguments(*call, view);
    if (!args) {
      return hir::kInvalidExpressionId;
    }

    // Build format ops with decimal as default radix (per LRM)
    std::vector<const slang::ast::Expression*> slang_ptrs;
    slang_ptrs.reserve(call->arguments().size());
    for (const slang::ast::Expression* arg : call->arguments()) {
      slang_ptrs.push_back(arg);
    }

    std::vector<hir::FormatOp> ops = BuildDisplayFormatOps(
        slang_ptrs, *args, FormatKind::kDecimal, Ctx(),
        GetModuleTimeunitPower(view));

    return Ctx()->hir_arena->AddExpression(
        hir::Expression{
            .kind = hir::ExpressionKind::kSystemCall,
            .type = result_type,
            .span = span,
            .data = hir::SeveritySystemCallData{
                .level = info.level, .ops = std::move(ops)}});
  }

  auto operator()(const FatalFunctionInfo& /*info*/) const
      -> hir::ExpressionId {
    // $fatal should be handled in statement.cpp, not here.
    // If we reach here, something is wrong.
    SourceSpan span = Ctx()->SpanOf(call->sourceRange);
    Ctx()->sink->Error(span, "$fatal should not be used as an expression");
    return hir::kInvalidExpressionId;
  }

  auto operator()(const SFormatFunctionInfo& info) const -> hir::ExpressionId {
    SourceSpan span = Ctx()->SpanOf(call->sourceRange);

    FormatKind default_format = RadixToFormatKind(info.radix);
    hir::SFormatSystemCallData data;
    data.default_format = default_format;

    // Cursor into slang arguments
    size_t arg_cursor = 0;

    // Extract output target if present.
    // Slang wraps the output arg as Assignment(left=target,
    // right=EmptyArgument). We extract the left side and lower it as the output
    // expression.
    if (info.has_output_target) {
      const slang::ast::Expression* out_arg = call->arguments()[0];
      if (out_arg->kind == slang::ast::ExpressionKind::Assignment) {
        const auto& assign = out_arg->as<slang::ast::AssignmentExpression>();
        data.output = LowerExpression(assign.left(), view);
      } else {
        data.output = LowerExpression(*out_arg, view);
      }
      if (!data.output || !*data.output) {
        return hir::kInvalidExpressionId;
      }
      ++arg_cursor;
    }

    // Lower remaining arguments (skip the output arg)
    std::vector<hir::ExpressionId> lowered_args;
    for (size_t i = arg_cursor; i < call->arguments().size(); ++i) {
      hir::ExpressionId arg_id = LowerExpression(*call->arguments()[i], view);
      if (!arg_id) {
        return hir::kInvalidExpressionId;
      }
      lowered_args.push_back(arg_id);
    }

    std::span<const hir::ExpressionId> remaining(lowered_args);

    if (info.has_format_string && !remaining.empty()) {
      // $sformatf/$sformat: next arg is format string
      const slang::ast::Expression* fmt_arg = call->arguments()[arg_cursor];

      // Unwrap conversion
      if (fmt_arg->kind == slang::ast::ExpressionKind::Conversion) {
        const auto& conv = fmt_arg->as<slang::ast::ConversionExpression>();
        fmt_arg = &conv.operand();
      }

      bool is_literal = false;
      std::string format_str;
      if (fmt_arg->kind == slang::ast::ExpressionKind::StringLiteral) {
        const auto& literal = fmt_arg->as<slang::ast::StringLiteral>();
        format_str = std::string(literal.getValue());
        is_literal = true;
      }

      if (is_literal) {
        // Compile-time path: parse format string with value args
        auto value_args = remaining.subspan(1);
        auto parse_result = ParseFormatString(
            format_str, value_args, default_format, *Ctx()->sink, span,
            GetModuleTimeunitPower(view));
        data.ops = std::move(parse_result.ops);

        // Auto-format any remaining arguments not consumed
        size_t next_arg = 1 + parse_result.args_consumed;
        while (next_arg < remaining.size()) {
          data.ops.push_back(
              MakeAutoFormatOp(remaining[next_arg], default_format, Ctx()));
          ++next_arg;
        }
      } else {
        // Runtime path: put format arg + remaining value args in data.args
        for (hir::ExpressionId expr_id : remaining) {
          data.args.push_back(expr_id);
        }
      }
    } else if (!info.has_format_string) {
      // $swrite* family: reuse same $display logic on remaining args
      if (remaining.empty()) {
        // No args - empty string result
      } else {
        // Check if first argument is a string literal
        const slang::ast::Expression* first_arg = call->arguments()[arg_cursor];
        if (first_arg->kind == slang::ast::ExpressionKind::Conversion) {
          const auto& conv = first_arg->as<slang::ast::ConversionExpression>();
          first_arg = &conv.operand();
        }

        bool has_format_string = false;
        std::string format_str;
        if (first_arg->kind == slang::ast::ExpressionKind::StringLiteral) {
          const auto& literal = first_arg->as<slang::ast::StringLiteral>();
          format_str = std::string(literal.getValue());
          has_format_string = true;
        }

        if (has_format_string && format_str.find('%') != std::string::npos) {
          // String with '%': parse as format string
          auto value_args = remaining.subspan(1);
          auto parse_result = ParseFormatString(
              format_str, value_args, default_format, *Ctx()->sink, span,
              GetModuleTimeunitPower(view));
          data.ops = std::move(parse_result.ops);

          size_t next_arg = 1 + parse_result.args_consumed;
          while (next_arg < remaining.size()) {
            data.ops.push_back(
                MakeAutoFormatOp(remaining[next_arg], default_format, Ctx()));
            ++next_arg;
          }
        } else if (has_format_string) {
          // String without '%': literal prefix + auto-format rest
          data.ops.push_back(
              hir::FormatOp{
                  .kind = FormatKind::kLiteral,
                  .value = std::nullopt,
                  .literal = format_str,
                  .mods = {}});
          for (size_t i = 1; i < remaining.size(); ++i) {
            data.ops.push_back(
                MakeAutoFormatOp(remaining[i], default_format, Ctx()));
          }
        } else {
          // First arg not a string literal: auto-format all
          for (hir::ExpressionId expr_id : remaining) {
            data.ops.push_back(
                MakeAutoFormatOp(expr_id, default_format, Ctx()));
          }
        }
      }
    }

    return Ctx()->hir_arena->AddExpression(
        hir::Expression{
            .kind = hir::ExpressionKind::kSystemCall,
            .type = result_type,
            .span = span,
            .data = std::move(data)});
  }

  auto operator()(const TimeFormatFunctionInfo& /*info*/) const
      -> hir::ExpressionId {
    SourceSpan span = Ctx()->SpanOf(call->sourceRange);
    hir::TimeFormatData data;

    const auto& args = call->arguments();

    // Helper to unwrap conversion and get underlying literal
    auto unwrap =
        [](const slang::ast::Expression* arg) -> const slang::ast::Expression* {
      if (arg->kind == slang::ast::ExpressionKind::Conversion) {
        const auto& conv = arg->as<slang::ast::ConversionExpression>();
        return &conv.operand();
      }
      return arg;
    };

    // Helper to extract constant integer from slang argument
    auto extract_int = [&](size_t idx) -> std::optional<int64_t> {
      if (idx >= args.size()) return std::nullopt;
      const auto* arg = unwrap(args[idx]);
      if (arg->kind == slang::ast::ExpressionKind::IntegerLiteral) {
        const auto& literal = arg->as<slang::ast::IntegerLiteral>();
        return literal.getValue().as<int64_t>();
      }
      if (arg->kind == slang::ast::ExpressionKind::UnaryOp) {
        // Handle negative literals like -9
        const auto& unary = arg->as<slang::ast::UnaryExpression>();
        if (unary.op == slang::ast::UnaryOperator::Minus) {
          const auto* operand = unwrap(&unary.operand());
          if (operand->kind == slang::ast::ExpressionKind::IntegerLiteral) {
            const auto& literal = operand->as<slang::ast::IntegerLiteral>();
            return -literal.getValue().as<int64_t>().value();
          }
        }
      }
      return std::nullopt;
    };

    // Helper to extract constant string
    auto extract_string = [&](size_t idx) -> std::optional<std::string> {
      if (idx >= args.size()) return std::nullopt;
      const auto* arg = unwrap(args[idx]);
      if (arg->kind == slang::ast::ExpressionKind::StringLiteral) {
        const auto& literal = arg->as<slang::ast::StringLiteral>();
        return std::string(literal.getValue());
      }
      return std::nullopt;
    };

    // units (arg 0): -15 to 2
    if (auto units = extract_int(0)) {
      if (*units < -15 || *units > 2) {
        Ctx()->ErrorFmt(
            span, "$timeformat units must be -15 to 2, got {}", *units);
        return hir::kInvalidExpressionId;
      }
      data.units = static_cast<int8_t>(*units);
    } else if (!args.empty()) {
      Ctx()->ErrorFmt(span, "$timeformat units must be a constant integer");
      return hir::kInvalidExpressionId;
    }

    // precision (arg 1): 0 to 17
    if (auto precision = extract_int(1)) {
      if (*precision < 0 || *precision > 17) {
        Ctx()->ErrorFmt(
            span, "$timeformat precision must be 0 to 17, got {}", *precision);
        return hir::kInvalidExpressionId;
      }
      data.precision = static_cast<int>(*precision);
    } else if (args.size() > 1) {
      Ctx()->ErrorFmt(span, "$timeformat precision must be a constant integer");
      return hir::kInvalidExpressionId;
    }

    // suffix (arg 2): string
    if (auto suffix = extract_string(2)) {
      data.suffix = *suffix;
    } else if (args.size() > 2) {
      Ctx()->ErrorFmt(span, "$timeformat suffix must be a constant string");
      return hir::kInvalidExpressionId;
    }

    // min_width (arg 3): >= 0
    if (auto min_width = extract_int(3)) {
      if (*min_width < 0) {
        Ctx()->ErrorFmt(
            span, "$timeformat min_width must be >= 0, got {}", *min_width);
        return hir::kInvalidExpressionId;
      }
      data.min_width = static_cast<int>(*min_width);
    } else if (args.size() > 3) {
      Ctx()->ErrorFmt(span, "$timeformat min_width must be a constant integer");
      return hir::kInvalidExpressionId;
    }

    return Ctx()->hir_arena->AddExpression(
        hir::Expression{
            .kind = hir::ExpressionKind::kSystemCall,
            .type = result_type,
            .span = span,
            .data = std::move(data)});
  }

  auto operator()(const MonitorFunctionInfo& info) const -> hir::ExpressionId {
    SourceSpan span = Ctx()->SpanOf(call->sourceRange);

    auto args = LowerArguments(*call, view);
    if (!args) {
      return hir::kInvalidExpressionId;
    }

    FormatKind default_format = RadixToFormatKind(info.radix);

    // Build format ops using the same logic as $display
    std::vector<const slang::ast::Expression*> slang_ptrs;
    slang_ptrs.reserve(call->arguments().size());
    for (const slang::ast::Expression* arg : call->arguments()) {
      slang_ptrs.push_back(arg);
    }

    std::vector<hir::FormatOp> ops = BuildDisplayFormatOps(
        slang_ptrs, *args, default_format, Ctx(), GetModuleTimeunitPower(view));

    // Check eligibility of each operand type for $monitor snapshotting.
    // Track argument index for error messages.
    size_t arg_idx = 0;
    for (const auto& op : ops) {
      if (!op.value.has_value()) {
        continue;  // Literal ops have no value to snapshot
      }
      const hir::Expression& expr = (*Ctx()->hir_arena)[*op.value];
      const Type& ty = (*Ctx()->type_arena)[expr.type];
      if (!IsMonitorEligibleType(ty)) {
        Ctx()->ErrorFmt(
            expr.span,
            "unsupported operand type '{}' for $monitor at argument {}",
            ToString(ty.Kind()), arg_idx + 1);
        return hir::kInvalidExpressionId;
      }
      ++arg_idx;
    }

    return Ctx()->hir_arena->AddExpression(
        hir::Expression{
            .kind = hir::ExpressionKind::kSystemCall,
            .type = result_type,
            .span = span,
            .data = hir::MonitorSystemCallData{
                .print_kind = PrintKind::kDisplay,  // Always with newline
                .ops = std::move(ops),
                .descriptor = std::nullopt}});
  }

  auto operator()(const MonitorControlFunctionInfo& info) const
      -> hir::ExpressionId {
    SourceSpan span = Ctx()->SpanOf(call->sourceRange);

    return Ctx()->hir_arena->AddExpression(
        hir::Expression{
            .kind = hir::ExpressionKind::kSystemCall,
            .type = result_type,
            .span = span,
            .data = hir::MonitorControlData{.enable = info.enable}});
  }
};

}  // namespace

auto BuildDisplayFormatOps(
    std::span<const slang::ast::Expression* const> slang_args,
    std::span<const hir::ExpressionId> hir_args, FormatKind default_format,
    Context* ctx, int8_t module_timeunit_power) -> std::vector<hir::FormatOp> {
  SourceSpan span = ctx->SpanOf(
      slang_args.empty() ? slang::SourceRange{} : slang_args[0]->sourceRange);
  std::vector<hir::FormatOp> ops;

  if (hir_args.empty()) {
    return ops;
  }

  // Check if first argument is a string literal with '%'
  const slang::ast::Expression* first_arg = slang_args[0];

  // Unwrap conversion if present (slang wraps string literals)
  if (first_arg->kind == slang::ast::ExpressionKind::Conversion) {
    const auto& conv = first_arg->as<slang::ast::ConversionExpression>();
    first_arg = &conv.operand();
  }

  bool has_format_string = false;
  std::string format_str;

  if (first_arg->kind == slang::ast::ExpressionKind::StringLiteral) {
    const auto& literal = first_arg->as<slang::ast::StringLiteral>();
    format_str = std::string(literal.getValue());
    has_format_string = true;
  }

  if (has_format_string && format_str.find('%') != std::string::npos) {
    // Parse format string with remaining arguments
    auto remaining_args = hir_args.subspan(1);
    auto parse_result = ParseFormatString(
        format_str, remaining_args, default_format, *ctx->sink, span,
        module_timeunit_power);
    ops = std::move(parse_result.ops);

    // Auto-format any remaining arguments not consumed by format string
    size_t next_arg = 1 + parse_result.args_consumed;
    while (next_arg < hir_args.size()) {
      ops.push_back(MakeAutoFormatOp(hir_args[next_arg], default_format, ctx));
      ++next_arg;
    }
  } else if (has_format_string) {
    // String without '%' - output as literal prefix + auto-format rest
    ops.push_back(
        hir::FormatOp{
            .kind = FormatKind::kLiteral,
            .value = std::nullopt,
            .literal = format_str,
            .mods = {}});
    for (size_t i = 1; i < hir_args.size(); ++i) {
      ops.push_back(MakeAutoFormatOp(hir_args[i], default_format, ctx));
    }
  } else {
    // First arg is not a string - auto-format all arguments
    for (hir::ExpressionId arg_id : hir_args) {
      ops.push_back(MakeAutoFormatOp(arg_id, default_format, ctx));
    }
  }

  return ops;
}

auto LowerSystemCall(
    const slang::ast::CallExpression& call, ExpressionLoweringView view)
    -> hir::ExpressionId {
  auto* ctx = view.context;
  SourceSpan span = ctx->SpanOf(call.sourceRange);
  std::string_view name = call.getSubroutineName();

  const SystemFunctionInfo* info = FindSystemFunction(name);
  if (info == nullptr) {
    ctx->ErrorFmt(span, "unsupported system call '{}'", name);
    return hir::kInvalidExpressionId;
  }

  // Validate argument count
  size_t arg_count = call.arguments().size();
  if (arg_count < info->min_args || arg_count > info->max_args) {
    ctx->ErrorFmt(
        span, "'{}' expects {}-{} arguments, got {}", name, info->min_args,
        info->max_args, arg_count);
    return hir::kInvalidExpressionId;
  }

  // Determine return type from registry metadata (exhaustive switch)
  TypeId result_type;
  switch (info->return_type) {
    case SystemFunctionReturnType::kVoid:
      result_type = ctx->type_arena->Intern(TypeKind::kVoid, std::monostate{});
      break;
    case SystemFunctionReturnType::kString:
      result_type =
          ctx->type_arena->Intern(TypeKind::kString, std::monostate{});
      break;
  }

  return std::visit(
      LowerVisitor{.call = &call, .view = view, .result_type = result_type},
      info->payload);
}

}  // namespace lyra::lowering::ast_to_hir
