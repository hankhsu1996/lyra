#include "lyra/mir/interp/format.hpp"

#include <cstddef>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/common/format.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/mir/interp/runtime_value.hpp"
#include "lyra/semantic/format.hpp"
#include "lyra/semantic/value.hpp"

namespace lyra::mir::interp {

namespace {

// Check if a type is signed (handles kIntegral and packed array types).
auto IsSignedIntegral(TypeId type, const TypeArena& types) -> bool {
  if (!type) {
    return false;
  }
  const auto& ty = types[type];
  if (ty.Kind() == TypeKind::kIntegral) {
    return ty.AsIntegral().is_signed;
  }
  if (IsPacked(ty)) {
    return IsPackedSigned(ty, types);
  }
  return false;
}

// Convert default_format char to FormatKind
auto CharToFormatKind(char c) -> FormatKind {
  switch (c) {
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
    default:
      return FormatKind::kDecimal;
  }
}

// Convert TypedValue to semantic::FormatArg
auto ToFormatArg(const TypedValue& typed, const TypeArena& types)
    -> semantic::FormatArg {
  return semantic::FormatArg{
      .value = semantic::Clone(typed.value),
      .is_signed = IsSignedIntegral(typed.type, types),
  };
}

// Auto-format a typed value using the default format kind.
// This handles the extra args policy from the plan.
auto AutoFormat(const TypedValue& arg, FormatKind kind, const TypeArena& types)
    -> std::string {
  bool is_signed = IsSignedIntegral(arg.type, types);
  semantic::FormatSpec spec;
  spec.kind = kind;
  // No explicit width - semantic layer will apply auto-sizing
  return semantic::FormatValue(arg.value, spec, is_signed);
}

}  // namespace

auto FormatValue(
    const TypedValue& arg, const FormatSpec& spec, const TypeArena& types,
    const FormatContext& /*ctx*/) -> std::string {
  bool is_signed = IsSignedIntegral(arg.type, types);
  return semantic::FormatValue(arg.value, spec, is_signed);
}

auto FormatDisplay(
    std::string_view fmt, std::span<const TypedValue> args, char default_format,
    const TypeArena& types, const FormatContext& /*ctx*/) -> std::string {
  // Convert TypedValues to FormatArgs
  std::vector<semantic::FormatArg> format_args;
  format_args.reserve(args.size());
  for (const auto& arg : args) {
    format_args.push_back(ToFormatArg(arg, types));
  }

  // Call semantic FormatMessage
  auto result = semantic::FormatMessage(fmt, format_args);

  if (!result.has_value()) {
    // Format errors should be caught by slang at compile time.
    // If we get here, it's a bug in AST->HIR->MIR lowering.
    throw common::InternalError(
        "FormatDisplay", "format error at runtime: " + result.error().message);
  }

  std::string output = std::move(result->output);

  // Handle extra args with default format
  FormatKind kind = CharToFormatKind(default_format);
  for (size_t i = result->args_consumed; i < args.size(); ++i) {
    output += AutoFormat(args[i], kind, types);
  }

  return output;
}

auto FormatMessage(
    std::span<const TypedValue> args, char default_format,
    const TypeArena& types, const FormatContext& ctx) -> std::string {
  if (args.empty()) {
    return "";
  }

  FormatKind kind = CharToFormatKind(default_format);

  // Check if first argument is a string
  if (IsString(args[0].value)) {
    const auto& first_str = AsString(args[0].value).value;

    // If string contains any '%', treat as format string
    if (first_str.find('%') != std::string::npos) {
      return FormatDisplay(
          first_str, args.subspan(1), default_format, types, ctx);
    }

    // String without '%': output as prefix + auto-format rest (no separator)
    std::string result = first_str;
    for (size_t i = 1; i < args.size(); ++i) {
      result += AutoFormat(args[i], kind, types);
    }
    return result;
  }

  // First arg is not a string: auto-format all (no separator)
  std::string result;
  for (const auto& arg : args) {
    result += AutoFormat(arg, kind, types);
  }
  return result;
}

}  // namespace lyra::mir::interp
