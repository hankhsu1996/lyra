#include "lyra/value/format.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/value/chandle.hpp"
#include "lyra/value/format_parse.hpp"
#include "lyra/value/integral_format.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/managed_ref.hpp"
#include "lyra/value/object_ref.hpp"
#include "lyra/value/string.hpp"

namespace lyra::value {

auto TimeUnitSpellingOf(std::int8_t power) -> TimeUnitSpelling {
  std::int8_t unit_power = power;
  while (unit_power % 3 != 0) {
    --unit_power;
  }
  std::uint64_t count = 1;
  for (std::int8_t i = unit_power; i < power; ++i) {
    count *= 10;
  }
  switch (unit_power) {
    case 0:
      return TimeUnitSpelling{.count = count, .unit = "s"};
    case -3:
      return TimeUnitSpelling{.count = count, .unit = "ms"};
    case -6:
      return TimeUnitSpelling{.count = count, .unit = "us"};
    case -9:
      return TimeUnitSpelling{.count = count, .unit = "ns"};
    case -12:
      return TimeUnitSpelling{.count = count, .unit = "ps"};
    case -15:
      return TimeUnitSpelling{.count = count, .unit = "fs"};
    default:
      throw InternalError(std::format("no time unit at 10^{}", power));
  }
}

auto TimeUnitText(std::int8_t power) -> std::string {
  const TimeUnitSpelling spelling = TimeUnitSpellingOf(power);
  return std::format("{}{}", spelling.count, spelling.unit);
}

namespace {

auto FieldFill(const FormatSpec& spec) -> char {
  switch (spec.kind) {
    case FormatKind::kHex:
    case FormatKind::kBinary:
    case FormatKind::kOctal:
      return '0';
    case FormatKind::kRealDecimal:
    case FormatKind::kRealExponential:
    case FormatKind::kRealGeneral:
      return spec.zero_pad ? '0' : ' ';
    case FormatKind::kDecimal:
    case FormatKind::kString:
    case FormatKind::kChar:
    case FormatKind::kAssignmentPattern:
    case FormatKind::kTime:
      return ' ';
  }
  throw InternalError("FieldFill: unknown format kind");
}

auto FormatRealBody(const FormatSpec& spec, double v) -> std::string {
  // LRM Table 21-2: %f decimal, %e exponential, %g shorter of the two.
  // Default precision matches C printf (6).
  const int precision = spec.precision >= 0 ? spec.precision : 6;
  switch (spec.kind) {
    case FormatKind::kRealExponential:
      return std::format("{:.{}e}", v, precision);
    case FormatKind::kRealGeneral:
      return std::format("{:.{}g}", v, precision);
    case FormatKind::kRealDecimal:
    default:
      return std::format("{:.{}f}", v, precision);
  }
}

}  // namespace

auto ApplyFieldWidth(std::string body, const FormatSpec& spec) -> std::string {
  if (spec.width <= 0) return body;
  const auto target = static_cast<std::size_t>(spec.width);
  if (body.size() >= target) return body;
  const std::size_t pad = target - body.size();
  if (spec.left_align) {
    body.append(pad, ' ');
    return body;
  }
  return std::string(pad, FieldFill(spec)) + std::move(body);
}

auto Format(const FormatSpec& spec, FormatArg arg, const FormatContext& ctx)
    -> std::string {
  // LRM 21.2.1.6 renders a value by facts its type declares, which the
  // operand brings with it because nothing here can reach them from a value.
  if (spec.kind == FormatKind::kAssignmentPattern && arg.pattern != nullptr) {
    return ApplyFieldWidth(std::string{arg.pattern->View()}, spec);
  }
  // An operand the clause defines under the assignment pattern and under no
  // other conversion carries no reading for this one, which is a program
  // asking for something the language does not define rather than anything
  // this can answer.
  if (arg.format_fn == nullptr) {
    throw SimulationError(
        "an aggregate is printed only by the assignment pattern conversion");
  }
  return arg.format_fn(spec, arg, ctx);
}

auto Format(std::span<const PrintItem> items, const TimeFormat& time_format)
    -> String {
  const FormatContext ctx{.time_format = &time_format};
  std::string out;
  for (const PrintItem& item : items) {
    std::visit(
        Overloaded{
            [&](const PrintLiteralItem& lit) {
              out.append(std::string_view{lit.data, lit.size});
            },
            [&](const PrintValueItem& v) {
              out.append(Format(v.spec, v.arg, ctx));
            },
        },
        item);
  }
  return String(std::move(out));
}

auto FormatRuntime(
    const String& format, std::span<const FormatArg> args,
    const String& scope_path, const String& library_binding,
    const TimeFormat& time_format, std::int64_t timeunit_power) -> String {
  const auto power = static_cast<std::int32_t>(timeunit_power);
  const FormatParseResult parsed = ParseFormatString(format.View());
  switch (parsed.error) {
    case FormatParseError::kNone:
      break;
    case FormatParseError::kUnsupportedSpecifier:
      throw SimulationError(
          "the '%{}' conversion is not yet supported", parsed.spec_char);
    case FormatParseError::kUnknownSpecifier:
    case FormatParseError::kWidthOverflow:
    case FormatParseError::kPrecisionOverflow:
    case FormatParseError::kModifierNotPermitted:
      throw SimulationError(
          "the format string is malformed at offset {}", parsed.error_offset);
  }

  const FormatContext ctx{.time_format = &time_format};
  const FormatArg path_arg{scope_path};
  const FormatArg binding_arg{library_binding};
  std::string out;
  std::size_t next_arg = 0;
  for (const FormatDirective& directive : parsed.directives) {
    const auto spec = [&](FormatKind kind) {
      FormatSpec s;
      s.kind = kind;
      s.width = directive.modifiers.width;
      s.precision = directive.modifiers.precision;
      s.zero_pad = directive.modifiers.zero_pad;
      s.left_align = directive.modifiers.left_align;
      s.timeunit_power = power;
      return s;
    };
    switch (directive.role) {
      case FormatDirective::Role::kLiteral:
        out.append(directive.literal);
        break;
      case FormatDirective::Role::kModulePath:
        out.append(Format(spec(FormatKind::kString), path_arg, ctx));
        break;
      case FormatDirective::Role::kLibraryBinding:
        out.append(Format(spec(FormatKind::kString), binding_arg, ctx));
        break;
      case FormatDirective::Role::kValue:
        // LRM 21.3.3: a directive with no operand left to take contributes
        // nothing and the format continues, rather than aborting the run.
        if (next_arg < args.size()) {
          out.append(Format(spec(directive.kind), args[next_arg], ctx));
          ++next_arg;
        }
        break;
    }
  }
  return String(std::move(out));
}

auto FormatIntegralOperand(
    const FormatSpec& spec, const ConstIntegralView& value,
    const FormatContext& ctx) -> std::string {
  if (spec.kind == FormatKind::kTime) {
    if (ctx.time_format == nullptr) {
      throw InternalError(
          "FormatIntegralOperand: %t requires a TimeFormat in the format "
          "context");
    }
    return FormatIntegralTime(spec, value, *ctx.time_format);
  }
  // LRM 21.2.1.6: an integral element of an assignment pattern prints "as it
  // would unformatted", and the default $display radix is decimal. It occupies
  // no field: the white space a pattern carries is the tool's to choose, and an
  // element sits inside a pattern rather than in the columns the directive
  // asked of the whole. The rewrite is here rather than at whatever reached the
  // element, so an integral answers alike whether it is the whole operand or
  // one position of a pattern something else composed.
  if (spec.kind == FormatKind::kAssignmentPattern) {
    FormatSpec element_spec = spec;
    element_spec.kind = FormatKind::kDecimal;
    element_spec.width = 0;
    return FormatIntegral(element_spec, value);
  }
  return FormatIntegral(spec, value);
}

auto Formatter<String>::Format(const FormatSpec& spec, const String& value)
    -> std::string {
  if (spec.kind == FormatKind::kTime) {
    // LRM 21.2.1.3.
    throw SimulationError("a string operand cannot be formatted as '%t'");
  }
  std::string body{value.View()};
  // LRM 21.2.1.6: a string element of an assignment pattern prints quoted, and
  // like every element occupies no field of its own.
  if (spec.kind == FormatKind::kAssignmentPattern) {
    return "\"" + std::move(body) + "\"";
  }
  return ApplyFieldWidth(std::move(body), spec);
}

namespace {

auto FormatHandleIdentity(const FormatSpec& spec, const void* identity)
    -> std::string {
  if (spec.kind != FormatKind::kAssignmentPattern) {
    // LRM 21.2.1.6.
    throw SimulationError(
        "a handle is printed only by the assignment pattern conversion");
  }
  if (identity == nullptr) {
    return std::string{"null"};
  }
  return std::format("{}", identity);
}

}  // namespace

auto Formatter<Chandle>::Format(const FormatSpec& spec, const Chandle& value)
    -> std::string {
  return FormatHandleIdentity(spec, value.Ptr());
}

auto Formatter<ObjectRef>::Format(
    const FormatSpec& spec, const ObjectRef& value) -> std::string {
  return FormatHandleIdentity(spec, value.Handle().Share().get());
}

auto Formatter<double>::Format(
    const FormatSpec& spec, double value, const FormatContext& ctx)
    -> std::string {
  if (spec.kind == FormatKind::kTime) {
    if (ctx.time_format == nullptr) {
      throw InternalError(
          "Formatter<double>::Format: %t requires a TimeFormat in the format "
          "context");
    }
    return FormatRealTime(spec, value, *ctx.time_format);
  }
  return ApplyFieldWidth(FormatRealBody(spec, value), spec);
}

auto Formatter<float>::Format(
    const FormatSpec& spec, float value, const FormatContext& ctx)
    -> std::string {
  if (spec.kind == FormatKind::kTime) {
    if (ctx.time_format == nullptr) {
      throw InternalError(
          "Formatter<float>::Format: %t requires a TimeFormat in the format "
          "context");
    }
    return FormatRealTime(spec, static_cast<double>(value), *ctx.time_format);
  }
  return ApplyFieldWidth(
      FormatRealBody(spec, static_cast<double>(value)), spec);
}

FormatSpec::FormatSpec(
    std::int64_t kind, std::int64_t width, std::int64_t precision,
    std::int64_t zero_pad, std::int64_t left_align, std::int64_t timeunit_power)
    : kind(static_cast<FormatKind>(kind)),
      width(static_cast<std::int32_t>(width)),
      precision(static_cast<std::int32_t>(precision)),
      zero_pad(zero_pad != 0),
      left_align(left_align != 0),
      timeunit_power(static_cast<std::int32_t>(timeunit_power)) {
}

PrintLiteralItem::PrintLiteralItem(const String& text)
    : data(text.View().data()),
      size(static_cast<std::uint32_t>(text.View().size())) {
}

}  // namespace lyra::value
