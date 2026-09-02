#include "lyra/value/format.hpp"

#include <array>
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
#include "lyra/value/format_parse.hpp"
#include "lyra/value/integral_format.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/string.hpp"

namespace lyra::value {

auto TimeUnitText(std::int8_t power) -> std::string {
  constexpr std::array<std::string_view, 6> kUnits = {"s",  "ms", "us",
                                                      "ns", "ps", "fs"};
  constexpr std::array<std::string_view, 3> kMantissa = {"1", "10", "100"};
  // A time power is a small signed number (LRM 3.14): -9 is ns, so widening it
  // with sign extension is the meaning, not the byte read this check looks for.
  // NOLINTNEXTLINE(bugprone-signed-char-misuse)
  const int p = static_cast<int>(power);
  const int index = (-p + 2) / 3;
  const int mantissa_exp = p + (3 * index);
  const std::string_view unit = (index >= 0 && index < 6)
                                    ? kUnits.at(static_cast<std::size_t>(index))
                                    : "?";
  const std::string_view mantissa =
      (mantissa_exp >= 0 && mantissa_exp < 3)
          ? kMantissa.at(static_cast<std::size_t>(mantissa_exp))
          : "?";
  return std::string(mantissa) + std::string(unit);
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

auto PackedArrayLowWord(const PackedArray& pa) -> std::uint64_t {
  const auto words = pa.ValueWords();
  return words.empty() ? 0U : words[0];
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
  return arg.format_fn(spec, arg.ptr, ctx);
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
    const String& scope_path, const TimeFormat& time_format,
    const PackedArray& timeunit_power) -> String {
  const auto power = static_cast<std::int32_t>(timeunit_power.ToInt64());
  const FormatParseResult parsed = ParseFormatString(format.View());
  if (parsed.error != FormatParseError::kNone) {
    throw SimulationError(
        std::format(
            "malformed format string at offset {}", parsed.error_offset));
  }

  const FormatContext ctx{.time_format = &time_format};
  const FormatArg path_arg{scope_path};
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

auto Formatter<PackedArray>::Format(
    const FormatSpec& spec, const PackedArray& value, const FormatContext& ctx)
    -> std::string {
  if (spec.kind == FormatKind::kTime) {
    if (ctx.time_format == nullptr) {
      throw InternalError(
          "Formatter<PackedArray>::Format: %t requires a TimeFormat in the "
          "format context");
    }
    return FormatTimeMagnitude(
        spec, static_cast<double>(PackedArrayLowWord(value)), *ctx.time_format);
  }
  // LRM 21.2.1.6: an integral element of an assignment pattern prints "as it
  // would unformatted", and the default $display radix is decimal. It occupies
  // no field: the white space a pattern carries is the tool's to choose, and an
  // element sits inside a pattern rather than in the columns the directive
  // asked of the whole. Rewriting the spec here is what makes that true at
  // every depth, since the recursion through an aggregate hands each element
  // the pattern spec the directive was written with.
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
    throw SimulationError(
        "a string operand cannot be formatted as %t (LRM 21.2.1.3)");
  }
  std::string body{value.View()};
  // LRM 21.2.1.6: a string element of an assignment pattern prints quoted, and
  // like every element occupies no field of its own.
  if (spec.kind == FormatKind::kAssignmentPattern) {
    return "\"" + std::move(body) + "\"";
  }
  return ApplyFieldWidth(std::move(body), spec);
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
    return FormatTimeMagnitude(spec, value, *ctx.time_format);
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
    return FormatTimeMagnitude(
        spec, static_cast<double>(value), *ctx.time_format);
  }
  return ApplyFieldWidth(
      FormatRealBody(spec, static_cast<double>(value)), spec);
}

FormatSpec::FormatSpec(const PackedArray& kind)
    : kind(static_cast<FormatKind>(kind.ToInt64())) {
}

FormatSpec::FormatSpec(
    const PackedArray& kind, const PackedArray& width,
    const PackedArray& precision, const PackedArray& zero_pad,
    const PackedArray& left_align, const PackedArray& timeunit_power)
    : kind(static_cast<FormatKind>(kind.ToInt64())),
      width(static_cast<std::int32_t>(width.ToInt64())),
      precision(static_cast<std::int32_t>(precision.ToInt64())),
      zero_pad(zero_pad.ToInt64() != 0),
      left_align(left_align.ToInt64() != 0),
      timeunit_power(static_cast<std::int32_t>(timeunit_power.ToInt64())) {
}

PrintLiteralItem::PrintLiteralItem(const String& text)
    : data(text.View().data()),
      size(static_cast<std::uint32_t>(text.View().size())) {
}

}  // namespace lyra::value
