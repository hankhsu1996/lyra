#include "lyra/value/format.hpp"

#include <cstdint>
#include <format>
#include <limits>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/value/integral_format.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/packed_internal.hpp"

namespace lyra::value {

auto IntegralValueView::Narrow(
    std::uint64_t value_word, std::uint64_t unknown_word,
    std::uint64_t bit_width, IntegralStateKind state_kind, bool is_signed)
    -> IntegralValueView {
  if (bit_width == 0U) {
    throw InternalError("IntegralValueView::Narrow: bit_width must be >= 1");
  }
  if (bit_width > 64U) {
    throw InternalError(
        "IntegralValueView::Narrow: bit_width > 64 must use Wide");
  }
  if (state_kind == IntegralStateKind::kTwoState && unknown_word != 0U) {
    throw InternalError(
        "IntegralValueView::Narrow: 2-state must have unknown_word == 0");
  }
  return IntegralValueView{
      .data = NarrowIntegralView{
          .value_word = value_word,
          .unknown_word = unknown_word,
          .bit_width = bit_width,
          .state_kind = state_kind,
          .is_signed = is_signed,
      }};
}

auto IntegralValueView::Wide(
    std::span<const std::uint64_t> value_words,
    std::span<const std::uint64_t> unknown_words, std::uint64_t bit_width,
    IntegralStateKind state_kind, bool is_signed) -> IntegralValueView {
  if (bit_width == 0U) {
    throw InternalError("IntegralValueView::Wide: bit_width must be >= 1");
  }
  const std::size_t expected = WordCountForBits(bit_width);
  if (value_words.size() != expected) {
    throw InternalError(
        std::format(
            "IntegralValueView::Wide: value word count mismatch ({} vs {})",
            value_words.size(), expected));
  }
  switch (state_kind) {
    case IntegralStateKind::kTwoState:
      if (!unknown_words.empty()) {
        throw InternalError(
            "IntegralValueView::Wide: 2-state must have empty unknown_words");
      }
      break;
    case IntegralStateKind::kFourState:
      if (unknown_words.size() != value_words.size()) {
        throw InternalError(
            std::format(
                "IntegralValueView::Wide: unknown word count mismatch "
                "({} vs {})",
                unknown_words.size(), value_words.size()));
      }
      break;
  }
  return IntegralValueView{
      .data = WideIntegralView{
          .value_words = value_words,
          .unknown_words = unknown_words,
          .bit_width = bit_width,
          .state_kind = state_kind,
          .is_signed = is_signed,
      }};
}

auto IntegralValueView::BitWidth() const -> std::uint64_t {
  return std::visit([](const auto& v) { return v.bit_width; }, data);
}

auto IntegralValueView::StateKind() const -> IntegralStateKind {
  return std::visit([](const auto& v) { return v.state_kind; }, data);
}

auto IntegralValueView::IsSigned() const -> bool {
  return std::visit([](const auto& v) { return v.is_signed; }, data);
}

auto IntegralValueView::IsWide() const -> bool {
  return std::holds_alternative<WideIntegralView>(data);
}

auto RuntimeValueView::NarrowIntegral(
    std::uint64_t word, std::uint32_t bit_width, bool is_signed)
    -> RuntimeValueView {
  return RuntimeValueView{
      .data = IntegralValueView::Narrow(
          word, 0, bit_width, IntegralStateKind::kTwoState, is_signed)};
}

auto RuntimeValueView::String(std::string_view sv) -> RuntimeValueView {
  if (sv.size() > std::numeric_limits<std::uint32_t>::max()) {
    throw InternalError("RuntimeValueView::String: string too large");
  }
  return RuntimeValueView{
      .data = StringValueView{
          .data = sv.data(),
          .size = static_cast<std::uint32_t>(sv.size()),
      }};
}

auto RuntimeValueView::Real64(double v) -> RuntimeValueView {
  return RuntimeValueView{.data = Real64ValueView{.value = v}};
}

auto RuntimeValueView::Real32(float v) -> RuntimeValueView {
  return RuntimeValueView{.data = Real32ValueView{.value = v}};
}

auto RuntimeValueView::FromBitView(ConstBitView view, bool is_signed)
    -> RuntimeValueView {
  if (detail::PackedAccess::BitOffset(view) != 0U) {
    throw InternalError("RuntimeValueView::FromBitView: bit_offset must be 0");
  }
  const auto value_words = detail::PackedAccess::ValueWords(view);
  return RuntimeValueView{
      .data = IntegralValueView::Wide(
          value_words, std::span<const std::uint64_t>{}, view.Width(),
          IntegralStateKind::kTwoState, is_signed)};
}

auto RuntimeValueView::FromLogicView(ConstLogicView view, bool is_signed)
    -> RuntimeValueView {
  if (detail::PackedAccess::BitOffset(view) != 0U) {
    throw InternalError(
        "RuntimeValueView::FromLogicView: bit_offset must be 0");
  }
  const auto value_words = detail::PackedAccess::ValueWords(view);
  const auto unknown_words = detail::PackedAccess::UnknownWords(view);
  return RuntimeValueView{
      .data = IntegralValueView::Wide(
          value_words, unknown_words, view.Width(),
          IntegralStateKind::kFourState, is_signed)};
}

auto RuntimeValueView::FromPackedArray(const PackedArray& pa)
    -> RuntimeValueView {
  const auto state_kind = pa.IsFourState() ? IntegralStateKind::kFourState
                                           : IntegralStateKind::kTwoState;
  return RuntimeValueView{
      .data = IntegralValueView::Wide(
          pa.ValueWords(), pa.UnknownWords(), pa.BitWidth(), state_kind,
          pa.IsSigned())};
}

namespace {

auto ApplyStringWidth(std::string body, const FormatSpec& spec) -> std::string {
  if (spec.width <= 0) return body;
  const auto target = static_cast<std::size_t>(spec.width);
  if (body.size() >= target) return body;
  const std::size_t pad = target - body.size();
  const char fill = spec.zero_pad ? '0' : ' ';
  if (spec.left_align) {
    body.append(pad, ' ');
    return body;
  }
  return std::string(pad, fill) + std::move(body);
}

}  // namespace

namespace {

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

auto Pow10Double(int exp) -> double {
  double result = 1.0;
  for (int i = 0; i < exp; ++i) {
    result *= 10.0;
  }
  for (int i = 0; i < -exp; ++i) {
    result /= 10.0;
  }
  return result;
}

auto TimeOperandMagnitude(const RuntimeValueView& value) -> double {
  if (const auto* integral = std::get_if<IntegralValueView>(&value.data)) {
    return std::visit(
        Overloaded{
            [](const NarrowIntegralView& n) {
              return static_cast<double>(n.value_word);
            },
            [](const WideIntegralView& w) {
              return w.value_words.empty()
                         ? 0.0
                         : static_cast<double>(w.value_words[0]);
            }},
        integral->data);
  }
  if (const auto* real64 = std::get_if<Real64ValueView>(&value.data)) {
    return real64->value;
  }
  if (const auto* real32 = std::get_if<Real32ValueView>(&value.data)) {
    return static_cast<double>(real32->value);
  }
  throw InternalError("FormatTime: %t operand is not numeric");
}

}  // namespace

auto FormatTime(
    const FormatSpec& spec, const RuntimeValueView& value, const TimeFormat& tf)
    -> std::string {
  const double scaled = TimeOperandMagnitude(value) *
                        Pow10Double(spec.timeunit_power - tf.units_power);
  const int precision = tf.precision >= 0 ? tf.precision : 0;
  std::string body = std::format("{:.{}f}", scaled, precision);
  body += tf.suffix;
  if (tf.min_width > 0 &&
      body.size() < static_cast<std::size_t>(tf.min_width)) {
    body.insert(0, static_cast<std::size_t>(tf.min_width) - body.size(), ' ');
  }
  return body;
}

auto FormatValue(const FormatSpec& spec, const RuntimeValueView& value)
    -> std::string {
  return std::visit(
      Overloaded{
          [&](const IntegralValueView& v) -> std::string {
            // LRM 21.2.1.6: a singular integral type with %p prints "as it
            // would unformatted" -- the default $display radix is decimal.
            if (spec.kind == FormatKind::kAssignmentPattern) {
              FormatSpec decimal_spec = spec;
              decimal_spec.kind = FormatKind::kDecimal;
              return FormatIntegral(decimal_spec, v);
            }
            return FormatIntegral(spec, v);
          },
          [&](const StringValueView& v) -> std::string {
            std::string body(v.data, v.size);
            // LRM 21.2.1.6: a string with %p prints quoted.
            if (spec.kind == FormatKind::kAssignmentPattern) {
              return ApplyStringWidth("\"" + std::move(body) + "\"", spec);
            }
            return ApplyStringWidth(std::move(body), spec);
          },
          [&](const Real64ValueView& v) -> std::string {
            return ApplyStringWidth(FormatRealBody(spec, v.value), spec);
          },
          [&](const Real32ValueView& v) -> std::string {
            return ApplyStringWidth(
                FormatRealBody(spec, static_cast<double>(v.value)), spec);
          },
          [&](const UnpackedArrayValueView& v) -> std::string {
            // LRM 21.2.1.6: aggregate operand prints as assignment pattern.
            // Each element recurses through FormatValue with the same spec;
            // the singular arms above implement the LRM per-type element
            // rules (integral -> decimal, string -> quoted). Multi-dim
            // composes naturally because nested elements are themselves
            // UnpackedArrayValueView.
            std::string out;
            out += "'{";
            for (std::size_t i = 0; i < v.elements.size(); ++i) {
              if (i != 0) {
                out += ", ";
              }
              out += FormatValue(spec, v.elements[i]);
            }
            out += "}";
            return out;
          },
      },
      value.data);
}

}  // namespace lyra::value
