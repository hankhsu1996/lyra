#include "lyra/runtime/format.hpp"

#include <cstdint>
#include <format>
#include <limits>
#include <string>
#include <string_view>
#include <utility>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"

namespace lyra::runtime {

namespace {

auto MaskFor(std::uint32_t bit_width) -> std::uint64_t {
  if (bit_width >= 64) return ~std::uint64_t{0};
  return (std::uint64_t{1} << bit_width) - 1;
}

auto ApplyWidth(std::string body, const FormatSpec& spec) -> std::string {
  if (spec.width <= 0) return body;
  const auto target = static_cast<std::size_t>(spec.width);
  if (body.size() >= target) return body;
  const std::size_t pad = target - body.size();
  const char fill = spec.zero_pad ? '0' : ' ';
  if (spec.left_align) {
    body.append(pad, fill);
    return body;
  }
  return std::string(pad, fill) + std::move(body);
}

auto FormatDecimalSigned(std::uint64_t raw, std::uint32_t bit_width)
    -> std::string {
  // Sign-extend `raw` (already masked to bit_width) into int64_t.
  std::int64_t signed_value = 0;
  if (bit_width >= 64) {
    signed_value = static_cast<std::int64_t>(raw);
  } else {
    const std::uint64_t sign_bit = std::uint64_t{1} << (bit_width - 1);
    if ((raw & sign_bit) != 0) {
      // Set the upper bits to 1 to sign-extend.
      const std::uint64_t extension = ~MaskFor(bit_width);
      signed_value = static_cast<std::int64_t>(raw | extension);
    } else {
      signed_value = static_cast<std::int64_t>(raw);
    }
  }
  return std::format("{}", signed_value);
}

auto FormatDecimalUnsigned(std::uint64_t raw) -> std::string {
  return std::format("{}", raw);
}

auto ResolveAutoWidth(
    const FormatSpec& spec, const IntegralValueView& v, FormatKind kind)
    -> std::int32_t {
  if (spec.width >= 0) return spec.width;
  // Auto-width when no explicit width specified (-1 sentinel).
  switch (kind) {
    case FormatKind::kHex:
      return static_cast<std::int32_t>((v.bit_width + 3) / 4);
    case FormatKind::kBinary:
      return static_cast<std::int32_t>(v.bit_width);
    case FormatKind::kOctal:
      return static_cast<std::int32_t>((v.bit_width + 2) / 3);
    default:
      return 0;
  }
}

auto FormatHex(std::uint64_t raw, std::int32_t width) -> std::string {
  return std::format(
      "{:0{}x}", raw, width > 0 ? static_cast<std::size_t>(width) : 0);
}

auto FormatBinary(std::uint64_t raw, std::int32_t width) -> std::string {
  return std::format(
      "{:0{}b}", raw, width > 0 ? static_cast<std::size_t>(width) : 0);
}

auto FormatOctal(std::uint64_t raw, std::int32_t width) -> std::string {
  return std::format(
      "{:0{}o}", raw, width > 0 ? static_cast<std::size_t>(width) : 0);
}

auto FormatIntegralNarrow(const FormatSpec& spec, const IntegralValueView& v)
    -> std::string {
  // 4-state values route through FormatIntegralFourStateNarrow before
  // reaching this function, so this path always sees pure 2-state input.
  const std::uint64_t raw = v.inline_word & MaskFor(v.bit_width);
  std::string body;
  switch (spec.kind) {
    case FormatKind::kDecimal:
      body = v.is_signed ? FormatDecimalSigned(raw, v.bit_width)
                         : FormatDecimalUnsigned(raw);
      break;
    case FormatKind::kHex:
      body = FormatHex(raw, ResolveAutoWidth(spec, v, FormatKind::kHex));
      break;
    case FormatKind::kBinary:
      body = FormatBinary(raw, ResolveAutoWidth(spec, v, FormatKind::kBinary));
      break;
    case FormatKind::kOctal:
      body = FormatOctal(raw, ResolveAutoWidth(spec, v, FormatKind::kOctal));
      break;
    default:
      throw InternalError(
          "FormatIntegralNarrow: backend emitted unsupported FormatKind");
  }
  return ApplyWidth(std::move(body), spec);
}

// Binary-only 4-state narrow formatter: walks each bit MSB->LSB and emits
// '0'/'1'/'x'/'z' from the (value, state) plane pair. Decimal/hex/octal of
// 4-state runtime packed values are not yet implemented.
auto FormatIntegralFourStateNarrow(
    const FormatSpec& spec, const IntegralValueView& v) -> std::string {
  if (spec.kind != FormatKind::kBinary) {
    throw InternalError(
        "FormatIntegralFourStateNarrow: only binary is supported");
  }
  std::string body;
  body.reserve(v.bit_width);
  for (std::int32_t i = static_cast<std::int32_t>(v.bit_width) - 1; i >= 0;
       --i) {
    const bool value = ((v.inline_word >> i) & 1U) != 0U;
    const bool unknown = ((v.inline_unknown_word >> i) & 1U) != 0U;
    char ch = '0';
    if (unknown) {
      ch = value ? 'x' : 'z';
    } else {
      ch = value ? '1' : '0';
    }
    body.push_back(ch);
  }
  return ApplyWidth(std::move(body), spec);
}

}  // namespace

auto RuntimeValueView::NarrowIntegral(
    std::uint64_t word, std::uint32_t bit_width, bool is_signed)
    -> RuntimeValueView {
  return RuntimeValueView{
      .data = IntegralValueView{
          .state = IntegralStateKind::kTwoState,
          .inline_word = word,
          .bit_width = bit_width,
          .is_signed = is_signed,
      }};
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

auto FormatValue(const FormatSpec& spec, const RuntimeValueView& value)
    -> std::string {
  return std::visit(
      Overloaded{
          [&](const IntegralValueView& v) -> std::string {
            switch (v.state) {
              case IntegralStateKind::kTwoState:
                return FormatIntegralNarrow(spec, v);
              case IntegralStateKind::kFourState:
                return FormatIntegralFourStateNarrow(spec, v);
            }
            throw InternalError(
                "FormatValue: backend emitted unsupported IntegralStateKind");
          },
          [&](const StringValueView& v) -> std::string {
            std::string body(v.data, v.size);
            return ApplyWidth(std::move(body), spec);
          },
      },
      value.data);
}

}  // namespace lyra::runtime
