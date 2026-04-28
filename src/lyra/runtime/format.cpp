#include "lyra/runtime/format.hpp"

#include <cstdint>
#include <format>
#include <string>
#include <utility>

#include "lyra/base/internal_error.hpp"

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
    const FormatSpec& spec, const RuntimeValueView& v, FormatKind kind)
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

auto FormatIntegralNarrow(const FormatSpec& spec, const RuntimeValueView& v)
    -> std::string {
  if (v.unknown_words != nullptr && *v.unknown_words != 0) {
    throw InternalError(
        "FormatIntegralNarrow: 4-state X/Z formatting not implemented");
  }
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
  // Auto-width already applied for radix kinds; for kDecimal apply spec.width.
  if (spec.kind == FormatKind::kDecimal) {
    return ApplyWidth(std::move(body), spec);
  }
  // Apply spec.width if explicitly larger than the rendered body.
  return ApplyWidth(std::move(body), spec);
}

}  // namespace

auto FormatValue(const FormatSpec& spec, const RuntimeValueView& value)
    -> std::string {
  switch (value.kind) {
    case RuntimeValueKind::kIntegral:
      if (value.word_count > 1) {
        throw InternalError(
            "FormatValue: wide integrals not implemented in this cut");
      }
      return FormatIntegralNarrow(spec, value);
    case RuntimeValueKind::kString: {
      std::string body(value.string_data, value.string_size);
      return ApplyWidth(std::move(body), spec);
    }
  }
  throw InternalError(
      "FormatValue: backend emitted unsupported RuntimeValueKind");
}

}  // namespace lyra::runtime
