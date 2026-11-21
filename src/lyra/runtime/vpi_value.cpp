#include "lyra/runtime/vpi_value.hpp"

#include <algorithm>
#include <array>
#include <bit>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/overloaded.hpp"
#include "lyra/runtime/vpi_object.hpp"
#include "lyra/runtime/vpi_variable.hpp"
#include "lyra/value/bit_vec.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/integral_format.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/real.hpp"
#include "lyra/value/string.hpp"
#include "vpi_user.h"

namespace lyra::runtime {

namespace {

// The union `s_vpi_value` holds its value in, which is the C ABI's (LRM
// Figure 38-7): the format says which member foreign code reads or wrote.
// Each member begins at the union's first byte, so a member is read and
// written as those bytes, which is defined whichever member was written last.
// A member pointing at a structure is read and written as an untyped address,
// which has the same bytes, and converted where it is used.
using ValueUnion = decltype(s_vpi_value::value);

template <typename Member>
auto MemberOf(const ValueUnion& held) -> Member {
  const auto bytes =
      std::bit_cast<std::array<std::byte, sizeof(ValueUnion)>>(held);
  std::array<std::byte, sizeof(Member)> leading{};
  std::ranges::copy_n(bytes.begin(), sizeof(Member), leading.begin());
  return std::bit_cast<Member>(leading);
}

template <typename Member>
void SetMember(ValueUnion& held, Member member) {
  std::array<std::byte, sizeof(ValueUnion)> bytes{};
  std::ranges::copy(
      std::bit_cast<std::array<std::byte, sizeof(Member)>>(member),
      bytes.begin());
  held = std::bit_cast<ValueUnion>(bytes);
}

auto FormatDoesNotApply() -> std::unexpected<std::string> {
  return std::unexpected<std::string>(
      "the value format does not apply to the object");
}

// The bits `bits` of a value of `shape`, signed as the shape is.
auto SignedView(const value::BitVec& bits, const IntegralShape& shape)
    -> value::ConstIntegralView {
  value::ConstIntegralView view = bits.View();
  view.signedness = shape.is_signed ? value::Signedness::kSigned
                                    : value::Signedness::kUnsigned;
  return view;
}

auto Formatted(
    value::FormatKind kind, std::int32_t width,
    const value::ConstIntegralView& view) -> std::string {
  value::FormatSpec spec;
  spec.kind = kind;
  spec.width = width;
  return value::FormatIntegral(spec, view);
}

// The 32 bits of `plane` from bit `at` up.
auto Chunk(std::span<const std::uint64_t> plane, std::uint64_t at)
    -> PLI_UINT32 {
  const std::size_t word = at / 64;
  if (word >= plane.size()) return 0;
  return static_cast<PLI_UINT32>(plane[word] >> (at % 64));
}

// The scalar bit `at` holds, as VPI names one.
auto ScalarAt(const value::ConstIntegralView& view, std::uint64_t at)
    -> PLI_INT32 {
  const bool one = ((Chunk(view.value, at) & 1U) != 0U);
  const bool unknown = ((Chunk(view.unknown, at) & 1U) != 0U);
  if (unknown) return one ? vpiX : vpiZ;
  return one ? vpi1 : vpi0;
}

// A real converted to an `int` as an assignment converts it, by the value
// library's conversion every real into an integral type takes (LRM 6.12.1).
auto RoundedToInt(double real) -> PLI_INT32 {
  value::BitVec bits = value::BitVec::Zeros(32);
  value::Real(real).RoundInto(bits.MutableView());
  value::ConstIntegralView number = bits.View();
  number.signedness = value::Signedness::kSigned;
  return static_cast<PLI_INT32>(value::ToInt64(number));
}

auto FillIntegral(
    const value::ConstIntegralView& view, PLI_INT32 format, s_vpi_value& value,
    VpiValueStore& store) -> VpiAnswer<void> {
  switch (format) {
    case vpiBinStrVal:
      store.text = Formatted(value::FormatKind::kBinary, -1, view);
      SetMember<PLI_BYTE8*>(value.value, store.text.data());
      return {};
    case vpiOctStrVal:
      store.text = Formatted(value::FormatKind::kOctal, -1, view);
      SetMember<PLI_BYTE8*>(value.value, store.text.data());
      return {};
    case vpiHexStrVal:
      store.text = Formatted(value::FormatKind::kHex, -1, view);
      SetMember<PLI_BYTE8*>(value.value, store.text.data());
      return {};
    case vpiDecStrVal:
      store.text = Formatted(value::FormatKind::kDecimal, 0, view);
      SetMember<PLI_BYTE8*>(value.value, store.text.data());
      return {};
    case vpiStringVal:
      store.text = std::string(value::String::FromIntegral(view).View());
      SetMember<PLI_BYTE8*>(value.value, store.text.data());
      return {};
    case vpiScalarVal:
      SetMember<PLI_INT32>(value.value, ScalarAt(view, 0));
      return {};
    case vpiIntVal:
      SetMember<PLI_INT32>(
          value.value, static_cast<PLI_INT32>(value::ToInt64(view)));
      return {};
    case vpiRealVal:
      SetMember<double>(value.value, value::ToFloating<double>(view));
      return {};
    case vpiTimeVal: {
      const auto number = static_cast<std::uint64_t>(value::ToInt64(view));
      store.time = s_vpi_time{
          .type = vpiSimTime,
          .high = static_cast<PLI_UINT32>(number >> 32U),
          .low = static_cast<PLI_UINT32>(number),
          .real = 0.0};
      SetMember<void*>(value.value, &store.time);
      return {};
    }
    case vpiVectorVal: {
      const std::uint64_t words = (view.width + 31) / 32;
      store.vector.assign(words, s_vpi_vecval{});
      for (std::uint64_t at = 0; at < words; ++at) {
        store.vector[at] = s_vpi_vecval{
            .aval = Chunk(view.value, at * 32),
            .bval = Chunk(view.unknown, at * 32)};
      }
      SetMember<void*>(value.value, store.vector.data());
      return {};
    }
    // A variable's every bit is of strong strength (LRM 38.15).
    case vpiStrengthVal: {
      store.strength.assign(view.width, s_vpi_strengthval{});
      for (std::uint64_t at = 0; at < view.width; ++at) {
        store.strength[at] = s_vpi_strengthval{
            .logic = ScalarAt(view, at),
            .s0 = vpiStrongDrive,
            .s1 = vpiStrongDrive};
      }
      SetMember<void*>(value.value, store.strength.data());
      return {};
    }
    default:
      return FormatDoesNotApply();
  }
}

auto FillReal(double real, PLI_INT32 format, s_vpi_value& value)
    -> VpiAnswer<void> {
  switch (format) {
    case vpiRealVal:
      SetMember<double>(value.value, real);
      return {};
    case vpiIntVal:
      SetMember<PLI_INT32>(value.value, RoundedToInt(real));
      return {};
    default:
      return FormatDoesNotApply();
  }
}

auto FillText(
    const value::String& text, PLI_INT32 format, s_vpi_value& value,
    VpiValueStore& store) -> VpiAnswer<void> {
  if (format != vpiStringVal) return FormatDoesNotApply();
  store.text = std::string(text.View());
  SetMember<PLI_BYTE8*>(value.value, store.text.data());
  return {};
}

// Sets the bits of `bits` a value of VPI's own scalar names at `at` (LRM
// 38.15): an H as a 1 and an L as a 0, since a variable holds no strength.
auto SetScalar(value::BitVec& bits, std::uint64_t at, PLI_INT32 scalar)
    -> bool {
  bool one = false;
  bool unknown = false;
  switch (scalar) {
    case vpi0:
    case vpiL:
      break;
    case vpi1:
    case vpiH:
      one = true;
      break;
    case vpiX:
      one = true;
      unknown = true;
      break;
    case vpiZ:
      unknown = true;
      break;
    default:
      return false;
  }
  const value::IntegralView view = bits.MutableView();
  const std::uint64_t mask = std::uint64_t{1} << (at % 64);
  if (one) view.value[at / 64] |= mask;
  if (unknown && !view.unknown.empty()) view.unknown[at / 64] |= mask;
  return true;
}

// What `text` written in `radix` is as `width` bits, none for text holding
// no number of the radix (LRM 21.4 reads its digits the same way).
auto DigitBits(
    std::string_view text, value::DigitRadix radix, std::uint64_t width)
    -> std::optional<value::BitVec> {
  if (!value::IsDigits(radix, text)) return std::nullopt;
  value::BitVec bits = value::BitVec::Zeros(width);
  value::FromDigits(bits.MutableView(), radix, text);
  return bits;
}

// The characters of `text` as bits, its last character the least significant
// byte, as a string assigned to an integral value is (LRM 6.16).
auto TextBits(std::string_view text, std::uint64_t width) -> value::BitVec {
  value::BitVec bits = value::BitVec::Zeros(width);
  const value::IntegralView view = bits.MutableView();
  for (std::size_t at = 0; at < text.size(); ++at) {
    const std::uint64_t position = 8U * (text.size() - 1 - at);
    if (position >= width) continue;
    view.value[position / 64] |=
        static_cast<std::uint64_t>(static_cast<unsigned char>(text[at]))
        << (position % 64);
  }
  value::ClearAboveWidth(view.value, view.width);
  return bits;
}

auto IntegralToWrite(const IntegralShape& shape, const s_vpi_value& value)
    -> VpiAnswer<HeldValue> {
  const auto held = [&](value::BitVec bits) -> VpiAnswer<HeldValue> {
    return HeldValue{std::move(bits)};
  };
  const auto from_text =
      [&](std::optional<value::BitVec> bits) -> VpiAnswer<HeldValue> {
    if (!bits.has_value()) {
      return std::unexpected<std::string>(
          "the value's text is no number of its format");
    }
    return held(*std::move(bits));
  };
  const auto text = [&]() -> std::string_view {
    return value::CStringText(MemberOf<PLI_BYTE8*>(value.value));
  };
  switch (value.format) {
    case vpiBinStrVal:
      return from_text(
          DigitBits(text(), value::DigitRadix::kBinary, shape.width));
    case vpiOctStrVal:
      return from_text(
          DigitBits(text(), value::DigitRadix::kOctal, shape.width));
    case vpiHexStrVal:
      return from_text(DigitBits(text(), value::DigitRadix::kHex, shape.width));
    case vpiDecStrVal:
      return from_text(value::BitVec::OfDecimal(text(), shape.width));
    case vpiStringVal:
      return held(TextBits(text(), shape.width));
    case vpiScalarVal: {
      value::BitVec bits = value::BitVec::Zeros(shape.width);
      if (shape.width > 0 &&
          !SetScalar(bits, 0, MemberOf<PLI_INT32>(value.value))) {
        return std::unexpected<std::string>("the scalar value is no scalar");
      }
      return held(std::move(bits));
    }
    case vpiIntVal: {
      value::BitVec bits = value::BitVec::Zeros(shape.width);
      value::FromInt(bits.MutableView(), MemberOf<PLI_INT32>(value.value));
      return held(std::move(bits));
    }
    case vpiRealVal: {
      value::BitVec bits = value::BitVec::Zeros(shape.width);
      value::Real(MemberOf<double>(value.value)).RoundInto(bits.MutableView());
      return held(std::move(bits));
    }
    case vpiTimeVal: {
      const auto* time =
          static_cast<const s_vpi_time*>(MemberOf<void*>(value.value));
      if (time == nullptr) {
        return std::unexpected<std::string>("no time value was given");
      }
      value::BitVec bits = value::BitVec::Zeros(shape.width);
      value::FromInt(
          bits.MutableView(),
          static_cast<std::int64_t>(
              (static_cast<std::uint64_t>(time->high) << 32U) | time->low));
      return held(std::move(bits));
    }
    // The array foreign code hands holds one entry per 32 bits (LRM 38.15).
    case vpiVectorVal: {
      const auto* vector =
          static_cast<const s_vpi_vecval*>(MemberOf<void*>(value.value));
      if (vector == nullptr) {
        return std::unexpected<std::string>("no vector value was given");
      }
      const std::span<const s_vpi_vecval> chunks(
          vector, static_cast<std::size_t>((shape.width + 31) / 32));
      value::BitVec bits = value::BitVec::Zeros(shape.width);
      const value::IntegralView view = bits.MutableView();
      for (std::uint64_t at = 0; at < chunks.size(); ++at) {
        const s_vpi_vecval& chunk = chunks[at];
        view.value[(at * 32) / 64] |= static_cast<std::uint64_t>(chunk.aval)
                                      << ((at * 32) % 64);
        if (!view.unknown.empty()) {
          view.unknown[(at * 32) / 64] |= static_cast<std::uint64_t>(chunk.bval)
                                          << ((at * 32) % 64);
        }
      }
      value::ClearAboveWidth(view.value, view.width);
      value::ClearAboveWidth(view.unknown, view.width);
      return held(std::move(bits));
    }
    // A value with strength is put to a scalar only (LRM 38.34), as one entry.
    case vpiStrengthVal: {
      if (shape.width != 1) {
        return std::unexpected<std::string>(
            "a value with strength is put to no vector");
      }
      const auto* strength =
          static_cast<const s_vpi_strengthval*>(MemberOf<void*>(value.value));
      if (strength == nullptr) {
        return std::unexpected<std::string>("no strength value was given");
      }
      value::BitVec bits = value::BitVec::Zeros(1);
      if (!SetScalar(bits, 0, strength->logic)) {
        return std::unexpected<std::string>(
            "a strength value's logic is no scalar");
      }
      return held(std::move(bits));
    }
    default:
      return FormatDoesNotApply();
  }
}

}  // namespace

auto ReadValueStore() -> VpiValueStore& {
  static thread_local VpiValueStore store;
  return store;
}

auto FillValue(
    const ValueShape& shape, const HeldValue& held, s_vpi_value& value,
    VpiValueStore& store) -> VpiAnswer<void> {
  if (value.format == vpiObjTypeVal) value.format = shape.natural_format;
  const PLI_INT32 format = value.format;
  if (format == vpiSuppressVal) return {};
  return std::visit(
      Overloaded{
          [&](const value::BitVec& bits) {
            return FillIntegral(
                SignedView(bits, std::get<IntegralShape>(shape.held)), format,
                value, store);
          },
          [&](double real) { return FillReal(real, format, value); },
          [&](const value::String& text) {
            return FillText(text, format, value, store);
          }},
      held);
}

auto ValueToWrite(const ValueShape& shape, const s_vpi_value& value)
    -> VpiAnswer<HeldValue> {
  return std::visit(
      Overloaded{
          [&](const IntegralShape& integral) {
            return IntegralToWrite(integral, value);
          },
          [&](const RealShape&) -> VpiAnswer<HeldValue> {
            switch (value.format) {
              case vpiRealVal:
                return HeldValue{MemberOf<double>(value.value)};
              case vpiIntVal:
                return HeldValue{
                    static_cast<double>(MemberOf<PLI_INT32>(value.value))};
              default:
                return FormatDoesNotApply();
            }
          },
          [&](const TextShape&) -> VpiAnswer<HeldValue> {
            if (value.format != vpiStringVal) return FormatDoesNotApply();
            return HeldValue{value::String(MemberOf<PLI_BYTE8*>(value.value))};
          }},
      shape.held);
}

}  // namespace lyra::runtime
