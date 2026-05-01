#include "lyra/runtime/format.hpp"

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
#include "lyra/runtime/integral_format.hpp"
#include "lyra/runtime/packed_internal.hpp"

namespace lyra::runtime {

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

auto FormatValue(const FormatSpec& spec, const RuntimeValueView& value)
    -> std::string {
  return std::visit(
      Overloaded{
          [&](const IntegralValueView& v) -> std::string {
            return FormatIntegral(spec, v);
          },
          [&](const StringValueView& v) -> std::string {
            std::string body(v.data, v.size);
            return ApplyStringWidth(std::move(body), spec);
          },
      },
      value.data);
}

}  // namespace lyra::runtime
