#include "lyra/value/packed_bitwise.hpp"

#include <cstddef>
#include <cstdint>
#include <string_view>

#include "lyra/value/packed.hpp"
#include "lyra/value/packed_internal.hpp"

namespace lyra::value {

auto BitwiseNot(ConstBitView src, BitView dst) -> void {
  constexpr std::string_view kWhere = "BitwiseNot(Bit)";
  detail::RequireSameWidth(kWhere, src.Width(), dst.Width());
  const auto src_words = src.ValueWords();
  const auto dst_words = dst.ValueWords();
  for (std::size_t i = 0; i < dst_words.size(); ++i) {
    dst_words[i] = ~src_words[i];
  }
  MaskUnusedTopBits(dst_words, dst.Width());
}

auto BitwiseNot(ConstLogicView src, LogicView dst) -> void {
  constexpr std::string_view kWhere = "BitwiseNot(Logic)";
  detail::RequireSameWidth(kWhere, src.Width(), dst.Width());
  const auto src_v = src.ValueWords();
  const auto src_u = src.UnknownWords();
  const auto dst_v = dst.ValueWords();
  const auto dst_u = dst.UnknownWords();
  for (std::size_t i = 0; i < dst_v.size(); ++i) {
    dst_u[i] = src_u[i];
    dst_v[i] = (~src_v[i]) | src_u[i];
  }
  MaskUnusedTopBits(dst_v, dst.Width());
  MaskUnusedTopBits(dst_u, dst.Width());
}

auto BitwiseAnd(ConstBitView lhs, ConstBitView rhs, BitView dst) -> void {
  constexpr std::string_view kWhere = "BitwiseAnd(Bit)";
  detail::RequireSameWidth(kWhere, lhs.Width(), rhs.Width(), dst.Width());
  const auto lhs_words = lhs.ValueWords();
  const auto rhs_words = rhs.ValueWords();
  const auto dst_words = dst.ValueWords();
  for (std::size_t i = 0; i < dst_words.size(); ++i) {
    dst_words[i] = lhs_words[i] & rhs_words[i];
  }
  MaskUnusedTopBits(dst_words, dst.Width());
}

auto BitwiseAnd(ConstLogicView lhs, ConstLogicView rhs, LogicView dst) -> void {
  constexpr std::string_view kWhere = "BitwiseAnd(Logic)";
  detail::RequireSameWidth(kWhere, lhs.Width(), rhs.Width(), dst.Width());
  const auto lhs_v = lhs.ValueWords();
  const auto lhs_u = lhs.UnknownWords();
  const auto rhs_v = rhs.ValueWords();
  const auto rhs_u = rhs.UnknownWords();
  const auto dst_v = dst.ValueWords();
  const auto dst_u = dst.UnknownWords();
  for (std::size_t i = 0; i < dst_v.size(); ++i) {
    const std::uint64_t av = lhs_v[i];
    const std::uint64_t au = lhs_u[i];
    const std::uint64_t bv = rhs_v[i];
    const std::uint64_t bu = rhs_u[i];
    const std::uint64_t a_zero = ~au & ~av;
    const std::uint64_t b_zero = ~bu & ~bv;
    const std::uint64_t a_one = ~au & av;
    const std::uint64_t b_one = ~bu & bv;
    const std::uint64_t is_zero = a_zero | b_zero;
    const std::uint64_t is_one = a_one & b_one;
    const std::uint64_t unknown = ~(is_zero | is_one);
    dst_v[i] = is_one | unknown;
    dst_u[i] = unknown;
  }
  MaskUnusedTopBits(dst_v, dst.Width());
  MaskUnusedTopBits(dst_u, dst.Width());
}

auto BitwiseOr(ConstBitView lhs, ConstBitView rhs, BitView dst) -> void {
  constexpr std::string_view kWhere = "BitwiseOr(Bit)";
  detail::RequireSameWidth(kWhere, lhs.Width(), rhs.Width(), dst.Width());
  const auto lhs_words = lhs.ValueWords();
  const auto rhs_words = rhs.ValueWords();
  const auto dst_words = dst.ValueWords();
  for (std::size_t i = 0; i < dst_words.size(); ++i) {
    dst_words[i] = lhs_words[i] | rhs_words[i];
  }
  MaskUnusedTopBits(dst_words, dst.Width());
}

auto BitwiseOr(ConstLogicView lhs, ConstLogicView rhs, LogicView dst) -> void {
  constexpr std::string_view kWhere = "BitwiseOr(Logic)";
  detail::RequireSameWidth(kWhere, lhs.Width(), rhs.Width(), dst.Width());
  const auto lhs_v = lhs.ValueWords();
  const auto lhs_u = lhs.UnknownWords();
  const auto rhs_v = rhs.ValueWords();
  const auto rhs_u = rhs.UnknownWords();
  const auto dst_v = dst.ValueWords();
  const auto dst_u = dst.UnknownWords();
  for (std::size_t i = 0; i < dst_v.size(); ++i) {
    const std::uint64_t av = lhs_v[i];
    const std::uint64_t au = lhs_u[i];
    const std::uint64_t bv = rhs_v[i];
    const std::uint64_t bu = rhs_u[i];
    const std::uint64_t a_zero = ~au & ~av;
    const std::uint64_t b_zero = ~bu & ~bv;
    const std::uint64_t a_one = ~au & av;
    const std::uint64_t b_one = ~bu & bv;
    const std::uint64_t is_one = a_one | b_one;
    const std::uint64_t is_zero = a_zero & b_zero;
    const std::uint64_t unknown = ~(is_zero | is_one);
    dst_v[i] = is_one | unknown;
    dst_u[i] = unknown;
  }
  MaskUnusedTopBits(dst_v, dst.Width());
  MaskUnusedTopBits(dst_u, dst.Width());
}

auto BitwiseXor(ConstBitView lhs, ConstBitView rhs, BitView dst) -> void {
  constexpr std::string_view kWhere = "BitwiseXor(Bit)";
  detail::RequireSameWidth(kWhere, lhs.Width(), rhs.Width(), dst.Width());
  const auto lhs_words = lhs.ValueWords();
  const auto rhs_words = rhs.ValueWords();
  const auto dst_words = dst.ValueWords();
  for (std::size_t i = 0; i < dst_words.size(); ++i) {
    dst_words[i] = lhs_words[i] ^ rhs_words[i];
  }
  MaskUnusedTopBits(dst_words, dst.Width());
}

auto BitwiseXor(ConstLogicView lhs, ConstLogicView rhs, LogicView dst) -> void {
  constexpr std::string_view kWhere = "BitwiseXor(Logic)";
  detail::RequireSameWidth(kWhere, lhs.Width(), rhs.Width(), dst.Width());
  const auto lhs_v = lhs.ValueWords();
  const auto lhs_u = lhs.UnknownWords();
  const auto rhs_v = rhs.ValueWords();
  const auto rhs_u = rhs.UnknownWords();
  const auto dst_v = dst.ValueWords();
  const auto dst_u = dst.UnknownWords();
  for (std::size_t i = 0; i < dst_v.size(); ++i) {
    const std::uint64_t unknown = lhs_u[i] | rhs_u[i];
    dst_v[i] = unknown | (lhs_v[i] ^ rhs_v[i]);
    dst_u[i] = unknown;
  }
  MaskUnusedTopBits(dst_v, dst.Width());
  MaskUnusedTopBits(dst_u, dst.Width());
}

auto BitwiseXnor(ConstBitView lhs, ConstBitView rhs, BitView dst) -> void {
  constexpr std::string_view kWhere = "BitwiseXnor(Bit)";
  detail::RequireSameWidth(kWhere, lhs.Width(), rhs.Width(), dst.Width());
  const auto lhs_words = lhs.ValueWords();
  const auto rhs_words = rhs.ValueWords();
  const auto dst_words = dst.ValueWords();
  for (std::size_t i = 0; i < dst_words.size(); ++i) {
    dst_words[i] = ~(lhs_words[i] ^ rhs_words[i]);
  }
  MaskUnusedTopBits(dst_words, dst.Width());
}

auto BitwiseXnor(ConstLogicView lhs, ConstLogicView rhs, LogicView dst)
    -> void {
  constexpr std::string_view kWhere = "BitwiseXnor(Logic)";
  detail::RequireSameWidth(kWhere, lhs.Width(), rhs.Width(), dst.Width());
  const auto lhs_v = lhs.ValueWords();
  const auto lhs_u = lhs.UnknownWords();
  const auto rhs_v = rhs.ValueWords();
  const auto rhs_u = rhs.UnknownWords();
  const auto dst_v = dst.ValueWords();
  const auto dst_u = dst.UnknownWords();
  for (std::size_t i = 0; i < dst_v.size(); ++i) {
    const std::uint64_t unknown = lhs_u[i] | rhs_u[i];
    dst_v[i] = unknown | ~(lhs_v[i] ^ rhs_v[i]);
    dst_u[i] = unknown;
  }
  MaskUnusedTopBits(dst_v, dst.Width());
  MaskUnusedTopBits(dst_u, dst.Width());
}

}  // namespace lyra::value
