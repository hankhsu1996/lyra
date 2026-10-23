#include "lyra/runtime/natural.hpp"

#include <algorithm>
#include <bit>
#include <compare>
#include <cstddef>
#include <cstdint>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/rng.hpp"

namespace lyra::runtime {

namespace {

constexpr std::uint32_t kWordBits = 64;

}  // namespace

Natural::Natural(std::uint64_t value) {
  if (value != 0) {
    words_.push_back(value);
  }
}

auto Natural::FromBits(const std::vector<bool>& bits) -> Natural {
  Natural out;
  out.words_.assign((bits.size() + kWordBits - 1) / kWordBits, 0);
  for (std::size_t i = 0; i < bits.size(); ++i) {
    if (bits[i]) {
      out.words_[i / kWordBits] |= std::uint64_t{1} << (i % kWordBits);
    }
  }
  out.Trim();
  return out;
}

void Natural::Trim() {
  while (!words_.empty() && words_.back() == 0) {
    words_.pop_back();
  }
}

auto Natural::BitLength() const -> std::uint32_t {
  if (words_.empty()) {
    return 0;
  }
  return static_cast<std::uint32_t>(
      (words_.size() * kWordBits) -
      static_cast<std::size_t>(std::countl_zero(words_.back())));
}

auto Natural::ShiftedLeft(std::uint32_t shift) const -> Natural {
  if (words_.empty()) {
    return *this;
  }
  const std::uint32_t bits = shift % kWordBits;
  Natural out;
  out.words_.assign(shift / kWordBits, 0);
  std::uint64_t carry = 0;
  for (const std::uint64_t word : words_) {
    out.words_.push_back((word << bits) | carry);
    carry = bits == 0 ? 0 : word >> (kWordBits - bits);
  }
  out.words_.push_back(carry);
  out.Trim();
  return out;
}

auto Natural::TakeLowBits(std::uint32_t count) -> std::vector<bool> {
  std::vector<bool> out(count);
  for (std::uint32_t i = 0; i < count; ++i) {
    const std::size_t word = i / kWordBits;
    out[i] =
        word < words_.size() && ((words_[word] >> (i % kWordBits)) & 1U) != 0;
  }
  const std::uint32_t skipped = count / kWordBits;
  const std::uint32_t bits = count % kWordBits;
  std::vector<std::uint64_t> rest;
  for (std::size_t i = skipped; i < words_.size(); ++i) {
    const std::uint64_t above = i + 1 < words_.size() ? words_[i + 1] : 0;
    rest.push_back(
        bits == 0 ? words_[i]
                  : (words_[i] >> bits) | (above << (kWordBits - bits)));
  }
  words_ = std::move(rest);
  Trim();
  return out;
}

auto operator+(const Natural& a, const Natural& b) -> Natural {
  Natural out;
  out.words_.assign(std::max(a.words_.size(), b.words_.size()) + 1, 0);
  std::uint64_t carry = 0;
  for (std::size_t i = 0; i + 1 < out.words_.size(); ++i) {
    const std::uint64_t x = i < a.words_.size() ? a.words_[i] : 0;
    const std::uint64_t y = i < b.words_.size() ? b.words_[i] : 0;
    const std::uint64_t partial = x + y;
    const std::uint64_t total = partial + carry;
    carry = (partial < x ? 1U : 0U) + (total < partial ? 1U : 0U);
    out.words_[i] = total;
  }
  out.words_.back() = carry;
  out.Trim();
  return out;
}

auto operator-(const Natural& a, const Natural& b) -> Natural {
  if (a < b) {
    throw InternalError(
        "Natural: a subtraction would go below zero -- please report this as "
        "a bug");
  }
  Natural out = a;
  std::uint64_t borrow = 0;
  for (std::size_t i = 0; i < out.words_.size(); ++i) {
    const std::uint64_t x = out.words_[i];
    const std::uint64_t y = i < b.words_.size() ? b.words_[i] : 0;
    const std::uint64_t partial = x - y;
    out.words_[i] = partial - borrow;
    borrow = (x < y ? 1U : 0U) + (partial < borrow ? 1U : 0U);
  }
  out.Trim();
  return out;
}

auto operator*(const Natural& a, const Natural& b) -> Natural {
  // Long multiplication over half words, whose products fit a word.
  const auto halves = [](const Natural& n) {
    std::vector<std::uint64_t> out;
    for (const std::uint64_t word : n.words_) {
      out.push_back(word & 0xFFFFFFFFU);
      out.push_back(word >> 32U);
    }
    return out;
  };
  const std::vector<std::uint64_t> x = halves(a);
  const std::vector<std::uint64_t> y = halves(b);
  std::vector<std::uint64_t> product(x.size() + y.size() + 1, 0);
  for (std::size_t i = 0; i < x.size(); ++i) {
    std::uint64_t carry = 0;
    for (std::size_t j = 0; j < y.size(); ++j) {
      const std::uint64_t t = (x[i] * y[j]) + product[i + j] + carry;
      product[i + j] = t & 0xFFFFFFFFU;
      carry = t >> 32U;
    }
    product[i + y.size()] += carry;
  }
  Natural out;
  for (std::size_t i = 0; i + 1 < product.size(); i += 2) {
    out.words_.push_back(product[i] | (product[i + 1] << 32U));
  }
  out.Trim();
  return out;
}

auto operator<=>(const Natural& a, const Natural& b) -> std::strong_ordering {
  if (a.words_.size() != b.words_.size()) {
    return a.words_.size() <=> b.words_.size();
  }
  for (std::size_t i = a.words_.size(); i-- > 0;) {
    if (a.words_[i] != b.words_[i]) {
      return a.words_[i] <=> b.words_[i];
    }
  }
  return std::strong_ordering::equal;
}

auto Natural::DrawBelow(DrawRng& rng, const Natural& bound) -> Natural {
  if (bound.IsZero()) {
    throw InternalError(
        "Natural: a draw below zero -- please report this as a bug");
  }
  // As many random bits as the bound is long, drawn again while they reach
  // it, so each round is accepted with probability above a half.
  const std::uint32_t length = bound.BitLength();
  while (true) {
    Natural n;
    n.words_.assign((length + kWordBits - 1) / kWordBits, 0);
    for (std::uint32_t i = 0; i < length; i += 32) {
      n.words_[i / kWordBits] |= std::uint64_t{rng.NextValue()}
                                 << (i % kWordBits);
    }
    if (length % kWordBits != 0) {
      n.words_.back() &= (std::uint64_t{1} << (length % kWordBits)) - 1;
    }
    n.Trim();
    if (n < bound) {
      return n;
    }
  }
}

}  // namespace lyra::runtime
