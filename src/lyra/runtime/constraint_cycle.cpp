#include "lyra/runtime/constraint_cycle.hpp"

#include <cstdint>
#include <optional>

#include "lyra/runtime/rng.hpp"

namespace lyra::runtime {

namespace {

constexpr int kFeistelRounds = 6;

auto Mask(std::uint64_t width) -> std::uint64_t {
  return width >= 64 ? ~std::uint64_t{0} : (std::uint64_t{1} << width) - 1;
}

// A round's mixing of one half under the key (Stafford's variant 13 of the
// MurmurHash3 finalizer).
auto Round(std::uint64_t key, int round, std::uint64_t half) -> std::uint64_t {
  std::uint64_t value =
      half ^ key ^ (static_cast<std::uint64_t>(round) * 0x9e3779b97f4a7c15ULL);
  value = (value ^ (value >> 30U)) * 0xbf58476d1ce4e5b9ULL;
  value = (value ^ (value >> 27U)) * 0x94d049bb133111ebULL;
  return value ^ (value >> 31U);
}

}  // namespace

ConstraintCycle::ConstraintCycle(
    std::uint64_t width, std::optional<std::uint64_t> count)
    : width_(width), count_(count) {
}

auto ConstraintCycle::Size() const -> std::uint64_t {
  if (count_.has_value()) {
    return *count_;
  }
  return width_ >= 64 ? ~std::uint64_t{0} : std::uint64_t{1} << width_;
}

auto ConstraintCycle::ValueAt(std::uint64_t position) const -> std::uint64_t {
  std::uint64_t value = Permute(position);
  while (count_.has_value() && value >= *count_) {
    value = Permute(value);
  }
  return value;
}

auto ConstraintCycle::Permute(std::uint64_t value) const -> std::uint64_t {
  if (width_ <= 1) {
    return (value ^ key_) & Mask(width_);
  }
  // An unbalanced Feistel network on a value of `width_` bits: each round
  // replaces one half with itself mixed with the other, which is invertible
  // whatever the mixing does, so the whole is a permutation of the width.
  const std::uint64_t low_bits = width_ / 2;
  const std::uint64_t high_bits = width_ - low_bits;
  std::uint64_t low = value & Mask(low_bits);
  std::uint64_t high = (value >> low_bits) & Mask(high_bits);
  for (int round = 0; round < kFeistelRounds; ++round) {
    if (round % 2 == 0) {
      high = (high ^ Round(key_, round, low)) & Mask(high_bits);
    } else {
      low = (low ^ Round(key_, round, high)) & Mask(low_bits);
    }
  }
  return (high << low_bits) | low;
}

void ConstraintCycle::Restart(DrawRng& rng) {
  key_ = (std::uint64_t{rng.NextValue()} << 32U) | rng.NextValue();
  cursor_ = 0;
  taken_.clear();
}

void ConstraintCycle::ConstrainedBy(std::uint64_t signature, DrawRng& rng) {
  if (constrained_by_ != signature) {
    Restart(rng);
    constrained_by_ = signature;
  }
}

void ConstraintCycle::Take(std::uint64_t value, DrawRng& rng) {
  taken_.insert(value);
  while (cursor_ < Size() && taken_.contains(ValueAt(cursor_))) {
    ++cursor_;
  }
  if (cursor_ >= Size()) {
    Restart(rng);
  }
}

}  // namespace lyra::runtime
