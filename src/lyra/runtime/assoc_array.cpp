#include "lyra/runtime/assoc_array.hpp"

#include <algorithm>
#include <cstdint>
#include <format>
#include <memory>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/runtime/assoc_array_heap.hpp"
#include "lyra/semantic/value.hpp"

namespace lyra::runtime {

using semantic::AsIntegral;
using semantic::AsString;
using semantic::Clone;
using semantic::IsIntegral;
using semantic::IsString;
using semantic::MakeIntegralWide;
using semantic::MakeString;
using semantic::RuntimeValue;

// --- CanonKeyLess ---

auto CanonKeyLess::operator()(
    const CanonKeyPayload& a, const CanonKeyPayload& b) const -> bool {
  if (spec.kind == KeySpec::Kind::kString) {
    return a.str_value < b.str_value;
  }

  // Integral comparison: MSW-to-LSW
  if (spec.is_signed) {
    // Sign-aware two's complement comparison
    size_t n = a.int_words.size();
    if (n == 0) return false;

    // Check sign bits of MSW
    uint64_t a_msw = a.int_words[n - 1];
    uint64_t b_msw = b.int_words[n - 1];
    uint32_t top_bits = spec.bit_width % 64;
    if (top_bits == 0) top_bits = 64;
    uint64_t sign_mask = 1ULL << (top_bits - 1);
    bool a_neg = (a_msw & sign_mask) != 0;
    bool b_neg = (b_msw & sign_mask) != 0;
    if (a_neg != b_neg) return a_neg;  // negative < positive

    // Same sign: unsigned MSW-to-LSW
    for (size_t i = n; i > 0; --i) {
      if (a.int_words[i - 1] != b.int_words[i - 1]) {
        return a.int_words[i - 1] < b.int_words[i - 1];
      }
    }
    return false;
  }

  // Unsigned: MSW-to-LSW
  size_t n = a.int_words.size();
  for (size_t i = n; i > 0; --i) {
    if (a.int_words[i - 1] != b.int_words[i - 1]) {
      return a.int_words[i - 1] < b.int_words[i - 1];
    }
  }
  return false;
}

// --- Canonicalization ---

auto CanonicalizeKey(const RuntimeValue& value, const KeySpec& spec)
    -> CanonResult {
  if (spec.kind == KeySpec::Kind::kString) {
    if (!IsString(value)) {
      throw common::InternalError(
          "CanonicalizeKey", "expected string value for string key");
    }
    return CanonResult{
        .status = KeyStatus::kOk,
        .payload = CanonKeyPayload{.str_value = AsString(value).value}};
  }

  // Integral key
  if (!IsIntegral(value)) {
    throw common::InternalError(
        "CanonicalizeKey", "expected integral value for integral key");
  }
  const auto& integral = AsIntegral(value);

  // Check for X/Z
  for (uint64_t unk : integral.unknown) {
    if (unk != 0) {
      return CanonResult{.status = KeyStatus::kHasXZ, .payload = {}};
    }
  }

  // Canonicalize to spec.bit_width: sign-extend if signed, then mask top word.
  uint32_t num_words = (spec.bit_width + 63) / 64;
  std::vector<uint64_t> words(num_words, 0);
  size_t src_words = integral.value.size();
  for (size_t i = 0; i < std::min<size_t>(src_words, num_words); ++i) {
    words[i] = integral.value[i];
  }

  // Sign-extend if the source value is narrower than the key type.
  if (spec.is_signed && src_words > 0 && src_words <= num_words) {
    uint32_t src_bits = integral.bit_width;
    if (src_bits < spec.bit_width) {
      uint32_t src_top_bits = src_bits % 64;
      if (src_top_bits == 0) src_top_bits = 64;
      uint64_t src_msw = integral.value[src_words - 1];
      bool negative = (src_msw & (1ULL << (src_top_bits - 1))) != 0;
      if (negative) {
        // Fill remaining bits in the source top word
        if (src_top_bits < 64) {
          words[src_words - 1] |= ~((1ULL << src_top_bits) - 1);
        }
        // Fill remaining words above the source
        for (size_t i = src_words; i < num_words; ++i) {
          words[i] = ~uint64_t{0};
        }
      }
    }
  }

  // Mask unused bits in top word to ensure canonical form
  uint32_t top_bits = spec.bit_width % 64;
  if (top_bits != 0 && !words.empty()) {
    words.back() &= (1ULL << top_bits) - 1;
  }

  return CanonResult{
      .status = KeyStatus::kOk,
      .payload = CanonKeyPayload{.int_words = std::move(words)}};
}

auto KeyPayloadToRuntimeValue(
    const CanonKeyPayload& payload, const KeySpec& spec) -> RuntimeValue {
  if (spec.kind == KeySpec::Kind::kString) {
    return MakeString(payload.str_value);
  }
  return MakeIntegralWide(
      payload.int_words.data(), payload.int_words.size(), spec.bit_width);
}

auto MakeKeySpec(TypeId key_type, const TypeArena& types) -> KeySpec {
  if (!key_type) {
    // Wildcard [*] -> int (32-bit signed)
    return KeySpec{
        .kind = KeySpec::Kind::kIntegral, .bit_width = 32, .is_signed = true};
  }
  const auto& type = types[key_type];
  if (type.Kind() == TypeKind::kString) {
    return KeySpec{.kind = KeySpec::Kind::kString};
  }
  if (type.Kind() == TypeKind::kEnum) {
    const auto& info = type.AsEnum();
    const auto& base = types[info.base_type];
    const auto& integral = base.AsIntegral();
    return KeySpec{
        .kind = KeySpec::Kind::kIntegral,
        .bit_width = integral.bit_width,
        .is_signed = integral.is_signed,
        .enum_type = key_type};
  }
  if (type.Kind() == TypeKind::kIntegral) {
    const auto& integral = type.AsIntegral();
    return KeySpec{
        .kind = KeySpec::Kind::kIntegral,
        .bit_width = integral.bit_width,
        .is_signed = integral.is_signed};
  }
  throw common::InternalError(
      "MakeKeySpec",
      std::format("unsupported key type kind: {}", ToString(type.Kind())));
}

// --- AssocArrayObj ---

AssocArrayObj::AssocArrayObj(KeySpec spec)
    : key_spec_(std::move(spec)), entries_(CanonKeyLess{.spec = key_spec_}) {
}

auto AssocArrayObj::Find(const CanonKeyPayload& key) const
    -> const RuntimeValue* {
  auto it = entries_.find(key);
  if (it == entries_.end()) return nullptr;
  return &it->second;
}

auto AssocArrayObj::FindMut(const CanonKeyPayload& key) -> RuntimeValue* {
  auto it = entries_.find(key);
  if (it == entries_.end()) return nullptr;
  return &it->second;
}

void AssocArrayObj::Set(CanonKeyPayload key, RuntimeValue value) {
  entries_.insert_or_assign(std::move(key), std::move(value));
}

auto AssocArrayObj::Exists(const CanonKeyPayload& key) const -> bool {
  return entries_.contains(key);
}

void AssocArrayObj::Delete() {
  entries_.clear();
}

void AssocArrayObj::DeleteKey(const CanonKeyPayload& key) {
  entries_.erase(key);
}

auto AssocArrayObj::Num() const -> int64_t {
  return static_cast<int64_t>(entries_.size());
}

auto AssocArrayObj::First() const -> std::optional<CanonKeyPayload> {
  if (entries_.empty()) return std::nullopt;
  return entries_.begin()->first;
}

auto AssocArrayObj::Last() const -> std::optional<CanonKeyPayload> {
  if (entries_.empty()) return std::nullopt;
  return entries_.rbegin()->first;
}

auto AssocArrayObj::Next(const CanonKeyPayload& current) const
    -> std::optional<CanonKeyPayload> {
  auto it = entries_.upper_bound(current);
  if (it == entries_.end()) return std::nullopt;
  return it->first;
}

auto AssocArrayObj::Prev(const CanonKeyPayload& current) const
    -> std::optional<CanonKeyPayload> {
  auto it = entries_.lower_bound(current);
  if (it == entries_.begin()) return std::nullopt;
  --it;
  return it->first;
}

auto AssocArrayObj::SnapshotKeys() const -> std::vector<CanonKeyPayload> {
  std::vector<CanonKeyPayload> keys;
  keys.reserve(entries_.size());
  for (const auto& [key, _] : entries_) {
    keys.push_back(key);
  }
  return keys;
}

auto AssocArrayObj::Clone() const -> std::unique_ptr<AssocArrayObj> {
  auto copy = std::make_unique<AssocArrayObj>(key_spec_);
  for (const auto& [key, value] : entries_) {
    copy->entries_.emplace(key, semantic::Clone(value));
  }
  return copy;
}

// --- AssocArrayHeap ---

AssocArrayHeap::AssocArrayHeap() = default;
AssocArrayHeap::~AssocArrayHeap() = default;
AssocArrayHeap::AssocArrayHeap(AssocArrayHeap&&) noexcept = default;
auto AssocArrayHeap::operator=(AssocArrayHeap&&) noexcept
    -> AssocArrayHeap& = default;

auto AssocArrayHeap::Allocate(KeySpec spec) -> AssocArrayHandle {
  auto id = static_cast<uint32_t>(objects_.size());
  objects_.push_back(std::make_unique<AssocArrayObj>(std::move(spec)));
  return AssocArrayHandle{id};
}

auto AssocArrayHeap::Get(AssocArrayHandle h) -> AssocArrayObj& {
  return *objects_.at(h.id);
}

auto AssocArrayHeap::Get(AssocArrayHandle h) const -> const AssocArrayObj& {
  return *objects_.at(h.id);
}

auto AssocArrayHeap::DeepCopy(AssocArrayHandle src) -> AssocArrayHandle {
  auto id = static_cast<uint32_t>(objects_.size());
  objects_.push_back(Get(src).Clone());
  return AssocArrayHandle{id};
}

}  // namespace lyra::runtime
