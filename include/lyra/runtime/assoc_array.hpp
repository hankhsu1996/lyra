#pragma once

#include <cstdint>
#include <map>
#include <memory>
#include <optional>
#include <string>
#include <vector>

#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/semantic/value.hpp"

namespace lyra::runtime {

struct KeySpec {
  enum class Kind : uint8_t { kIntegral, kString };
  Kind kind;
  uint32_t bit_width = 0;
  bool is_signed = false;
  TypeId enum_type = kInvalidTypeId;
};

struct CanonKeyPayload {
  std::vector<uint64_t> int_words;
  std::string str_value;
  auto operator==(const CanonKeyPayload&) const -> bool = default;
};

struct CanonKeyLess {
  KeySpec spec;
  auto operator()(const CanonKeyPayload& a, const CanonKeyPayload& b) const
      -> bool;
};

enum class KeyStatus { kOk, kHasXZ };

struct CanonResult {
  KeyStatus status;
  CanonKeyPayload payload;
};

// Raw canonicalization: value/xz words in LSW-first order.
// Both MIR (via CanonicalizeKey wrapper) and LLVM (via store-to-alloca) use
// this. xz_words may be null (2-state key).
auto CanonicalizeKeyRaw(
    const uint64_t* value_words, const uint64_t* xz_words, uint32_t num_words,
    const KeySpec& spec) -> CanonResult;

// String key canonicalization.
auto CanonicalizeKeyString(const char* str_data, uint32_t str_len)
    -> CanonResult;

// RuntimeValue wrapper: extracts words and delegates to CanonicalizeKeyRaw.
auto CanonicalizeKey(const semantic::RuntimeValue& value, const KeySpec& spec)
    -> CanonResult;

// Write canonical key payload into raw words (for LLVM iteration output).
// out_words must have capacity for at least ceil(spec.bit_width/64) words.
void KeyPayloadToRaw(
    const CanonKeyPayload& payload, const KeySpec& spec, uint64_t* out_words,
    uint32_t num_words);

auto KeyPayloadToRuntimeValue(
    const CanonKeyPayload& payload, const KeySpec& spec)
    -> semantic::RuntimeValue;
auto MakeKeySpec(TypeId key_type, const TypeArena& types) -> KeySpec;

class AssocArrayObj {
 public:
  using Map = std::map<CanonKeyPayload, semantic::RuntimeValue, CanonKeyLess>;

  explicit AssocArrayObj(KeySpec spec);

  [[nodiscard]] auto GetKeySpec() const -> const KeySpec& {
    return key_spec_;
  }

  auto Find(const CanonKeyPayload& key) const -> const semantic::RuntimeValue*;
  auto FindMut(const CanonKeyPayload& key) -> semantic::RuntimeValue*;

  void Set(CanonKeyPayload key, semantic::RuntimeValue value);

  [[nodiscard]] auto Exists(const CanonKeyPayload& key) const -> bool;
  void Delete();
  void DeleteKey(const CanonKeyPayload& key);
  [[nodiscard]] auto Num() const -> int64_t;

  [[nodiscard]] auto First() const -> std::optional<CanonKeyPayload>;
  [[nodiscard]] auto Last() const -> std::optional<CanonKeyPayload>;
  [[nodiscard]] auto Next(const CanonKeyPayload& current) const
      -> std::optional<CanonKeyPayload>;
  [[nodiscard]] auto Prev(const CanonKeyPayload& current) const
      -> std::optional<CanonKeyPayload>;

  [[nodiscard]] auto SnapshotKeys() const -> std::vector<CanonKeyPayload>;

  [[nodiscard]] auto Clone() const -> std::unique_ptr<AssocArrayObj>;

 private:
  KeySpec key_spec_;
  Map entries_;
};

}  // namespace lyra::runtime
