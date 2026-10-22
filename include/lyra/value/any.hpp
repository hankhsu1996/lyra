#pragma once

#include <string_view>
#include <utility>

#include "lyra/value/any_value.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::value {

// A value of any type, kept with that type and with the identity it is tested
// by when it is asked to become a value of another: what a message of a
// mailbox of no stated type is (LRM 15.4.9), as C++'s `std::any` and Swift's
// `Any` are. Two types share an identity exactly where a value of one may be
// taken as a value of the other (LRM 6.22.2), and whoever makes a value states
// its identity as text, so a value made where one unit compiles is tested
// where another does with nothing shared between the two.
class Any {
 public:
  // Holds no value: a message before one is put.
  Any() = default;

  // A copy of the value of `type` lying at `value`, of the identity `identity`
  // names, which outlives every value made with it.
  [[nodiscard]] static auto CopyOf(
      const ValueType& type, const void* value, const char* identity) -> Any {
    return {AnyValue::CopyOf(type, value), identity};
  }

  [[nodiscard]] auto Identity() const -> std::string_view {
    return identity_ == nullptr ? std::string_view{} : identity_;
  }
  // Where the value lies, and nothing where none is held.
  [[nodiscard]] auto HeldValue() const -> const void* {
    return held_.Bytes();
  }

  // Two values are equal where they are of one identity and their values are
  // equal; a value of another identity is a different value, as values of two
  // types the language keeps apart are never equal.
  [[nodiscard]] auto operator==(const Any& other) const -> FourStateBit;
  [[nodiscard]] auto operator!=(const Any& other) const -> FourStateBit;
  [[nodiscard]] auto IsBitIdentical(const Any& other) const -> bool;
  [[nodiscard]] auto HasUnknown() const -> bool;

 private:
  Any(AnyValue held, const char* identity)
      : held_(std::move(held)), identity_(identity) {
  }

  AnyValue held_;
  const char* identity_ = nullptr;
};

static_assert(LyraValue<Any>);

}  // namespace lyra::value
