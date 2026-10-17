#pragma once

#include <cstdint>
#include <utility>

#include "lyra/value/any_value.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::value {

// A value laid out by a type the design is compiled with, as the runtime keeps
// one of its own. The execution backend's library is compiled once, so it
// cannot be instantiated for each such type the way the C++ backend's library
// types are; it holds every value of one through this one shape instead: the
// value exactly as the program lays it out, in storage of its own, held with
// its type as any value the library was compiled without is. The bytes carry
// no type, so whoever hands a value in by its bytes states the type beside
// them, and every operation on the whole value is the type's, whichever kind
// of value -- a structure, a union, a tuple, a fixed-size unpacked array --
// the type is.
class LaidOutValue {
 public:
  // Holds no value: storage before whatever declares it installs one.
  LaidOutValue() = default;

  // The value `value` holds, which is a value of a type laid out by the
  // design.
  explicit LaidOutValue(AnyValue value) : value_(std::move(value)) {
  }

  // A copy of the value of `type` laid out at `laid_out`.
  [[nodiscard]] static auto CopyOf(const ValueType& type, const void* laid_out)
      -> LaidOutValue {
    return LaidOutValue(AnyValue::CopyOf(type, laid_out));
  }

  // The value's bytes, which is what crosses as its handle, and its type.
  [[nodiscard]] auto Bytes() const -> const void* {
    return value_.Bytes();
  }
  [[nodiscard]] auto Bytes() -> void* {
    return value_.Bytes();
  }
  [[nodiscard]] auto Type() const -> const ValueType& {
    return value_.Type();
  }

  // The value of this one's type laid out at `bytes`, compared with and taken
  // into what this holds, so a store of one obtains no storage for it. The
  // bytes may be this value's own.
  [[nodiscard]] auto IsBitIdenticalTo(const void* bytes) const -> bool {
    return Type().BitIdentical(Bytes(), bytes);
  }
  void AssignBytes(const void* bytes) {
    Type().Assign(Bytes(), bytes);
  }

  // A copy of the value, or the value itself, laid out in `out`.
  auto CopyInto(void* out) const -> void* {
    Type().Copy(Bytes(), out);
    return out;
  }
  auto MoveInto(void* out) && -> void* {
    Type().Move(Bytes(), out);
    value_ = AnyValue();
    return out;
  }

  // The value given up as the value held with its type that this keeps it as,
  // which leaves this holding none.
  [[nodiscard]] auto IntoAnyValue() && -> AnyValue {
    return std::exchange(value_, AnyValue());
  }

  [[nodiscard]] auto operator==(const LaidOutValue& other) const
      -> FourStateBit {
    return value_ == Held(other);
  }
  [[nodiscard]] auto operator!=(const LaidOutValue& other) const
      -> FourStateBit {
    return value_ != Held(other);
  }
  [[nodiscard]] auto CaseEqual(const LaidOutValue& other) const -> Bit {
    return value_.CaseEqual(Held(other));
  }
  [[nodiscard]] auto ResolveTriState(const LaidOutValue& other) const
      -> LaidOutValue {
    return LaidOutValue(value_.ResolveTriState(Held(other)));
  }
  [[nodiscard]] auto ResolveWiredAnd(const LaidOutValue& other) const
      -> LaidOutValue {
    return LaidOutValue(value_.ResolveWiredAnd(Held(other)));
  }
  [[nodiscard]] auto ResolveWiredOr(const LaidOutValue& other) const
      -> LaidOutValue {
    return LaidOutValue(value_.ResolveWiredOr(Held(other)));
  }
  [[nodiscard]] auto Dominating(const LaidOutValue& weaker) const
      -> LaidOutValue {
    return LaidOutValue(value_.Dominating(Held(weaker)));
  }
  [[nodiscard]] static auto FilledLike(
      const LaidOutValue& prototype, const Logic& fill) -> LaidOutValue {
    return LaidOutValue(AnyValue::FilledLike(Held(prototype), fill));
  }
  [[nodiscard]] auto IsBitIdentical(const LaidOutValue& other) const -> bool {
    return value_.IsBitIdentical(Held(other));
  }
  [[nodiscard]] auto HasUnknown() const -> bool {
    return value_.HasUnknown();
  }
  [[nodiscard]] auto IsUnknown() const -> Bit {
    return value_.IsUnknown();
  }
  [[nodiscard]] auto BitstreamWidth() const -> Int {
    return value_.BitstreamWidth();
  }
  [[nodiscard]] auto CountBits(const ConstIntegralView& control_bits) const
      -> Int {
    return value_.CountBits(control_bits);
  }
  [[nodiscard]] auto WriteToStream(
      const IntegralView& stream, std::uint64_t filled) const -> std::uint64_t {
    return value_.WriteToStream(stream, filled);
  }

 private:
  [[nodiscard]] static auto Held(const LaidOutValue& other) -> const AnyValue& {
    return other.value_;
  }

  AnyValue value_;
};

static_assert(LyraValue<LaidOutValue>);
static_assert(NetResolvable<LaidOutValue>);
static_assert(CaseEqualComparable<LaidOutValue>);
static_assert(BitstreamSizable<LaidOutValue, ConstIntegralView>);
static_assert(TakesBytesInPlace<LaidOutValue>);

}  // namespace lyra::value
