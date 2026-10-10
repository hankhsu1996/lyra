#pragma once

#include <cstdint>

#include "lyra/value/integral_words.hpp"

// The names of the integral types, for what is stated in terms of them before
// the type itself is defined: the contracts every value type answers to, and
// the integral type's own members, which answer at one another's types.
namespace lyra::value {

// How many values a bit of an integral type can take (LRM 6.11.2).
enum class StateDomain : std::uint8_t { kTwoState, kFourState };

template <std::uint64_t kWidth, Signedness kSignedness, StateDomain kDomain>
class Integral;

template <class T>
inline constexpr bool kIsIntegral = false;
template <std::uint64_t kWidth, Signedness kSignedness, StateDomain kDomain>
inline constexpr bool kIsIntegral<Integral<kWidth, kSignedness, kDomain>> =
    true;

template <class T>
concept IntegralValue = kIsIntegral<T>;

// The one-bit answer of a predicate over operands of domain `kDomain`: it can
// be x only where they can.
template <StateDomain kDomain>
using OneBit = Integral<1, Signedness::kUnsigned, kDomain>;

// SystemVerilog's predefined integer types (LRM 6.11.1).
using Bit = Integral<1, Signedness::kUnsigned, StateDomain::kTwoState>;
using Logic = Integral<1, Signedness::kUnsigned, StateDomain::kFourState>;
using Byte = Integral<8, Signedness::kSigned, StateDomain::kTwoState>;
using ShortInt = Integral<16, Signedness::kSigned, StateDomain::kTwoState>;
using Int = Integral<32, Signedness::kSigned, StateDomain::kTwoState>;
using IntUnsigned = Integral<32, Signedness::kUnsigned, StateDomain::kTwoState>;
using LongInt = Integral<64, Signedness::kSigned, StateDomain::kTwoState>;
using Integer = Integral<32, Signedness::kSigned, StateDomain::kFourState>;
using Time = Integral<64, Signedness::kUnsigned, StateDomain::kFourState>;

// Where a select lands, as a value the program computed.
using Position =
    Integral<kPositionWidth, Signedness::kSigned, StateDomain::kFourState>;

// The spellings generated code names a type by: a vector of `kWidth` bits,
// two-state or four-state, unsigned or signed (LRM 6.11).
template <std::uint64_t kWidth>
using BitVector =
    Integral<kWidth, Signedness::kUnsigned, StateDomain::kTwoState>;
template <std::uint64_t kWidth>
using SignedBitVector =
    Integral<kWidth, Signedness::kSigned, StateDomain::kTwoState>;
template <std::uint64_t kWidth>
using LogicVector =
    Integral<kWidth, Signedness::kUnsigned, StateDomain::kFourState>;
template <std::uint64_t kWidth>
using SignedLogicVector =
    Integral<kWidth, Signedness::kSigned, StateDomain::kFourState>;

// Some bits of a value of `T` held where a write lands, read and written at
// the type `Part`.
template <IntegralValue T, IntegralValue Part>
class BitsRef;

}  // namespace lyra::value
