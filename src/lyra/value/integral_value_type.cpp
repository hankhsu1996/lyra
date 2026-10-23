#include "lyra/value/integral_value_type.hpp"

#include <cstddef>
#include <cstdint>
#include <cstring>
#include <memory>
#include <utility>

#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::value {

namespace {

// The functions every integral type answers through, each reading the shape
// off the type it is handed.
struct IntegralWitnesses {
  static auto Of(const ValueType& type) -> const IntegralValueType& {
    return *type.AsIntegral();
  }

  static auto Load(const ValueType& type, const void* value) -> LoadedWords {
    return Of(type).Load(value);
  }

  // A fold of two values of the type into a third, which `fold` carries out.
  template <typename Fold>
  static void Folded(
      const ValueType& type, const void* lhs, const void* rhs, void* out,
      Fold fold) {
    const LoadedWords a = Load(type, lhs);
    const LoadedWords b = Load(type, rhs);
    WrittenWords result(Of(type).Shape());
    fold(result.MutableView(), a.View(), b.View());
    result.StoreTo(out);
  }

  static void Copy(const ValueType& type, const void* value, void* out) {
    std::memcpy(out, value, type.Size());
  }

  static void Move(const ValueType& type, void* value, void* out) noexcept {
    std::memcpy(out, value, type.Size());
  }

  // A value is its bytes, so ending one is ending them, which leaves nothing to
  // run.
  static void Destroy(const ValueType& type, void* value) noexcept {
    std::destroy_n(static_cast<std::byte*>(value), type.Size());
  }

  static void Assign(const ValueType& type, void* storage, const void* value) {
    std::memmove(storage, value, type.Size());
  }

  static auto Equal(const ValueType& type, const void* lhs, const void* rhs)
      -> FourStateBit {
    return lyra::value::Equal(Load(type, lhs).View(), Load(type, rhs).View());
  }

  // Every position above the width is clear in both planes, so two values are
  // the same bits exactly where their bytes are.
  static auto BitIdentical(
      const ValueType& type, const void* lhs, const void* rhs) -> bool {
    return std::memcmp(lhs, rhs, type.Size()) == 0;
  }

  static auto CaseEqual(const ValueType& type, const void* lhs, const void* rhs)
      -> bool {
    return BitIdentical(type, lhs, rhs);
  }

  static auto HasUnknown(const ValueType& type, const void* value) -> bool {
    return lyra::value::HasUnknown(Load(type, value).View());
  }

  static void BuildDefault(const ValueType& type, void* out) {
    WrittenWords built(Of(type).Shape());
    FillDefault(built.MutableView());
    built.StoreTo(out);
  }

  // LRM 20.6.2, 20.9, 6.24.3: an integral value's stream of bits is its own
  // bits, read unsigned, so how many it holds is the type's alone.
  static constexpr auto BitstreamWidth = [](const ValueType& type, const void*,
                                            void* out) {
    std::construct_at(
        static_cast<Int*>(out),
        Int::FromInt(static_cast<std::int64_t>(Of(type).Shape().width)));
  };

  static void CountBits(
      const ValueType& type, const void* value,
      const ConstIntegralView& control_bits, void* out) {
    std::construct_at(
        static_cast<Int*>(out),
        Int::FromInt(
            lyra::value::CountBits(Load(type, value).View(), control_bits)));
  }

  static auto WriteToStream(
      const ValueType& type, const void* value, const IntegralView& stream,
      std::uint64_t filled) -> std::uint64_t {
    const std::uint64_t width = Of(type).Shape().width;
    Insert(
        stream, Load(type, value).View(),
        static_cast<std::int64_t>(stream.width - filled - width));
    return filled + width;
  }

  // A two-state type reads each x or z of the stream as 0 (LRM 11.4.14.3).
  // Its width is its type's, so where the stream ends past it is no concern.
  static auto ReadFromStream(
      const ValueType& type, const ConstIntegralView& stream,
      std::uint64_t taken, std::uint64_t, void* out) -> std::uint64_t {
    const IntegralShape shape = Of(type).Shape();
    WrittenWords bits(shape);
    Extract(
        bits.MutableView(), stream,
        static_cast<std::int64_t>(stream.width - taken - shape.width));
    bits.StoreTo(out);
    return taken + shape.width;
  }

  static void ResolveTriState(
      const ValueType& type, const void* lhs, const void* rhs, void* out) {
    Folded(
        type, lhs, rhs, out,
        [](const IntegralView& o, const ConstIntegralView& a,
           const ConstIntegralView& b) {
          Resolve(o, a, b, NetResolution::kTriState);
        });
  }

  static void ResolveWiredAnd(
      const ValueType& type, const void* lhs, const void* rhs, void* out) {
    Folded(
        type, lhs, rhs, out,
        [](const IntegralView& o, const ConstIntegralView& a,
           const ConstIntegralView& b) {
          Resolve(o, a, b, NetResolution::kWiredAnd);
        });
  }

  static void ResolveWiredOr(
      const ValueType& type, const void* lhs, const void* rhs, void* out) {
    Folded(
        type, lhs, rhs, out,
        [](const IntegralView& o, const ConstIntegralView& a,
           const ConstIntegralView& b) {
          Resolve(o, a, b, NetResolution::kWiredOr);
        });
  }

  static void Dominating(
      const ValueType& type, const void* stronger, const void* weaker,
      void* out) {
    Folded(
        type, stronger, weaker, out,
        [](const IntegralView& o, const ConstIntegralView& a,
           const ConstIntegralView& b) { Dominate(o, a, b); });
  }

  // An integral value filled is the type's every position at the fill, which
  // takes nothing of the value it is filled like.
  static constexpr auto FilledLike = [](const ValueType& type, const void*,
                                        const void* fill, void* out) {
    WrittenWords filled(Of(type).Shape());
    FillScalar(filled.MutableView(), static_cast<const Logic*>(fill)->Lsb());
    filled.StoreTo(out);
  };

  // An integral value's positions are its bits.
  static void TakePositions(
      const ValueType& type, const void* into, const void* from,
      const std::int64_t* first, const std::int64_t* count, void* out) {
    Folded(
        type, into, from, out,
        [&](const IntegralView& o, const ConstIntegralView& a,
            const ConstIntegralView& b) {
          lyra::value::TakePositions(
              o, a, b,
              BitPositions{
                  .lsb = static_cast<std::uint64_t>(*first),
                  .width = static_cast<std::uint64_t>(*count)});
        });
  }

  // LRM 7.8.4: integral keys order by their numerical value at the type's
  // signedness.
  static auto OrderBefore(
      const ValueType& type, const void* lhs, const void* rhs) -> bool {
    return Less(Load(type, lhs).View(), Load(type, rhs).View()) ==
           FourStateBit::kOne;
  }

  // A packed value's bits are no storage of their own (LRM 7.4.1), so an
  // integral value holds no parts a walk reaches where they lie.
  static constexpr const PartsWitnessTable* kParts = nullptr;
};

}  // namespace

extern "C" {
constinit const ValueWitnessTable lyra_rt_integral_value_witness_table =
    kValueWitnessTableFor<IntegralWitnesses>;
}

}  // namespace lyra::value
