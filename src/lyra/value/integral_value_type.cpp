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

// The table every integral type answers through, each function reading the
// shape off the type it is handed.
struct IntegralWitnesses {
  static auto Of(const ValueType& type) -> const IntegralValueType& {
    return *type.AsIntegral();
  }

  static auto Load(const ValueType& type, const void* value) -> LoadedWords {
    return Of(type).Load(value);
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

  // Every position above the width is clear in both planes, so two values are
  // the same bits exactly where their bytes are.
  static auto BitIdentical(
      const ValueType& type, const void* lhs, const void* rhs) -> bool {
    return std::memcmp(lhs, rhs, type.Size()) == 0;
  }

  // A packed value's bits are no storage of their own (LRM 7.4.1), so an
  // integral value holds no parts a walk reaches where they lie.
  static constexpr const PartsWitnessTable* kParts = nullptr;
};

// The language's operations on an integral value, each reading the shape off
// the type the operations name.
struct IntegralOperations {
  static auto Load(const ValueOperations& self, const void* value)
      -> LoadedWords {
    return IntegralWitnesses::Load(*self.type, value);
  }
  static auto Shape(const ValueOperations& self) -> IntegralShape {
    return IntegralWitnesses::Of(*self.type).Shape();
  }

  static auto Equal(
      const ValueOperations& self, const void* lhs, const void* rhs)
      -> FourStateBit {
    return lyra::value::Equal(Load(self, lhs).View(), Load(self, rhs).View());
  }

  static auto CaseEqual(
      const ValueOperations& self, const void* lhs, const void* rhs) -> bool {
    return IntegralWitnesses::BitIdentical(*self.type, lhs, rhs);
  }

  static auto HasUnknown(const ValueOperations& self, const void* value)
      -> bool {
    return lyra::value::HasUnknown(Load(self, value).View());
  }

  // LRM 20.6.2, 20.9, 6.24.3: an integral value's stream of bits is its own
  // bits, read unsigned, so how many it holds is the type's alone.
  static void BitstreamWidth(
      const ValueOperations& self, const void*, void* out) {
    std::construct_at(
        static_cast<Int*>(out),
        Int::FromInt(static_cast<std::int64_t>(Shape(self).width)));
  }

  static void CountBits(
      const ValueOperations& self, const void* value,
      const ConstIntegralView& control_bits, void* out) {
    std::construct_at(
        static_cast<Int*>(out),
        Int::FromInt(
            lyra::value::CountBits(Load(self, value).View(), control_bits)));
  }

  static auto WriteToStream(
      const ValueOperations& self, const void* value,
      const IntegralView& stream, std::uint64_t filled) -> std::uint64_t {
    const std::uint64_t width = Shape(self).width;
    Insert(
        stream, Load(self, value).View(),
        static_cast<std::int64_t>(stream.width - filled - width));
    return filled + width;
  }

  // A two-state type reads each x or z of the stream as 0 (LRM 11.4.14.3).
  // Its width is its type's, so where the stream ends past it is no concern.
  static auto ReadFromStream(
      const ValueOperations& self, const ConstIntegralView& stream,
      std::uint64_t taken, std::uint64_t, void* out) -> std::uint64_t {
    const IntegralShape shape = Shape(self);
    WrittenWords bits(shape);
    Extract(
        bits.MutableView(), stream,
        static_cast<std::int64_t>(stream.width - taken - shape.width));
    bits.StoreTo(out);
    return taken + shape.width;
  }
};

}  // namespace

extern "C" {
constinit const ValueWitnessTable lyra_rt_integral_value_witness_table =
    kValueWitnessTableFor<IntegralWitnesses>;
constinit const OperationFunctions lyra_rt_integral_operation_functions =
    kOperationFunctionsFor<IntegralOperations>;
}

}  // namespace lyra::value
