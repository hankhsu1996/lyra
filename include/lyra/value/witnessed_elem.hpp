#pragma once

#include <cstddef>
#include <cstdint>

#include "lyra/value/element_policy.hpp"
#include "lyra/value/element_type.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/take_built.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::value {

// The element of a container the library holds, of a type the library was
// compiled without: what its storage asks of one is answered by the type's
// table, and an index naming no element (LRM 7.4.5) by the element type the
// container was handed.
class WitnessedElem {
 public:
  explicit WitnessedElem(const value::ElementType& element)
      : element_(&element) {
  }

  [[nodiscard]] auto ElementType() const -> const value::ElementType& {
    return *element_;
  }

  // The element type is stated as constant data, which lasts as long as the
  // program does, so its address is this policy's opaque value.
  [[nodiscard]] auto OpaqueValue() const -> const void* {
    return element_;
  }
  [[nodiscard]] static auto FromOpaqueValue(const void* opaque)
      -> WitnessedElem {
    return WitnessedElem(*static_cast<const value::ElementType*>(opaque));
  }

  [[nodiscard]] auto Default() const -> const void* {
    return element_->Default();
  }
  [[nodiscard]] auto DiscardedWrite() const -> void* {
    return element_->DiscardedWrite();
  }

  [[nodiscard]] auto Size() const -> std::size_t {
    return Type().Size();
  }
  [[nodiscard]] auto Align() const -> std::size_t {
    return Type().Align();
  }

  void Copy(const void* value, void* out) const {
    Type().Copy(value, out);
  }
  void Move(void* value, void* out) const {
    Type().Move(value, out);
  }
  void Destroy(void* value) const {
    Type().Destroy(value);
  }
  void Assign(void* storage, const void* value) const {
    Type().Assign(storage, value);
  }

  [[nodiscard]] auto BitIdentical(const void* lhs, const void* rhs) const
      -> bool {
    return Type().BitIdentical(lhs, rhs);
  }

 private:
  [[nodiscard]] auto Type() const -> const ValueType& {
    return element_->Type();
  }

  const value::ElementType* element_;
};

static_assert(ElementPolicy<WitnessedElem>);

// What the erased algorithms compare and stream an element of a type the
// library was compiled without by: the type's operations, handed to the
// operation that asks them, which no container holds.
class WitnessedOperations {
 public:
  explicit WitnessedOperations(const ValueOperations& operations)
      : operations_(&operations) {
  }

  [[nodiscard]] auto Equal(const void* lhs, const void* rhs) const
      -> FourStateBit {
    return Functions().equal(*operations_, lhs, rhs);
  }
  [[nodiscard]] auto CaseEqual(const void* lhs, const void* rhs) const -> bool {
    return Functions().case_equal(*operations_, lhs, rhs);
  }
  [[nodiscard]] static auto CaseAnswer(bool holds) -> Bit {
    return Bit::FromBool(holds);
  }
  [[nodiscard]] auto Agree(const void* lhs, const void* rhs) const -> bool {
    return Holds(Equal(lhs, rhs));
  }
  [[nodiscard]] auto HasUnknown(const void* value) const -> bool {
    return Functions().has_unknown(*operations_, value);
  }
  [[nodiscard]] auto BitstreamWidth(const void* value) const -> std::int64_t {
    return TakeBuilt<Int>([&](void* out) {
             Functions().bitstream_width(*operations_, value, out);
           })
        .ToInt64();
  }
  [[nodiscard]] auto CountBits(
      const void* value, const ConstIntegralView& control_bits) const
      -> std::int64_t {
    return TakeBuilt<Int>([&](void* out) {
             Functions().count_bits(*operations_, value, control_bits, out);
           })
        .ToInt64();
  }
  auto WriteToStream(
      const void* value, const IntegralView& stream, std::uint64_t filled) const
      -> std::uint64_t {
    return Functions().write_to_stream(*operations_, value, stream, filled);
  }
  auto ReadFromStream(
      const ConstIntegralView& stream, std::uint64_t taken, std::uint64_t until,
      void* out) const -> std::uint64_t {
    return Functions().read_from_stream(
        *operations_, stream, taken, until, out);
  }

 private:
  [[nodiscard]] auto Functions() const -> const OperationFunctions& {
    return *operations_->functions;
  }

  const ValueOperations* operations_;
};

}  // namespace lyra::value
