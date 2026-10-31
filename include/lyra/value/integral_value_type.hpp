#pragma once

#include <cstddef>
#include <cstdint>
#include <type_traits>

#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::value {

// The table every integral type answers through, and the functions of the
// language's operations on an integral value: each reads the shape off the type
// it is handed, so generated code stating an integral type names these rather
// than stating functions of its own.
extern "C" {
extern const ValueWitnessTable lyra_rt_integral_value_witness_table;
extern const OperationFunctions lyra_rt_integral_operation_functions;
}

// The type of an integral value (LRM 6.11) as code compiled before any design
// existed is handed it: the type's numbers, then the value's shape, at which
// every operation on a whole value is carried out over the bytes the value is
// laid out in. The library states one for each type its own code names, and
// generated code one for each integral type it hands a value of to such code.
class IntegralValueType {
 public:
  explicit constexpr IntegralValueType(IntegralShape shape)
      : type_(
            lyra_rt_integral_value_witness_table,
            static_cast<std::uint32_t>(
                IntegralBytesFor(shape.width, shape.domain)),
            static_cast<std::uint32_t>(IntegralAlignFor(shape.width)),
            ValueTypeKind::kIntegral),
        shape_(shape) {
  }

  // The library's type of the integral type `T`.
  template <IntegralValue T>
  [[nodiscard]] static auto Of() -> const IntegralValueType& {
    static constinit const IntegralValueType kType{kShapeOf<T>};
    return kType;
  }

  [[nodiscard]] auto Type() const -> const ValueType& {
    return type_;
  }
  [[nodiscard]] auto Shape() const -> IntegralShape {
    return shape_;
  }

  // The value laid out at `value`, read as words.
  [[nodiscard]] auto Load(const void* value) const -> LoadedWords {
    return LoadedWords::Load(value, shape_);
  }

  // Where the type states its shape, past the numbers every type states.
  [[nodiscard]] static constexpr auto ShapeStatedAt() -> std::size_t {
    return offsetof(IntegralValueType, shape_);
  }

 private:
  ValueType type_;
  IntegralShape shape_;
};

static_assert(std::is_standard_layout_v<IntegralValueType>);

}  // namespace lyra::value
