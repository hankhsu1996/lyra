#include "lyra/value/sort_order.hpp"

#include <bit>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/value/array_ordering.hpp"
#include "lyra/value/container_core.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_value_type.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/library_value_types.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::value {

namespace {

// The positions of `keys`, each read once as the value `read` makes of it,
// in the order `less` puts those values in, or the reverse of it. The
// positions are what the merge moves, and each comparison reads the values at
// two of them.
template <typename Read, typename Less>
auto PositionsInOrder(
    const DynamicArrayCore& keys, bool descending, const Read& read,
    const Less& less) -> std::vector<std::size_t> {
  std::vector<decltype(read(keys.data))> values;
  values.reserve(keys.count);
  for (std::size_t i = 0; i < keys.count; ++i) {
    values.push_back(read(DynamicArrayPlace(keys, keys.data, i)));
  }
  std::vector<std::size_t> positions(keys.count);
  for (std::size_t k = 0; k < keys.count; ++k) {
    positions[k] = k;
  }
  if (descending) {
    detail::MergeSort(positions, [&](std::size_t a, std::size_t b) {
      return less(values[b], values[a]);
    });
  } else {
    detail::MergeSort(positions, [&](std::size_t a, std::size_t b) {
      return less(values[a], values[b]);
    });
  }
  return positions;
}

// An integral key of at most one word as the number its order compares (LRM
// 11.4.4): its value at its type's signedness, a signed one offset so that it
// orders as an unsigned word does, and whether it holds no x or z, without
// which a comparison of it is unknown.
struct NarrowKey {
  std::uint64_t ordered;
  bool known;
};

auto NarrowKeyOf(const IntegralValueType& type, const void* at) -> NarrowKey {
  const LoadedWords loaded = type.Load(at);
  const ConstIntegralView view = loaded.View();
  std::uint64_t value = WordAt(view.value, 0);
  const IntegralShape shape = type.Shape();
  if (shape.signedness == Signedness::kSigned) {
    const auto unused = static_cast<unsigned>(64 - shape.width);
    value = std::bit_cast<std::uint64_t>(
        std::bit_cast<std::int64_t>(value << unused) >> unused);
    value ^= std::uint64_t{1} << 63U;
  }
  return {.ordered = value, .known = !HasUnknown(view)};
}

}  // namespace

auto SortOrder(const DynamicArrayCore& keys, bool descending)
    -> std::vector<std::size_t> {
  const ValueType& key = *keys.element;
  if (const IntegralValueType* integral = key.AsIntegral();
      integral != nullptr && integral->Shape().width <= 64) {
    return PositionsInOrder(
        keys, descending,
        [&](const void* at) { return NarrowKeyOf(*integral, at); },
        [](const NarrowKey& a, const NarrowKey& b) {
          return a.known && b.known && a.ordered < b.ordered;
        });
  }
  if (const IntegralValueType* integral = key.AsIntegral()) {
    return PositionsInOrder(
        keys, descending, [&](const void* at) { return integral->Load(at); },
        [](const LoadedWords& a, const LoadedWords& b) {
          return Less(a.View(), b.View()) == FourStateBit::kOne;
        });
  }
  if (&key == &lyra_rt_real_value_type) {
    return PositionsInOrder(
        keys, descending,
        [](const void* at) {
          double value = 0;
          std::memcpy(&value, at, sizeof(value));
          return value;
        },
        [](double a, double b) { return a < b; });
  }
  if (&key == &lyra_rt_shortreal_value_type) {
    return PositionsInOrder(
        keys, descending,
        [](const void* at) {
          float value = 0;
          std::memcpy(&value, at, sizeof(value));
          return value;
        },
        [](float a, float b) { return a < b; });
  }
  if (&key == &lyra_rt_string_value_type) {
    return PositionsInOrder(
        keys, descending,
        [](const void* at) { return static_cast<const String*>(at); },
        [](const String* a, const String* b) {
          return static_cast<bool>(*a < *b);
        });
  }
  throw InternalError(
      "an array is sorted by keys of a type no relational operator orders -- "
      "please report this as a bug");
}

}  // namespace lyra::value
