#pragma once

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <utility>
#include <vector>

// LRM 7.12.2 ordering, written once over items. A method answers with the
// position each element comes from once the order is applied, and the
// container then moves its own elements, so nothing here knows what an element
// is; what orders two of them is handed in.
namespace lyra::value::detail {

// LRM 7.12.2 reverse: the position each element comes from once the order is
// reversed.
[[nodiscard]] inline auto ReversedPositions(std::size_t count)
    -> std::vector<std::size_t> {
  std::vector<std::size_t> order(count);
  for (std::size_t k = 0; k < count; ++k) {
    order[k] = count - 1 - k;
  }
  return order;
}

// `items` put in the order `comes_first` says, which is told two items and
// answers whether the first comes before the second, equal ones keeping their
// order. The SV comparison over 4-state keys is not a strict weak ordering --
// a key carrying an x / z compares indeterminate, so neither of two comes
// before the other while a definite `a < c` still holds -- and a sort assuming
// one may read outside its range. A merge reads only within the two runs it
// merges whatever the comparison answers, so this one is defined for any
// comparison, and costs n log n comparisons.
template <typename Item, typename ComesFirst>
void MergeSort(std::vector<Item>& items, const ComesFirst& comes_first) {
  const std::size_t count = items.size();
  // Each item of a pass is written over before it is read, so the second
  // buffer starts as any copy of as many.
  std::vector<Item> merged(items);
  for (std::size_t width = 1; width < count; width *= 2) {
    for (std::size_t lo = 0; lo < count; lo += 2 * width) {
      const std::size_t mid = std::min(lo + width, count);
      const std::size_t hi = std::min(lo + (2 * width), count);
      std::size_t left = lo;
      std::size_t right = mid;
      std::size_t out = lo;
      while (left < mid && right < hi) {
        // The right run's item goes first only where it strictly comes
        // before the left's, so equal ones keep their order.
        if (comes_first(items[right], items[left])) {
          merged[out++] = items[right++];
        } else {
          merged[out++] = items[left++];
        }
      }
      while (left < mid) {
        merged[out++] = items[left++];
      }
      while (right < hi) {
        merged[out++] = items[right++];
      }
    }
    std::swap(items, merged);
  }
}

// LRM 7.12.2 sort and rsort: the position each of `count` elements comes from
// once ordered by `before`, which is told two positions and answers whether
// the element at the first comes before the one at the second -- `rsort`'s
// order being `sort`'s with the comparison reversed.
template <typename Before>
[[nodiscard]] auto SortedPositions(std::size_t count, const Before& before)
    -> std::vector<std::size_t> {
  std::vector<std::size_t> order(count);
  for (std::size_t k = 0; k < count; ++k) {
    order[k] = k;
  }
  MergeSort(order, [&](std::size_t a, std::size_t b) {
    return before(static_cast<std::int64_t>(a), static_cast<std::int64_t>(b));
  });
  return order;
}

}  // namespace lyra::value::detail

namespace lyra::value {

// The LRM 7.12.2 ordering methods of a fixed-size unpacked array, whose index
// is the ordinal position, as the library's template states them: reversing
// it, and putting it in an order a sort answered. How the elements move is the
// array's own.
template <typename Self>
class OrdinalArrayMethods {
 public:
  auto Reverse() -> void {
    MutableThis().Permute(detail::ReversedPositions(Count()));
  }
  // The elements put in the order `order`, an array of positions a sort
  // answered, holds: the one at each position becoming the one that was at
  // the position `order` holds there.
  template <typename Order>
  auto ApplyOrder(const Order& order) -> void {
    std::vector<std::size_t> positions;
    positions.reserve(order.Count());
    for (std::size_t k = 0; k < order.Count(); ++k) {
      positions.push_back(
          static_cast<std::size_t>(order.GetUnchecked(k).ToInt64()));
    }
    MutableThis().Permute(positions);
  }

 private:
  friend Self;
  OrdinalArrayMethods() = default;

  [[nodiscard]] auto MutableThis() -> Self& {
    return static_cast<Self&>(*this);
  }
  [[nodiscard]] auto Count() const -> std::size_t {
    return static_cast<const Self&>(*this).Count();
  }
};

}  // namespace lyra::value
