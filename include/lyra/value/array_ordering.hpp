#pragma once

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <utility>
#include <vector>

// LRM 7.12.2 ordering, written once over positions. A method answers with the
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

// LRM 7.12.2 sort and rsort: the position each of `count` elements comes from
// once ordered by `before`, which is told two positions and answers whether
// the element at the first comes before the one at the second -- `rsort`'s
// order being `sort`'s with the comparison reversed -- and equal ones keep
// their order. The SV comparison over 4-state keys is not a strict weak
// ordering -- a key carrying an x / z compares indeterminate, so neither of two
// comes before the other while a definite `a < c` still holds -- and a sort
// assuming one may read outside its range. A merge reads only within the two
// runs it merges whatever the comparison answers, so this one is defined for
// any comparison, and costs n log n comparisons.
template <typename Before>
[[nodiscard]] auto SortedPositions(std::size_t count, const Before& before)
    -> std::vector<std::size_t> {
  const auto comes_first = [&](std::size_t a, std::size_t b) -> bool {
    return before(static_cast<std::int64_t>(a), static_cast<std::int64_t>(b));
  };
  std::vector<std::size_t> order(count);
  for (std::size_t k = 0; k < count; ++k) {
    order[k] = k;
  }
  std::vector<std::size_t> merged(count);
  for (std::size_t width = 1; width < count; width *= 2) {
    for (std::size_t lo = 0; lo < count; lo += 2 * width) {
      const std::size_t mid = std::min(lo + width, count);
      const std::size_t hi = std::min(lo + (2 * width), count);
      std::size_t left = lo;
      std::size_t right = mid;
      std::size_t out = lo;
      while (left < mid && right < hi) {
        // The right run's element goes first only where it strictly comes
        // before the left's, so equal ones keep their order.
        if (comes_first(order[right], order[left])) {
          merged[out++] = order[right++];
        } else {
          merged[out++] = order[left++];
        }
      }
      while (left < mid) {
        merged[out++] = order[left++];
      }
      while (right < hi) {
        merged[out++] = order[right++];
      }
    }
    std::swap(order, merged);
  }
  return order;
}

}  // namespace lyra::value::detail
