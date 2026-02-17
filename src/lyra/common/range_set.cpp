#include "lyra/common/range_set.hpp"

#include <algorithm>
#include <cstdint>

namespace lyra::common {

void RangeSet::Insert(uint32_t offset, uint32_t size) {
  uint64_t new_start = offset;
  uint64_t new_end =
      static_cast<uint64_t>(offset) + static_cast<uint64_t>(size);

  // Find first range whose end >= new_start (could overlap or be adjacent).
  auto it = ranges_.begin();
  while (it != ranges_.end()) {
    uint64_t r_end =
        static_cast<uint64_t>(it->offset) + static_cast<uint64_t>(it->size);
    if (r_end >= new_start) break;
    ++it;
  }

  // If no existing range overlaps/adjoins, insert before it.
  if (it == ranges_.end() || static_cast<uint64_t>(it->offset) > new_end) {
    ranges_.insert(it, ByteRange{.offset = offset, .size = size});
    return;
  }

  // Merge: extend the found range to cover the union.
  new_start = std::min(new_start, static_cast<uint64_t>(it->offset));
  uint64_t r_end =
      static_cast<uint64_t>(it->offset) + static_cast<uint64_t>(it->size);
  new_end = std::max(new_end, r_end);

  // Absorb subsequent overlapping/adjacent ranges.
  auto merge_end = it + 1;
  while (merge_end != ranges_.end() &&
         static_cast<uint64_t>(merge_end->offset) <= new_end) {
    uint64_t me = static_cast<uint64_t>(merge_end->offset) +
                  static_cast<uint64_t>(merge_end->size);
    new_end = std::max(new_end, me);
    ++merge_end;
  }

  // Write merged range and erase absorbed entries.
  it->offset = static_cast<uint32_t>(new_start);
  it->size = static_cast<uint32_t>(new_end - new_start);
  ranges_.erase(it + 1, merge_end);
}

auto RangeSet::Overlaps(uint32_t obs_off, uint32_t obs_size) const -> bool {
  if (is_full_extent_) return true;

  uint64_t obs_end =
      static_cast<uint64_t>(obs_off) + static_cast<uint64_t>(obs_size);
  for (const auto& r : ranges_) {
    uint64_t r_end =
        static_cast<uint64_t>(r.offset) + static_cast<uint64_t>(r.size);
    if (r.offset < obs_end && obs_off < r_end) return true;
    if (r.offset >= obs_end) break;
  }
  return false;
}

}  // namespace lyra::common
