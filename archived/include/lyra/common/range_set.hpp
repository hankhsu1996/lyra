#pragma once

#include <cstdint>
#include <span>
#include <vector>

namespace lyra::common {

struct ByteRange {
  uint32_t offset = 0;
  uint32_t size = 0;
  auto operator==(const ByteRange&) const -> bool = default;
};

// Sorted, non-overlapping, merged range set.
//
// A default-constructed RangeSet is empty (not dirty).
// Call MarkFullExtent() to mean "everything dirty."
// Invariant: all entries have size > 0; sorted by offset; no overlap/adjacency.
class RangeSet {
 public:
  RangeSet() = default;

  // Insert a range and merge with overlapping/adjacent ranges.
  // Precondition: size > 0.
  void Insert(uint32_t offset, uint32_t size);

  // Mark full-extent (clear all ranges, semantics = "everything dirty").
  void MarkFullExtent() {
    ranges_.clear();
    is_full_extent_ = true;
  }

  [[nodiscard]] auto IsFullExtent() const -> bool {
    return is_full_extent_;
  }
  [[nodiscard]] auto Ranges() const -> std::span<const ByteRange> {
    return ranges_;
  }
  [[nodiscard]] auto IsEmpty() const -> bool {
    return !is_full_extent_ && ranges_.empty();
  }

  // Check if any range overlaps [obs_off, obs_off + obs_size).
  // Full-extent always overlaps. Precondition: obs_size > 0.
  [[nodiscard]] auto Overlaps(uint32_t obs_off, uint32_t obs_size) const
      -> bool;

  void Clear() {
    ranges_.clear();
    is_full_extent_ = false;
  }

 private:
  std::vector<ByteRange> ranges_;
  bool is_full_extent_ = false;
};

}  // namespace lyra::common
