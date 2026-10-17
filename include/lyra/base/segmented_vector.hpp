#pragma once

#include <bit>
#include <cstddef>
#include <span>
#include <utility>
#include <vector>

namespace lyra::base {

// A sequence that never moves an element it holds: growing it adds a segment
// and leaves every earlier one where it is, so a reference to an element stays
// good for the sequence's life. A pool is read while it is still being added
// to, in a language that cannot refuse a reference read after an append, so
// storage that does not move is what makes such a read sound. The name and the
// idea are WebKit's and Mozilla's (`WTF::SegmentedVector`,
// `mfbt::SegmentedVector`); segments double in size, as `rustc_arena`'s chunks
// do, so a small sequence stays small and a large one is a few dozen
// allocations.
template <typename T>
class SegmentedVector {
 public:
  SegmentedVector() = default;

  // A copy holds its own segments, each with the capacity its position gives
  // it, so appending to the copy moves nothing either.
  SegmentedVector(const SegmentedVector& other) : size_(other.size_) {
    segments_.reserve(other.segments_.size());
    for (std::size_t k = 0; k < other.segments_.size(); ++k) {
      std::vector<T>& segment = segments_.emplace_back();
      segment.reserve(SegmentCapacity(k));
      segment.insert(
          segment.end(), other.segments_[k].begin(), other.segments_[k].end());
    }
  }

  auto operator=(const SegmentedVector& other) -> SegmentedVector& {
    if (this != &other) {
      SegmentedVector copy(other);
      *this = std::move(copy);
    }
    return *this;
  }

  // A sequence moved from is empty, as a `std::vector` moved from is, so it
  // can be filled again from the start.
  SegmentedVector(SegmentedVector&& other) noexcept
      : segments_(std::move(other.segments_)),
        size_(std::exchange(other.size_, 0)) {
    other.segments_.clear();
  }

  auto operator=(SegmentedVector&& other) noexcept -> SegmentedVector& {
    if (this != &other) {
      segments_ = std::move(other.segments_);
      other.segments_.clear();
      size_ = std::exchange(other.size_, 0);
    }
    return *this;
  }

  ~SegmentedVector() = default;

  [[nodiscard]] auto operator[](std::size_t index) const -> const T& {
    return At(segments_, index);
  }

  [[nodiscard]] auto operator[](std::size_t index) -> T& {
    const auto [segment, offset] = Locate(index);
    return segments_[segment][offset];
  }

  void push_back(const T& value) {
    OpenRoom().push_back(value);
    ++size_;
  }

  void push_back(T&& value) {
    OpenRoom().push_back(std::move(value));
    ++size_;
  }

  // Appends default values until the sequence holds `count`, for a table
  // written at whichever index its writer reaches first.
  void GrowTo(std::size_t count) {
    while (size_ < count) {
      push_back(T{});
    }
  }

  [[nodiscard]] auto size() const -> std::size_t {
    return size_;
  }

  [[nodiscard]] auto empty() const -> bool {
    return size_ == 0;
  }

  // An iterator reaches the segments rather than the sequence, so it stays good
  // where the sequence object is moved, as a `std::vector`'s does, and like one
  // it is not good past an append: a walk that adds to what it walks goes by
  // index.
  class Iterator {
   public:
    using difference_type = std::ptrdiff_t;
    using value_type = T;

    Iterator() = default;

    Iterator(std::span<const std::vector<T>> segments, std::size_t index)
        : segments_(segments), index_(index) {
    }

    auto operator*() const -> const T& {
      return At(segments_, index_);
    }

    auto operator->() const -> const T* {
      return &At(segments_, index_);
    }

    auto operator++() -> Iterator& {
      ++index_;
      return *this;
    }

    auto operator++(int) -> Iterator {
      Iterator before = *this;
      ++index_;
      return before;
    }

    auto operator==(const Iterator& other) const -> bool {
      return index_ == other.index_;
    }

   private:
    std::span<const std::vector<T>> segments_;
    std::size_t index_ = 0;
  };

  [[nodiscard]] auto begin() const -> Iterator {
    return Iterator{segments_, 0};
  }

  [[nodiscard]] auto end() const -> Iterator {
    return Iterator{segments_, size_};
  }

  // Equal when they hold equal values in the same order; where the segments
  // fall follows from the count alone, so it says nothing more.
  [[nodiscard]] auto operator==(const SegmentedVector& other) const -> bool {
    return segments_ == other.segments_;
  }

 private:
  // Segment k holds 2^k elements and starts at index 2^k - 1, so a sequence
  // never holds room for more than twice what it has, as a `std::vector`.
  static constexpr auto SegmentCapacity(std::size_t segment) -> std::size_t {
    return std::size_t{1} << segment;
  }

  struct Position {
    std::size_t segment;
    std::size_t offset;
  };

  static constexpr auto Locate(std::size_t index) -> Position {
    const std::size_t segment = std::bit_width(index + 1) - 1;
    return Position{
        .segment = segment, .offset = index + 1 - SegmentCapacity(segment)};
  }

  // The segment the next element goes into, opened when the last is full.
  auto OpenRoom() -> std::vector<T>& {
    if (segments_.empty() ||
        segments_.back().size() == SegmentCapacity(segments_.size() - 1)) {
      std::vector<T> segment;
      segment.reserve(SegmentCapacity(segments_.size()));
      segments_.push_back(std::move(segment));
    }
    return segments_.back();
  }

  static auto At(std::span<const std::vector<T>> segments, std::size_t index)
      -> const T& {
    const auto [segment, offset] = Locate(index);
    return segments[segment][offset];
  }

  std::vector<std::vector<T>> segments_;
  std::size_t size_ = 0;
};

}  // namespace lyra::base
