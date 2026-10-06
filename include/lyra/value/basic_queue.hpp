#pragma once

#include <algorithm>
#include <bit>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <utility>
#include <vector>

#include "lyra/value/element_policy.hpp"
#include "lyra/value/element_sequence.hpp"
#include "lyra/value/formation.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/position.hpp"
#include "lyra/value/queue_bound.hpp"

namespace lyra::value {

// A queue (LRM 7.10), written once over what its element answers: compiled
// with the element's C++ type for the C++ backend, and once in the library,
// with the element type's table, for the execution backend. Every element is
// handed in and out by its address.
//
// Each element is storage of its own that never moves for as long as it is in
// the queue, and the queue holds the order of those elements as a ring of their
// addresses, so an element is reached in two loads, and inserting, removing and
// reordering move addresses rather than elements: an element's identity is not
// its position (LRM 7.10.3). The ring's length is a power of two, so a position
// is reduced to a ring index by a mask.
//
// A bounded queue (LRM 7.10.5) holds no element whose index exceeds its bound.
// The bound is a property of the variable rather than of the value written, so
// a write that grows the queue past it drops what is past it.
template <ElementPolicy Elem>
class BasicQueue {
 public:
  BasicQueue()
    requires std::default_initializable<Elem>
  = default;
  explicit BasicQueue(Elem elem, std::optional<std::uint64_t> bound = {})
      : elem_(std::move(elem)), bound_(bound) {
  }

  BasicQueue(const BasicQueue& other)
      : elem_(other.elem_), bound_(other.bound_) {
    Reserve(other.count_);
    for (std::size_t i = 0; i < other.count_; ++i) {
      void* slot = AllocateElement(elem_);
      elem_.Copy(other.At(i), slot);
      ring_[i] = slot;
      count_ = i + 1;
    }
  }
  BasicQueue(BasicQueue&& other) noexcept
      : elem_(std::move(other.elem_)),
        ring_(std::move(other.ring_)),
        head_(std::exchange(other.head_, 0)),
        count_(std::exchange(other.count_, 0)),
        bound_(other.bound_),
        discard_(std::exchange(other.discard_, nullptr)) {
  }
  auto operator=(const BasicQueue& other) -> BasicQueue& {
    if (this != &other) {
      BasicQueue copy(other);
      Swap(copy);
    }
    return *this;
  }
  auto operator=(BasicQueue&& other) noexcept -> BasicQueue& {
    if (this != &other) {
      BasicQueue taken(std::move(other));
      Swap(taken);
    }
    return *this;
  }
  ~BasicQueue() {
    Truncate(0);
    if (discard_ != nullptr) {
      FreeElement(elem_, discard_);
    }
  }

  [[nodiscard]] auto Element() const -> const Elem& {
    return elem_;
  }
  [[nodiscard]] auto Count() const -> std::size_t {
    return count_;
  }
  [[nodiscard]] auto At(std::size_t position) const -> const void* {
    return ring_[RingIndex(position)];
  }
  [[nodiscard]] auto At(std::size_t position) -> void* {
    return ring_[RingIndex(position)];
  }

  // The same elements, held to `bound` (a negative one being no bound).
  [[nodiscard]] auto WithBound(const PackedArray& bound) const -> BasicQueue {
    BasicQueue result = *this;
    result.bound_ = BoundOf(bound);
    result.EnforceBound();
    return result;
  }
  void SetBound(const PackedArray& bound) {
    bound_ = BoundOf(bound);
    EnforceBound();
  }

  // LRM 7.10.1 / 7.4.5 read: an index outside `0..size-1` (or carrying x/z)
  // reads the element default, and a read never grows the queue.
  [[nodiscard]] auto ElementAt(const PackedArray& position) const -> const
      void* {
    const auto ordinal = ElementOrdinal(position, count_);
    return ordinal ? At(*ordinal) : elem_.Default();
  }

  // The element a position names, or, where it names none, storage no read
  // reaches -- the place a write that may not grow the queue lands.
  [[nodiscard]] auto ExistingAt(const PackedArray& position) -> void* {
    const auto ordinal = ElementOrdinal(position, count_);
    return ordinal ? At(*ordinal) : DiscardTarget(elem_, discard_);
  }

  // LRM 7.10.1 write: `q[$+1] = v` (index == size) appends an element holding
  // the default and lands there. An x/z, negative, or beyond-`$+1` index lands
  // where no read reaches, so the write is discarded, and so does an append a
  // bounded queue cannot keep, which leaves the queue as it was.
  [[nodiscard]] auto ElementRef(const PackedArray& position, Formation& formed)
      -> void* {
    if (const auto ordinal = ElementOrdinal(position, count_)) {
      formed = Formation::kExisting;
      return At(*ordinal);
    }
    const std::optional<std::int64_t> at = ReadPosition(position);
    if (at && static_cast<std::uint64_t>(*at) == count_) {
      InsertCopy(count_, elem_.Default());
      EnforceBound();
      if (static_cast<std::uint64_t>(*at) < count_) {
        formed = Formation::kMade;
        return At(static_cast<std::size_t>(*at));
      }
    }
    formed = Formation::kNowhere;
    return DiscardTarget(elem_, discard_);
  }

  // LRM 7.10.1 queue slice: the elements from position `lo` through `hi`. A
  // bound that names no position, or `lo > hi` after clamping, yields the empty
  // queue; `lo` clamps up to 0 and `hi` down to the last index. The result
  // carries no bound: a bound belongs to the variable a value is stored into.
  [[nodiscard]] auto Slice(const PackedArray& lo, const PackedArray& hi) const
      -> BasicQueue {
    BasicQueue out(elem_);
    const std::optional<std::int64_t> first = ReadPosition(lo);
    const std::optional<std::int64_t> last = ReadPosition(hi);
    if (!first || !last) {
      return out;
    }
    const std::int64_t a = std::max<std::int64_t>(*first, 0);
    const std::int64_t b =
        std::min<std::int64_t>(*last, static_cast<std::int64_t>(count_) - 1);
    for (std::int64_t i = a; i <= b; ++i) {
      out.InsertCopy(out.count_, At(static_cast<std::size_t>(i)));
    }
    return out;
  }

  // LRM 7.10.2.7 / 7.10.2.6: one element added at the back or the front, the
  // queue then held to its bound.
  void PushBack(const void* item) {
    InsertCopy(count_, item);
    EnforceBound();
  }
  void PushFront(const void* item) {
    InsertCopy(0, item);
    EnforceBound();
  }

  // LRM 10.10 unpacked concatenation, as the steps a join folds to: a copy of
  // this queue with `items` appended in order, held to the bound as they land.
  [[nodiscard]] auto Concat(std::span<const void* const> items) const
      -> BasicQueue {
    BasicQueue out = *this;
    for (const void* item : items) {
      out.InsertCopy(out.count_, item);
    }
    out.EnforceBound();
    return out;
  }

  // LRM 7.6: the elements `items` names, in order, held to the bound.
  void Assign(std::span<const void* const> items) {
    Truncate(0);
    for (const void* item : items) {
      InsertCopy(count_, item);
    }
    EnforceBound();
  }

  // LRM 7.10.2.4 / 7.10.2.5: the element at the front or the back, moved into
  // `out` and removed. An empty queue has none to remove, so `out` takes the
  // element default and the queue stays as it is.
  void PopFront(void* out) {
    Pop(0, out);
  }
  void PopBack(void* out) {
    Pop(count_ == 0 ? 0 : count_ - 1, out);
  }

  // LRM 7.10.2.2: inserts a copy of `item` before `index`, where
  // `index == size` appends. An x or z, negative, or beyond-size index leaves
  // the queue unchanged.
  void Insert(const PackedArray& index, const void* item) {
    const std::optional<std::int64_t> at = ReadPosition(index);
    if (!at.has_value() || *at < 0 ||
        static_cast<std::uint64_t>(*at) > count_) {
      return;
    }
    InsertCopy(static_cast<std::size_t>(*at), item);
    EnforceBound();
  }

  // LRM 7.10.2.3: empties the queue, or removes the element at `index`. An
  // invalid index leaves the queue unchanged.
  void Delete() {
    Truncate(0);
  }
  void DeleteIndex(const PackedArray& index) {
    if (const auto ordinal = ElementOrdinal(index, count_)) {
      Erase(*ordinal);
    }
  }

  // Puts the element that was at `order[k]` at position `k` (LRM 7.12.2). The
  // elements themselves stay where they are.
  void Permute(std::span<const std::size_t> order) {
    std::vector<void*> slots;
    slots.reserve(order.size());
    for (const std::size_t from : order) {
      slots.push_back(At(from));
    }
    for (std::size_t k = 0; k < slots.size(); ++k) {
      ring_[RingIndex(k)] = slots[k];
    }
  }

  // LRM 11.4.11: the two arms of a conditional operator whose condition is
  // ambiguous, combined element by element -- an element the arms agree on
  // survives, and one they disagree on, or cannot know, takes the element
  // default (Table 7-1). Arms of unequal size put no elements in
  // correspondence, so the result is the empty queue that is a queue's own
  // default.
  [[nodiscard]] auto MergeConditional(const BasicQueue& other) const
      -> BasicQueue {
    BasicQueue result = *this;
    if (count_ != other.count_) {
      result.Truncate(0);
      return result;
    }
    for (std::size_t i = 0; i < count_; ++i) {
      if (!detail::ElementsAgree(elem_, At(i), other.At(i))) {
        elem_.Assign(result.At(i), elem_.Default());
      }
    }
    return result;
  }

 private:
  // Ends the element at `position`; the ones after it move one place forward.
  void Erase(std::size_t position) {
    void* slot = At(position);
    Unlink(position);
    FreeElement(elem_, slot);
  }

  // A bound is the greatest index the queue may hold (LRM 7.10.5). A queue with
  // no bound spells that as a negative one, so a bound and its absence reach
  // every construction and every store as the same operand.
  [[nodiscard]] static auto BoundOf(const PackedArray& bound)
      -> std::optional<std::uint64_t> {
    const std::int64_t value = bound.ToInt64();
    if (value < 0) {
      return std::nullopt;
    }
    return static_cast<std::uint64_t>(value);
  }

  // LRM 7.10.5: what grew past the bound is dropped, which is worth saying.
  void EnforceBound() {
    if (!bound_.has_value()) {
      return;
    }
    const auto cap = static_cast<std::size_t>(*bound_) + 1;
    if (count_ > cap) {
      Truncate(cap);
      ReportBoundOverflow();
    }
  }

  void Pop(std::size_t position, void* out) {
    if (count_ == 0) {
      elem_.Copy(elem_.Default(), out);
      return;
    }
    void* slot = At(position);
    Unlink(position);
    elem_.Move(slot, out);
    FreeElement(elem_, slot);
  }

  void Truncate(std::size_t count) {
    while (count_ > count) {
      Erase(count_ - 1);
    }
  }

  void InsertCopy(std::size_t position, const void* value) {
    void* slot = AllocateElement(elem_);
    elem_.Copy(value, slot);
    Link(position, slot);
  }

  [[nodiscard]] auto RingIndex(std::size_t position) const -> std::size_t {
    return (head_ + position) & (ring_.size() - 1);
  }

  void Swap(BasicQueue& other) noexcept {
    std::swap(elem_, other.elem_);
    std::swap(ring_, other.ring_);
    std::swap(head_, other.head_);
    std::swap(count_, other.count_);
    std::swap(bound_, other.bound_);
    std::swap(discard_, other.discard_);
  }

  // A ring at least `count` long, holding the same order from index zero.
  void Reserve(std::size_t count) {
    if (count <= ring_.size()) {
      return;
    }
    std::vector<void*> grown(std::bit_ceil(std::max<std::size_t>(count, 4)));
    for (std::size_t i = 0; i < count_; ++i) {
      grown[i] = At(i);
    }
    ring_ = std::move(grown);
    head_ = 0;
  }

  // Places `slot` at `position`, moving whichever side of it is shorter.
  void Link(std::size_t position, void* slot) {
    Reserve(count_ + 1);
    if (position < count_ - position) {
      head_ = (head_ + ring_.size() - 1) & (ring_.size() - 1);
      for (std::size_t i = 0; i < position; ++i) {
        ring_[RingIndex(i)] = ring_[RingIndex(i + 1)];
      }
    } else {
      for (std::size_t i = count_; i > position; --i) {
        ring_[RingIndex(i)] = ring_[RingIndex(i - 1)];
      }
    }
    ring_[RingIndex(position)] = slot;
    ++count_;
  }

  // Closes the gap `position` leaves, moving whichever side of it is shorter.
  void Unlink(std::size_t position) {
    if (position < count_ - 1 - position) {
      for (std::size_t i = position; i > 0; --i) {
        ring_[RingIndex(i)] = ring_[RingIndex(i - 1)];
      }
      head_ = (head_ + 1) & (ring_.size() - 1);
    } else {
      for (std::size_t i = position; i + 1 < count_; ++i) {
        ring_[RingIndex(i)] = ring_[RingIndex(i + 1)];
      }
    }
    --count_;
  }

  Elem elem_;
  std::vector<void*> ring_;
  std::size_t head_ = 0;
  std::size_t count_ = 0;
  std::optional<std::uint64_t> bound_;
  void* discard_ = nullptr;
};

}  // namespace lyra::value
