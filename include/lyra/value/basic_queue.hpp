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

#include "lyra/base/simulation_error.hpp"
#include "lyra/value/container_lease.hpp"
#include "lyra/value/element_policy.hpp"
#include "lyra/value/element_sequence.hpp"
#include "lyra/value/formation.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/position.hpp"
#include "lyra/value/queue_bound.hpp"

namespace lyra::value {

// Where a queue reads its bound (LRM 7.10.5) from: the largest index it holds,
// below zero where there is none. The bound is a fact of the queue's type,
// so it is read off whatever states that type -- the C++ type, or the type the
// library was handed.
template <typename B>
concept QueueBound = std::copyable<B> && requires(const B& b) {
  { b.Bound() } -> std::same_as<std::int64_t>;
};

// The elements a slice of a queue names: where they start and how many there
// are.
struct SliceWindow {
  std::size_t start = 0;
  std::size_t count = 0;
};

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
// A bounded queue (LRM 7.10.5) holds no element whose index exceeds its bound,
// so a write that grows the queue past it drops what is past it.
//
// A position arrives as what the program's position value names: a number, or
// none where it holds x or z.
template <ElementPolicy Elem, QueueBound Bound>
class BasicQueue {
 public:
  BasicQueue()
    requires std::default_initializable<Elem> &&
                 std::default_initializable<Bound>
  = default;
  BasicQueue(Elem elem, Bound bound)
      : elem_(std::move(elem)), bound_(std::move(bound)) {
  }

  // It delegates, so that the queue is already one whose destructor runs when
  // a copy throws, ending the elements copied before it.
  BasicQueue(const BasicQueue& other) : BasicQueue(other.elem_, other.bound_) {
    Reserve(other.count_);
    for (std::size_t i = 0; i < other.count_; ++i) {
      ring_[i] = CopyElement(elem_, other.At(i));
      count_ = i + 1;
    }
  }
  BasicQueue(BasicQueue&& other) noexcept
      : elem_(std::move(other.elem_)),
        bound_(other.bound_),
        ring_(std::move(other.ring_)),
        head_(std::exchange(other.head_, 0)),
        count_(std::exchange(other.count_, 0)) {
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

  // LRM 7.10.1 / 7.4.5 read: an index outside `0..size-1` (or carrying x/z)
  // reads the element default, and a read never grows the queue.
  [[nodiscard]] auto ElementAt(std::optional<std::int64_t> position) const
      -> const void* {
    const auto ordinal = ElementOrdinal(position, count_);
    return ordinal ? At(*ordinal) : elem_.Default();
  }

  // The element a position names, or, where it names none, storage no read
  // reaches -- the place a write that may not grow the queue lands.
  [[nodiscard]] auto ExistingAt(std::optional<std::int64_t> position) -> void* {
    const auto ordinal = ElementOrdinal(position, count_);
    return ordinal ? At(*ordinal) : elem_.DiscardedWrite();
  }

  // LRM 7.10.1 write: `q[$+1] = v` (index == size) appends an element holding
  // the default and lands there. An x/z, negative, or beyond-`$+1` index lands
  // where no read reaches, so the write is discarded, and so does an append a
  // bounded queue cannot keep, which leaves the queue as it was.
  [[nodiscard]] auto ElementMut(
      std::optional<std::int64_t> at, Formation& formed) -> void* {
    if (const auto ordinal = ElementOrdinal(at, count_)) {
      formed = Formation::kExisting;
      return At(*ordinal);
    }
    if (at && static_cast<std::uint64_t>(*at) == count_) {
      InsertCopy(count_, elem_.Default());
      EnforceBound();
      if (static_cast<std::uint64_t>(*at) < count_) {
        formed = Formation::kMade;
        return At(static_cast<std::size_t>(*at));
      }
    }
    formed = Formation::kNowhere;
    return elem_.DiscardedWrite();
  }

  // A lease a call takes on `element`, an element of this queue, which it then
  // goes on reaching though the queue removes it (LRM 13.5.2, 7.10.3).
  [[nodiscard]] auto Lease(void* element) const -> ContainerLease {
    return LeaseElement(elem_, element);
  }

  // LRM 7.10.1 queue slice: the elements from position `lo` through `hi`. A
  // bound that names no position, or `lo > hi` after clamping, yields the empty
  // queue; `lo` clamps up to 0 and `hi` down to the last index. The slice is a
  // queue of the type the selection answers, whose bound `into` states and
  // which need not be this queue's.
  template <QueueBound Into>
  [[nodiscard]] auto Slice(
      std::optional<std::int64_t> first, std::optional<std::int64_t> last,
      Into into) const -> BasicQueue<Elem, Into> {
    BasicQueue<Elem, Into> out(elem_, std::move(into));
    const SliceWindow window = WindowOf(first, last);
    for (std::size_t i = 0; i < window.count; ++i) {
      out.PushBack(At(window.start + i));
    }
    return out;
  }

  // LRM 7.6 / 7.10.1: the elements of that same slice take `replacement`,
  // element for element. A slice is assignment compatible only with an array
  // of as many elements, so one of another count is a run-time error and
  // nothing is written. Answers the elements written where any of them took a
  // different value (LRM 4.3).
  auto AssignSlice(
      std::optional<std::int64_t> first, std::optional<std::int64_t> last,
      std::span<const void* const> replacement) -> std::optional<SliceWindow> {
    const SliceWindow window = WindowOf(first, last);
    if (replacement.size() != window.count) {
      throw SimulationError(
          "a slice of a queue holding {} elements cannot be assigned an "
          "array of {}",
          window.count, replacement.size());
    }
    bool moved = false;
    for (std::size_t i = 0; i < window.count; ++i) {
      void* element = At(window.start + i);
      if (elem_.BitIdentical(element, replacement[i])) {
        continue;
      }
      elem_.Assign(element, replacement[i]);
      moved = true;
    }
    if (!moved) {
      return std::nullopt;
    }
    return window;
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

  // LRM 6.24.3: the elements' own streams, each written by `ops`, into
  // `stream` below its `filled` most significant positions, the front one most
  // significant. Answers how many are filled after them.
  template <typename Ops>
  [[nodiscard]] auto WriteToStream(
      const IntegralView& stream, std::uint64_t filled, const Ops& ops) const
      -> std::uint64_t {
    return detail::SequenceWriteToStream(*this, stream, filled, ops);
  }

  // The inverse: as many elements as fill the stream from its `taken` most
  // significant position up to `until`, each read by `ops`, replace these,
  // held to the bound (LRM 6.24.3, 11.4.14.4). Answers how many positions are
  // taken after them.
  template <typename Ops>
  auto ReadFromStream(
      const ConstIntegralView& stream, std::uint64_t taken, std::uint64_t until,
      const Ops& ops) -> std::uint64_t {
    const detail::ElementsRead<Elem> read(elem_, ops, stream, taken, until);
    Assign(read.Addresses());
    return until;
  }

  // LRM 11.4.14.4: the stream of the `count` elements from `start`, each the
  // element default where it lies outside the queue; and these elements with
  // those taking `values`, element for element, the queue first grown to hold
  // them and then held to its bound. A start naming no position names no
  // element to write.
  template <typename Ops>
  [[nodiscard]] auto WindowToBitVec(
      std::optional<std::int64_t> start, std::size_t count,
      const Ops& ops) const -> BitVec {
    return detail::StreamOfElements(
        ops, detail::SequenceSliceElements(*this, start, count));
  }
  [[nodiscard]] auto WithWindow(
      std::optional<std::int64_t> start, std::size_t count,
      std::span<const void* const> values) const -> BasicQueue {
    BasicQueue out(*this);
    if (count > 0 && start.has_value() && *start >= 0) {
      const std::size_t reaches = static_cast<std::size_t>(*start) + count;
      while (out.count_ < reaches) {
        out.InsertCopy(out.count_, elem_.Default());
      }
    }
    detail::SequenceAssignSlice(out, start, count, values);
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
  void Insert(std::optional<std::int64_t> at, const void* item) {
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
  void DeleteIndex(std::optional<std::int64_t> index) {
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
  // default. `ops` compares two elements.
  template <typename Ops>
  [[nodiscard]] auto MergeConditional(
      const BasicQueue& other, const Ops& ops) const -> BasicQueue {
    BasicQueue result = *this;
    if (count_ != other.count_) {
      result.Truncate(0);
      return result;
    }
    for (std::size_t i = 0; i < count_; ++i) {
      if (!ops.Agree(At(i), other.At(i))) {
        elem_.Assign(result.At(i), elem_.Default());
      }
    }
    return result;
  }

 private:
  // The elements a slice `[first:last]` names (LRM 7.10.1): `first` clamped up
  // to 0 and `last` down to the last index, none where either bound names no
  // position or the first clamped lies past the last.
  [[nodiscard]] auto WindowOf(
      std::optional<std::int64_t> first, std::optional<std::int64_t> last) const
      -> SliceWindow {
    if (!first || !last) {
      return {};
    }
    const std::int64_t a = std::max<std::int64_t>(*first, 0);
    const std::int64_t b =
        std::min<std::int64_t>(*last, static_cast<std::int64_t>(count_) - 1);
    if (a > b) {
      return {};
    }
    return SliceWindow{
        .start = static_cast<std::size_t>(a),
        .count = static_cast<std::size_t>(b - a + 1)};
  }

  // Ends the element at `position`; the ones after it move one place forward.
  void Erase(std::size_t position) {
    void* slot = At(position);
    Unlink(position);
    FreeElement(elem_, slot);
  }

  // LRM 7.10.5: what grew past the bound is dropped, which is worth saying.
  void EnforceBound() {
    const std::int64_t bound = bound_.Bound();
    if (bound < 0) {
      return;
    }
    const auto cap = static_cast<std::size_t>(bound) + 1;
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
    TakeElement(elem_, slot, out);
    FreeElement(elem_, slot);
  }

  void Truncate(std::size_t count) {
    while (count_ > count) {
      Erase(count_ - 1);
    }
  }

  // The ring is grown first, so that nothing can fail once the copy holds
  // storage of its own.
  void InsertCopy(std::size_t position, const void* value) {
    Reserve(count_ + 1);
    Link(position, CopyElement(elem_, value));
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
  [[no_unique_address]] Bound bound_;
  std::vector<void*> ring_;
  std::size_t head_ = 0;
  std::size_t count_ = 0;
};

}  // namespace lyra::value
