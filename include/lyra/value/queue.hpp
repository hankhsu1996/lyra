#pragma once

#include <cstddef>
#include <cstdint>
#include <deque>
#include <span>
#include <string>
#include <utility>

#include "lyra/value/format.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/value_concept.hpp"

namespace lyra::value {

// SystemVerilog queue (LRM 7.10): a variable-size ordered collection with
// efficient insertion and removal at both ends, so the storage is a
// `std::deque<T>` rather than the `std::vector<T>` the dynamic array uses.
// Default value is the empty queue (LRM Table 6-7).
//
// `oob_slot_` is the LRM 7.4.5 invalid-index shield seed, supplied at
// construction because `T = PackedArray` carries runtime shape parameters
// (bit width, signedness, 2/4-state) that cannot be recovered from the C++
// type alone. See `docs/decisions/runtime-shape-and-default-value.md`. It
// holds the element's canonical default for construction and the OOB-shield
// protocol (`ResetToDefault`) shared with the other container wrappers.
template <typename T>
class Queue {
 public:
  using ElementType = T;

  // Sentinel "uninitialized" form -- empty queue with a default-constructed
  // OOB slot. Used as the declared default state of a queue field; the first
  // MIR-level assignment overwrites the whole queue (LRM 10.5 variable
  // initialization).
  Queue() = default;

  // Empty queue with the shield slot seeded. Used for declarations like
  // `int q[$];` where the queue starts empty but the element shape is known
  // at lowering time.
  explicit Queue(T oob_slot) : oob_slot_(std::move(oob_slot)) {
  }

  // LRM 10.9.1 assignment-pattern construction: shield slot seeded, elements
  // taken from the pattern's list. The list is a span so the emit side hands
  // in a `std::array<T, N>{...}` literal, mirroring `DynamicArray`.
  Queue(T oob_slot, std::span<const T> init)
      : oob_slot_(std::move(oob_slot)), data_(init.begin(), init.end()) {
  }

  Queue(const Queue&) = default;
  Queue(Queue&&) noexcept = default;
  auto operator=(const Queue&) -> Queue& = default;
  auto operator=(Queue&&) noexcept -> Queue& = default;
  ~Queue() = default;

  [[nodiscard]] auto Size() const -> std::size_t {
    return data_.size();
  }

  [[nodiscard]] auto RawAt(std::size_t i) const -> const T& {
    return data_[i];
  }

  [[nodiscard]] auto Clone() const -> Queue {
    return *this;
  }

  // LRM Table 6-7: a queue's default is the empty queue. When this container
  // is itself an OOB shield slot of an outer container, the outer calls this
  // to restore canonical state before handing out a reference.
  auto ResetToDefault() -> void {
    data_.clear();
  }

  // LRM 7.10.1 / 7.4.5: an index outside 0..size-1 (or carrying x/z) routes
  // the access through `oob_slot_`, restored to its canonical default first so
  // an OOB write lands on the shield and is erased on the next access.
  // TODO(hankhsu): LRM 7.10.1 makes a write to Q[$+1] a legal append; the
  // shared read/write `ElementAt` cannot distinguish it from an invalid write,
  // so that one append corner is deferred (push_back covers the same need).
  [[nodiscard]] auto ElementAt(const PackedArray& idx) -> T& {
    if (IsInvalidIndex(idx)) {
      oob_slot_.ResetToDefault();
      return oob_slot_;
    }
    return data_[static_cast<std::size_t>(idx.ToInt64())];
  }

  [[nodiscard]] auto ElementAt(const PackedArray& idx) const -> const T& {
    if (IsInvalidIndex(idx)) {
      oob_slot_.ResetToDefault();
      return oob_slot_;
    }
    return data_[static_cast<std::size_t>(idx.ToInt64())];
  }

  // LRM 7.10.2.7 / 7.10.2.6: append / prepend a single element.
  auto PushBack(const T& item) -> void {
    data_.push_back(item);
  }
  auto PushFront(const T& item) -> void {
    data_.push_front(item);
  }

  // LRM 7.10.2.4 / 7.10.2.5: remove and return the first / last element. On an
  // empty queue the result is the element type's LRM Table 7-1 default (the
  // canonical seed held by `oob_slot_`) and the queue is left unchanged.
  auto PopFront() -> T {
    if (data_.empty()) {
      oob_slot_.ResetToDefault();
      return oob_slot_;
    }
    T front = std::move(data_.front());
    data_.pop_front();
    return front;
  }
  auto PopBack() -> T {
    if (data_.empty()) {
      oob_slot_.ResetToDefault();
      return oob_slot_;
    }
    T back = std::move(data_.back());
    data_.pop_back();
    return back;
  }

  // LRM 7.10.2.2: insert before `index`. The index is `integer` (4-state) so
  // x/z, negative, or `> size` is detectable and makes the call a no-op;
  // `index == size` is a valid append.
  auto Insert(const PackedArray& index, const T& item) -> void {
    if (index.HasUnknown()) {
      return;
    }
    const auto v = index.ToInt64();
    if (v < 0 || static_cast<std::uint64_t>(v) > data_.size()) {
      return;
    }
    data_.insert(data_.begin() + static_cast<std::ptrdiff_t>(v), item);
  }

  // LRM 7.10.2.3: with no index, clear the queue; with an index, remove that
  // element. An x/z, negative, or `>= size` index is a no-op.
  auto Delete() -> void {
    data_.clear();
  }
  auto Delete(const PackedArray& index) -> void {
    if (IsInvalidIndex(index)) {
      return;
    }
    data_.erase(data_.begin() + static_cast<std::ptrdiff_t>(index.ToInt64()));
  }

 private:
  [[nodiscard]] auto IsInvalidIndex(const PackedArray& idx) const -> bool {
    if (idx.HasUnknown()) {
      return true;
    }
    const auto v = idx.ToInt64();
    return v < 0 || static_cast<std::uint64_t>(v) >=
                        static_cast<std::uint64_t>(data_.size());
  }

  mutable T oob_slot_;
  std::deque<T> data_;
};

// LRM 21.2.1.6 aggregate format. Mirrors `Formatter<DynamicArray<T>>`: walk
// the elements, deferring each to its own `Formatter`. Empty queues compose
// naturally because the loop runs zero times.
template <typename T>
struct Formatter<Queue<T>> {
  static auto Format(const FormatSpec& spec, const Queue<T>& value)
      -> std::string {
    std::string out = "'{";
    for (std::size_t i = 0; i < value.Size(); ++i) {
      if (i != 0) {
        out += ", ";
      }
      out += lyra::value::Format(spec, MakeFormatArg(value.RawAt(i)));
    }
    out += "}";
    return out;
  }
};

static_assert(LyraValue<Queue<PackedArray>>);

}  // namespace lyra::value
