#pragma once

#include <cstddef>
#include <deque>
#include <span>
#include <string>
#include <utility>

#include "lyra/value/format.hpp"

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

 private:
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

}  // namespace lyra::value
