#pragma once

#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/value/element_policy.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::value {

// LRM 11.4.14.4: how many elements a `with` range names. The range behaves as
// a slice, which names none where it ends before it starts (LRM 7.10.1), and
// the lowering states that as a count of zero, so a negative one is a lowering
// defect rather than a value.
[[nodiscard]] inline auto WindowCount(std::int64_t count) -> std::size_t {
  if (count < 0) {
    throw InternalError("a 'with' range names a negative number of elements");
  }
  return static_cast<std::size_t>(count);
}

}  // namespace lyra::value

// The whole-value operations of a container whose elements are a sequence by
// position -- a queue, a dynamic array, a fixed-size array -- written once over
// any such container that answers its element policy, its count, and the
// element at a position.
namespace lyra::value::detail {

// LRM 11.2.2 aggregate equality / 11.4.5: element-wise reduction over
// matching positions, answering 0, 1 or x. A size mismatch yields 0; matching
// empties yield 1 (LRM is silent on both, matching industry convention). `==`
// propagates X / Z.
template <typename Seq>
[[nodiscard]] auto SequenceEqual(const Seq& lhs, const Seq& rhs)
    -> FourStateBit {
  const auto& elem = lhs.Element();
  if (lhs.Count() != rhs.Count()) {
    return FourStateBit::kZero;
  }
  FourStateBit result = FourStateBit::kOne;
  for (std::size_t i = 0; i < lhs.Count(); ++i) {
    result = LogicalAnd(result, elem.Equal(lhs.At(i), rhs.At(i)));
  }
  return result;
}

// LRM 11.4.5 `===`: matches X / Z as values and is deterministic, so it is
// never unknown.
template <typename Seq>
[[nodiscard]] auto SequenceCaseEqual(const Seq& lhs, const Seq& rhs) {
  const auto& elem = lhs.Element();
  if (lhs.Count() != rhs.Count()) {
    return elem.CaseAnswer(false);
  }
  for (std::size_t i = 0; i < lhs.Count(); ++i) {
    if (!elem.CaseEqual(lhs.At(i), rhs.At(i))) {
      return elem.CaseAnswer(false);
    }
  }
  return elem.CaseAnswer(true);
}

// The address of each element of `unit`, `count` times over (LRM 10.9.1), for
// a container that copies what they name.
template <typename T>
[[nodiscard]] auto ReplicatedAddresses(
    std::span<const T> unit, std::size_t count) -> std::vector<const void*> {
  std::vector<const void*> addresses;
  addresses.reserve(unit.size() * count);
  for (std::size_t i = 0; i < count; ++i) {
    for (const T& item : unit) {
      addresses.push_back(&item);
    }
  }
  return addresses;
}

// The address of each element of an unpacked array read by storage ordinal, in
// order (LRM 7.6).
template <typename C>
[[nodiscard]] auto OrdinalAddresses(const C& source)
    -> std::vector<const void*> {
  std::vector<const void*> addresses;
  addresses.reserve(source.RawSize());
  for (std::size_t i = 0; i < source.RawSize(); ++i) {
    addresses.push_back(&source.RawAt(i));
  }
  return addresses;
}

// LRM 9.4.2 update event predicate: element-wise bit identity, a size
// mismatch being a change.
template <typename Seq>
[[nodiscard]] auto SequenceBitIdentical(const Seq& lhs, const Seq& rhs)
    -> bool {
  if (lhs.Count() != rhs.Count()) {
    return false;
  }
  for (std::size_t i = 0; i < lhs.Count(); ++i) {
    if (!lhs.Element().BitIdentical(lhs.At(i), rhs.At(i))) {
      return false;
    }
  }
  return true;
}

// LRM 20.9: any element carrying an unknown bit propagates up.
template <typename Seq>
[[nodiscard]] auto SequenceHasUnknown(const Seq& seq) -> bool {
  for (std::size_t i = 0; i < seq.Count(); ++i) {
    if (seq.Element().HasUnknown(seq.At(i))) {
      return true;
    }
  }
  return false;
}

// LRM 20.6.2 `$bits` / LRM 20.9 `$countbits`: a container's bit stream is its
// elements' laid end to end, so each sums its elements' own.
template <typename Seq>
[[nodiscard]] auto SequenceBitstreamWidth(const Seq& seq) -> std::int64_t {
  std::int64_t total = 0;
  for (std::size_t i = 0; i < seq.Count(); ++i) {
    total += seq.Element().BitstreamWidth(seq.At(i));
  }
  return total;
}
template <typename Seq, typename Control>
[[nodiscard]] auto SequenceCountBits(
    const Seq& seq, const Control& control_bits) -> std::int64_t {
  std::int64_t total = 0;
  for (std::size_t i = 0; i < seq.Count(); ++i) {
    total += seq.Element().CountBits(seq.At(i), control_bits);
  }
  return total;
}

// LRM 6.24.3: the elements' own streams written into `stream` below its
// `filled` most significant positions, the one at position 0 most significant
// -- the order a `foreach` walks them in (LRM 11.4.14.1). Answers how many are
// filled after them.
template <typename Seq>
auto SequenceWriteToStream(
    const Seq& seq, const IntegralView& stream, std::uint64_t filled)
    -> std::uint64_t {
  for (std::size_t i = 0; i < seq.Count(); ++i) {
    filled = seq.Element().WriteToStream(seq.At(i), stream, filled);
  }
  return filled;
}

// LRM 11.4.14.4: the elements `elements` names written end to end into a run
// of bits of their own, the first most significant.
template <ElementPolicy Elem>
[[nodiscard]] auto StreamOfElements(
    const Elem& elem, std::span<const void* const> elements) -> BitVec {
  std::uint64_t width = 0;
  for (const void* element : elements) {
    width += static_cast<std::uint64_t>(elem.BitstreamWidth(element));
  }
  BitVec run = BitVec::Zeros(width);
  std::uint64_t filled = 0;
  for (const void* element : elements) {
    filled = elem.WriteToStream(element, run.MutableView(), filled);
  }
  return run;
}

// LRM 6.24.3, 11.4.14.4: the elements a stream holds from its `taken` most
// significant position up to `until`, each read in turn into storage of its own
// holding the element default, in the order a `foreach` walks them. An element
// whose type fixes its width takes that many positions, so as many are read as
// fill the run; one whose width the running program decides takes every
// position left, so it is the only one. A run that an element would end past
// does not divide into the elements.
template <ElementPolicy Elem>
class ElementsRead {
 public:
  ElementsRead(
      const Elem& elem, const ConstIntegralView& stream, std::uint64_t taken,
      std::uint64_t until)
      : read_(elem) {
    while (taken < until) {
      void* element = read_.Add(elem.Default());
      taken = elem.ReadFromStream(stream, taken, until, element);
    }
    if (taken != until) {
      throw SimulationError(
          "a stream of bits does not divide into the elements of the array it "
          "is read into");
    }
  }

  // Where each element read lies, in order.
  [[nodiscard]] auto Addresses() const -> std::vector<const void*> {
    return {read_.items.begin(), read_.items.end()};
  }

 private:
  // The storage of each element read, given back however the reading ends.
  struct Owned {
    explicit Owned(const Elem& of) : elem(&of) {
    }
    Owned(const Owned&) = delete;
    auto operator=(const Owned&) -> Owned& = delete;
    Owned(Owned&&) = delete;
    auto operator=(Owned&&) -> Owned& = delete;
    ~Owned() {
      for (void* item : items) {
        FreeElement(*elem, item);
      }
    }

    // Storage of its own holding a copy of `from`, kept here.
    auto Add(const void* from) -> void* {
      items.reserve(items.size() + 1);
      items.push_back(CopyElement(*elem, from));
      return items.back();
    }

    const Elem* elem;
    std::vector<void*> items;
  };

  Owned read_;
};

// LRM 7.4.5 / 7.4.6: the `count` elements of `seq` from `start`, each the
// element default where it lies outside the sequence, every one of them where
// `start` names no position.
template <typename Seq>
[[nodiscard]] auto SequenceSliceElements(
    const Seq& seq, std::optional<std::int64_t> start, std::size_t count)
    -> std::vector<const void*> {
  std::vector<const void*> elements;
  elements.reserve(count);
  const auto size = static_cast<std::int64_t>(seq.Count());
  for (std::size_t i = 0; i < count; ++i) {
    const std::int64_t at =
        start.has_value() ? *start + static_cast<std::int64_t>(i) : -1;
    elements.push_back(
        at >= 0 && at < size ? seq.At(static_cast<std::size_t>(at))
                             : seq.Element().Default());
  }
  return elements;
}

// LRM 7.6: the `count` elements of `seq` from `start` take `replacement`,
// element for element; an element outside the sequence is skipped and a start
// naming no position writes nothing. Answers whether any element took a
// different value (LRM 4.3).
template <typename Seq>
auto SequenceAssignSlice(
    Seq& seq, std::optional<std::int64_t> start, std::size_t count,
    std::span<const void* const> replacement) -> bool {
  if (!start.has_value()) {
    return false;
  }
  const auto size = static_cast<std::int64_t>(seq.Count());
  bool moved = false;
  for (std::size_t i = 0; i < count; ++i) {
    const std::int64_t at = *start + static_cast<std::int64_t>(i);
    if (at < 0 || at >= size) {
      continue;
    }
    void* element = seq.At(static_cast<std::size_t>(at));
    if (seq.Element().BitIdentical(element, replacement[i])) {
      continue;
    }
    seq.Element().Assign(element, replacement[i]);
    moved = true;
  }
  return moved;
}

// LRM 7.12.2: puts the value `held` has at `order[k]` at position `k` of
// `seq`. `held` is a copy of `seq` taken before any element moves, since an
// element is read after the position it came from was written.
template <typename Seq, typename Held>
void SequencePermute(
    Seq& seq, const Held& held, std::span<const std::size_t> order) {
  for (std::size_t k = 0; k < order.size(); ++k) {
    seq.Element().Assign(seq.At(k), held.At(order[k]));
  }
}

}  // namespace lyra::value::detail
