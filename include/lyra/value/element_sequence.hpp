#pragma once

#include <cstddef>
#include <span>
#include <vector>

#include "lyra/value/packed_array.hpp"

// The whole-value operations of a container whose elements are a sequence by
// position -- a queue, a dynamic array, a fixed-size array -- written once over
// any such container that answers its element policy, its count, and the
// element at a position.
namespace lyra::value::detail {

// LRM 11.4.5: a run of element comparisons answers in the state class an
// element's own equality does, read off the element default so an empty run
// has no case of its own and a size mismatch answers in the same class.
template <typename Seq>
[[nodiscard]] auto ElementsAreFourState(const Seq& seq) -> bool {
  const auto& elem = seq.Element();
  return elem.Equal(elem.Default(), elem.Default()).IsFourState();
}

// LRM 11.2.2 aggregate equality / 11.4.5: element-wise reduction over
// matching positions. A size mismatch yields 0; matching empties yield 1 (LRM
// is silent on both, matching industry convention). `==` propagates X / Z.
template <typename Seq>
[[nodiscard]] auto SequenceEqual(const Seq& lhs, const Seq& rhs)
    -> PackedArray {
  const bool four_state = ElementsAreFourState(lhs);
  if (lhs.Count() != rhs.Count()) {
    return PackedArray::FromInt(0, 1, false, four_state);
  }
  PackedArray result = PackedArray::FromInt(1, 1, false, four_state);
  for (std::size_t i = 0; i < lhs.Count(); ++i) {
    result = result && lhs.Element().Equal(lhs.At(i), rhs.At(i));
  }
  return result;
}

// LRM 11.4.5 `===`: matches X / Z as values and is deterministic, answering in
// the elements' state class whatever the counts.
template <typename Seq>
[[nodiscard]] auto SequenceCaseEqual(const Seq& lhs, const Seq& rhs)
    -> PackedArray {
  const bool four_state = ElementsAreFourState(lhs);
  if (lhs.Count() != rhs.Count()) {
    return PackedArray::FromInt(0, 1, false, four_state);
  }
  for (std::size_t i = 0; i < lhs.Count(); ++i) {
    if (!static_cast<bool>(lhs.Element().CaseEqual(lhs.At(i), rhs.At(i)))) {
      return PackedArray::FromInt(0, 1, false, four_state);
    }
  }
  return PackedArray::FromInt(1, 1, false, four_state);
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

// LRM 11.4.11: whether the two arms of an ambiguous conditional agree on an
// element, which only an equality known to hold says.
template <typename Elem>
[[nodiscard]] auto ElementsAgree(
    const Elem& elem, const void* lhs, const void* rhs) -> bool {
  return elem.Equal(lhs, rhs).Truth() == Truthiness::kKnownNonzero;
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
[[nodiscard]] auto SequenceBitstreamWidth(const Seq& seq) -> PackedArray {
  PackedArray total = PackedArray::Int(0);
  for (std::size_t i = 0; i < seq.Count(); ++i) {
    total = total + seq.Element().BitstreamWidth(seq.At(i));
  }
  return total;
}
template <typename Seq>
[[nodiscard]] auto SequenceCountBits(
    const Seq& seq, const PackedArray& control_bits) -> PackedArray {
  PackedArray total = PackedArray::Int(0);
  for (std::size_t i = 0; i < seq.Count(); ++i) {
    total = total + seq.Element().CountBits(seq.At(i), control_bits);
  }
  return total;
}

}  // namespace lyra::value::detail
