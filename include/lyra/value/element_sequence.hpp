#pragma once

#include <cstddef>
#include <cstdint>
#include <span>
#include <vector>

#include "lyra/value/integral_words.hpp"

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

}  // namespace lyra::value::detail
