#pragma once

#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/value/element_policy.hpp"
#include "lyra/value/integral_fwd.hpp"
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

// The whole-value operations of an array whose elements are a sequence by
// position, as the library's template for a fixed-size array states them,
// written once over any array that answers its element policy, its count, and
// the element at a position. What compares or streams one element is handed to
// the operation that does it, since the array holds an element type only for
// what its storage needs.
namespace lyra::value::detail {

// LRM 11.2.2 aggregate equality / 11.4.5: element-wise reduction over
// matching positions, answering 0, 1 or x. A size mismatch yields 0; matching
// empties yield 1 (LRM is silent on both, matching industry convention). `==`
// propagates X / Z.
template <typename Seq, typename Ops>
[[nodiscard]] auto SequenceEqual(const Seq& lhs, const Seq& rhs, const Ops& ops)
    -> FourStateBit {
  if (lhs.Count() != rhs.Count()) {
    return FourStateBit::kZero;
  }
  FourStateBit result = FourStateBit::kOne;
  for (std::size_t i = 0; i < lhs.Count(); ++i) {
    result = LogicalAnd(result, ops.Equal(lhs.At(i), rhs.At(i)));
  }
  return result;
}

// LRM 11.4.5 `===`: matches X / Z as values and is deterministic, so it is
// never unknown.
template <typename Seq, typename Ops>
[[nodiscard]] auto SequenceCaseEqual(
    const Seq& lhs, const Seq& rhs, const Ops& ops) {
  if (lhs.Count() != rhs.Count()) {
    return ops.CaseAnswer(false);
  }
  for (std::size_t i = 0; i < lhs.Count(); ++i) {
    if (!ops.CaseEqual(lhs.At(i), rhs.At(i))) {
      return ops.CaseAnswer(false);
    }
  }
  return ops.CaseAnswer(true);
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
template <typename Seq, typename Ops>
[[nodiscard]] auto SequenceHasUnknown(const Seq& seq, const Ops& ops) -> bool {
  for (std::size_t i = 0; i < seq.Count(); ++i) {
    if (ops.HasUnknown(seq.At(i))) {
      return true;
    }
  }
  return false;
}

// LRM 20.6.2 `$bits` / LRM 20.9 `$countbits`: a container's bit stream is its
// elements' laid end to end, so each sums its elements' own.
template <typename Seq, typename Ops>
[[nodiscard]] auto SequenceBitstreamWidth(const Seq& seq, const Ops& ops)
    -> std::int64_t {
  std::int64_t total = 0;
  for (std::size_t i = 0; i < seq.Count(); ++i) {
    total += ops.BitstreamWidth(seq.At(i));
  }
  return total;
}
template <typename Seq, typename Ops>
[[nodiscard]] auto SequenceCountBits(
    const Seq& seq, const LogicVector<4>& control_bits, const Ops& ops)
    -> std::int64_t {
  std::int64_t total = 0;
  for (std::size_t i = 0; i < seq.Count(); ++i) {
    total += ops.CountBits(seq.At(i), control_bits);
  }
  return total;
}

// LRM 11.4.14.4: the elements `elements` names written end to end into a run
// of bits of their own, the first most significant.
template <typename Ops>
[[nodiscard]] auto StreamOfElements(
    const Ops& ops, std::span<const void* const> elements) -> BitVec {
  std::uint64_t width = 0;
  for (const void* element : elements) {
    width += static_cast<std::uint64_t>(ops.BitstreamWidth(element));
  }
  BitVec run = BitVec::Zeros(width);
  std::uint64_t filled = 0;
  for (const void* element : elements) {
    filled = ops.WriteToStream(element, run.MutableView(), filled);
  }
  return run;
}

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

}  // namespace lyra::value::detail
