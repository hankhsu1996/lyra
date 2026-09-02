#pragma once

#include <cstddef>
#include <cstdint>
#include <functional>
#include <ranges>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/value/array_manipulation.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/runtime_value.hpp"

// LRM 7.12 array manipulation over the erased containers. The clause defines
// one family of methods that applies across the unpacked-array containers, and
// the four differ only in what an entry's index is and what container a result
// shapes into. Erasing the index and the element makes those two the same type
// everywhere, so each method here is one body rather than one per container --
// what the monomorphized containers spell four times, their index types being
// distinct at the C++ type level.
namespace lyra::value {

// The body a method runs once per entry: the element and its index, answering
// with the value the `with` expression settles on (LRM 7.12.4). It is supplied
// by the caller, the body being compiled where the source was rather than here.
using ArrayMethodBody = std::function<RuntimeValue(
    const RuntimeValue& item, const RuntimeValue& index)>;

// LRM 7.12.1 key comparisons over an erased key: which of two orders first, and
// whether two name the same value -- a 4-state key comparing bit-exact, so two
// x-valued keys are the same value while an x never equals a known bit
// (LRM 11.4.5 `===`). The shared algorithms reach these by argument-dependent
// lookup; they are stated here because a key of any domain is only nameable
// once the containers it closes over are complete.
[[nodiscard]] auto LocatorKeyLess(const RuntimeValue& a, const RuntimeValue& b)
    -> bool;
[[nodiscard]] auto LocatorKeySame(const RuntimeValue& a, const RuntimeValue& b)
    -> bool;

namespace detail {

using ErasedEntry = Entry<RuntimeValue, RuntimeValue>;

// The entries of a keyed container, in LRM 7.8 index order: each element paired
// with the index it is stored under, which is what LRM 7.12.4 reports for a
// keyed receiver.
[[nodiscard]] inline auto ErasedEntriesOf(
    const RuntimeAssociativeArray& array) {
  const auto count = static_cast<std::size_t>(array.Size().ToInt64());
  return std::views::iota(std::size_t{0}, count) |
         std::views::transform([&array](std::size_t position) {
           return ErasedEntry{
               array.IndexAt(position), &array.ElementAt(position)};
         });
}

// The entries of an ordinally indexed container: its elements in storage order,
// each paired with the position LRM 7.12.4 reports as its index. Lazy, so an
// index is materialized only for an entry the consumer reaches.
template <EntryWalkable Container>
[[nodiscard]] auto ErasedEntriesOf(const Container& container) {
  const auto count = static_cast<std::size_t>(container.Size().ToInt64());
  return std::views::iota(std::size_t{0}, count) |
         std::views::transform([&container](std::size_t position) {
           return ErasedEntry{
               RuntimeValue{
                   PackedArray::Int(static_cast<std::int32_t>(position))},
               &container.ElementAt(position)};
         });
}

// The truth of a projected value where LRM 7.12.1 reads one as a condition. An
// unknown selects nothing, so it reads false; a projection of another domain is
// one the front end does not admit as a locator condition.
[[nodiscard]] inline auto ErasedTruth(const RuntimeValue& projected) -> bool {
  const auto* bits = std::get_if<PackedArray>(&projected.value);
  if (bits == nullptr) {
    throw InternalError(
        "array manipulation: an LRM 7.12.1 locator condition is integral");
  }
  return static_cast<bool>(*bits);
}

// The condition an LRM 7.12.1 locator applies to each entry.
[[nodiscard]] inline auto ErasedCondition(const ArrayMethodBody& body) {
  return [&body](const RuntimeValue& item, const RuntimeValue& index) {
    return ErasedTruth(body(item, index));
  };
}

// A container of `Container`'s kind holding `elements`, whose element shape is
// `prototype`. The fixed-size array spells its construction as one replication
// of the whole list, the variable-size ones as the list itself; nothing else
// about the three differs where a family shapes its result.
template <typename Container>
[[nodiscard]] auto ContainerOf(
    std::vector<RuntimeValue> elements, RuntimeValue prototype) -> Container {
  if constexpr (std::is_same_v<Container, RuntimeUnpackedArray>) {
    return {std::move(prototype), std::move(elements), 1};
  } else {
    return {std::move(prototype), std::move(elements)};
  }
}

// The queue a located family answers with: what it located, in the order it was
// located, shaped by the result element the producer supplied.
[[nodiscard]] inline auto Located(
    std::vector<RuntimeValue> found, RuntimeValue prototype) -> RuntimeQueue {
  return {std::move(prototype), std::move(found)};
}

// LRM 7.12.3: two projected values combined by the domain's own operation. A
// domain the operation is not defined over is one the front end does not admit
// for that reduction.
template <typename Op>
[[nodiscard]] auto ErasedCombine(
    const RuntimeValue& a, const RuntimeValue& b, Op op) -> RuntimeValue {
  if (a.value.index() != b.value.index()) {
    throw InternalError(
        "array manipulation: an LRM 7.12.3 reduction combines values of one "
        "domain");
  }
  return std::visit(
      [&](const auto& lhs) -> RuntimeValue {
        using T = std::decay_t<decltype(lhs)>;
        const T& rhs = std::get<T>(b.value);
        if constexpr (requires { RuntimeValue{op(lhs, rhs)}; }) {
          return RuntimeValue{op(lhs, rhs)};
        } else {
          throw InternalError(
              "array manipulation: this LRM 7.12.3 reduction is not defined "
              "over the projected value's domain");
        }
      },
      a.value);
}

template <typename Container, typename Op>
[[nodiscard]] auto ErasedReduce(
    const Container& receiver, const ArrayMethodBody& body,
    RuntimeValue prototype, Op op) -> RuntimeValue {
  return ArrayFold(
      ErasedEntriesOf(receiver), std::move(prototype), body,
      [op](const RuntimeValue& a, const RuntimeValue& b) {
        return ErasedCombine(a, b, op);
      });
}

// The receiver's elements copied out, so an ordering can permute them.
template <typename Container>
[[nodiscard]] auto ErasedElementsOf(const Container& receiver)
    -> std::vector<RuntimeValue> {
  return ToVector(
      ErasedEntriesOf(receiver) |
      std::views::transform([](const auto& entry) { return *entry.element; }));
}

template <typename Container, typename Before>
[[nodiscard]] auto ErasedOrder(
    const Container& receiver, const ArrayMethodBody& body, Before before)
    -> Container {
  std::vector<RuntimeValue> elements = ErasedElementsOf(receiver);
  ArraySortByKey(
      elements,
      [&body](const RuntimeValue& item, const PackedArray& position) {
        return body(item, RuntimeValue{position});
      },
      before);
  return ContainerOf<Container>(std::move(elements), receiver.ElementDefault());
}

}  // namespace detail

// LRM 7.12.3 reduction. `prototype` carries the result's shape and is the
// answer for a receiver with no entries, which the clause leaves open.
template <typename Container>
[[nodiscard]] auto RuntimeArraySum(
    const Container& receiver, const ArrayMethodBody& body,
    RuntimeValue prototype) -> RuntimeValue {
  return detail::ErasedReduce(
      receiver, body, std::move(prototype),
      [](const auto& a, const auto& b) -> decltype(a + b) { return a + b; });
}

template <typename Container>
[[nodiscard]] auto RuntimeArrayProduct(
    const Container& receiver, const ArrayMethodBody& body,
    RuntimeValue prototype) -> RuntimeValue {
  return detail::ErasedReduce(
      receiver, body, std::move(prototype),
      [](const auto& a, const auto& b) -> decltype(a * b) { return a * b; });
}

template <typename Container>
[[nodiscard]] auto RuntimeArrayAnd(
    const Container& receiver, const ArrayMethodBody& body,
    RuntimeValue prototype) -> RuntimeValue {
  return detail::ErasedReduce(
      receiver, body, std::move(prototype),
      [](const auto& a, const auto& b) -> decltype(a & b) { return a & b; });
}

template <typename Container>
[[nodiscard]] auto RuntimeArrayOr(
    const Container& receiver, const ArrayMethodBody& body,
    RuntimeValue prototype) -> RuntimeValue {
  return detail::ErasedReduce(
      receiver, body, std::move(prototype),
      [](const auto& a, const auto& b) -> decltype(a | b) { return a | b; });
}

template <typename Container>
[[nodiscard]] auto RuntimeArrayXor(
    const Container& receiver, const ArrayMethodBody& body,
    RuntimeValue prototype) -> RuntimeValue {
  return detail::ErasedReduce(
      receiver, body, std::move(prototype),
      [](const auto& a, const auto& b) -> decltype(a ^ b) { return a ^ b; });
}

// LRM 7.12.1 locator family. Each answers with a queue of what it located, in
// the order it was located, whose element shape is `prototype`; nothing located
// is the empty queue. The index forms answer with the indices instead of the
// elements, which for a keyed receiver are its own indices.
template <typename Container>
[[nodiscard]] auto RuntimeArrayFind(
    const Container& receiver, const ArrayMethodBody& body,
    RuntimeValue prototype) -> RuntimeQueue {
  return detail::Located(
      detail::ArrayFind(
          detail::ErasedEntriesOf(receiver), detail::ErasedCondition(body)),
      std::move(prototype));
}

template <typename Container>
[[nodiscard]] auto RuntimeArrayFindIndex(
    const Container& receiver, const ArrayMethodBody& body,
    RuntimeValue prototype) -> RuntimeQueue {
  return detail::Located(
      detail::ArrayFindIndex(
          detail::ErasedEntriesOf(receiver), detail::ErasedCondition(body)),
      std::move(prototype));
}

template <typename Container>
[[nodiscard]] auto RuntimeArrayFindFirst(
    const Container& receiver, const ArrayMethodBody& body,
    RuntimeValue prototype) -> RuntimeQueue {
  return detail::Located(
      detail::ArrayFindFirst(
          detail::ErasedEntriesOf(receiver), detail::ErasedCondition(body)),
      std::move(prototype));
}

template <typename Container>
[[nodiscard]] auto RuntimeArrayFindFirstIndex(
    const Container& receiver, const ArrayMethodBody& body,
    RuntimeValue prototype) -> RuntimeQueue {
  return detail::Located(
      detail::ArrayFindFirstIndex(
          detail::ErasedEntriesOf(receiver), detail::ErasedCondition(body)),
      std::move(prototype));
}

template <typename Container>
[[nodiscard]] auto RuntimeArrayFindLast(
    const Container& receiver, const ArrayMethodBody& body,
    RuntimeValue prototype) -> RuntimeQueue {
  return detail::Located(
      detail::ArrayFindLast(
          detail::ErasedEntriesOf(receiver), detail::ErasedCondition(body)),
      std::move(prototype));
}

template <typename Container>
[[nodiscard]] auto RuntimeArrayFindLastIndex(
    const Container& receiver, const ArrayMethodBody& body,
    RuntimeValue prototype) -> RuntimeQueue {
  return detail::Located(
      detail::ArrayFindLastIndex(
          detail::ErasedEntriesOf(receiver), detail::ErasedCondition(body)),
      std::move(prototype));
}

template <typename Container>
[[nodiscard]] auto RuntimeArrayMin(
    const Container& receiver, const ArrayMethodBody& body,
    RuntimeValue prototype) -> RuntimeQueue {
  return detail::Located(
      detail::ArrayMin(detail::ErasedEntriesOf(receiver), body),
      std::move(prototype));
}

template <typename Container>
[[nodiscard]] auto RuntimeArrayMax(
    const Container& receiver, const ArrayMethodBody& body,
    RuntimeValue prototype) -> RuntimeQueue {
  return detail::Located(
      detail::ArrayMax(detail::ErasedEntriesOf(receiver), body),
      std::move(prototype));
}

template <typename Container>
[[nodiscard]] auto RuntimeArrayUnique(
    const Container& receiver, const ArrayMethodBody& body,
    RuntimeValue prototype) -> RuntimeQueue {
  return detail::Located(
      detail::ArrayUnique(detail::ErasedEntriesOf(receiver), body),
      std::move(prototype));
}

template <typename Container>
[[nodiscard]] auto RuntimeArrayUniqueIndex(
    const Container& receiver, const ArrayMethodBody& body,
    RuntimeValue prototype) -> RuntimeQueue {
  return detail::Located(
      detail::ArrayUniqueIndex(detail::ErasedEntriesOf(receiver), body),
      std::move(prototype));
}

// LRM 7.12.5 projection, in entry order, into a container of the receiver's own
// kind: a keyed receiver keeps each entry's index, a sequence one drops it.
// `prototype` is the element shape the `with` expression chose.
template <typename Container>
[[nodiscard]] auto RuntimeArrayMap(
    const Container& receiver, const ArrayMethodBody& body,
    RuntimeValue prototype) -> Container {
  return detail::ContainerOf<Container>(
      detail::ArrayMap(detail::ErasedEntriesOf(receiver), body),
      std::move(prototype));
}

[[nodiscard]] inline auto RuntimeArrayMap(
    const RuntimeAssociativeArray& receiver, const ArrayMethodBody& body,
    RuntimeValue prototype) -> RuntimeAssociativeArray {
  RuntimeAssociativeArray projected(std::move(prototype));
  for (const detail::ErasedEntry& entry : detail::ErasedEntriesOf(receiver)) {
    projected =
        projected.WithElement(entry.index, body(*entry.element, entry.index));
  }
  return projected;
}

// LRM 7.12.2 ordering: a positional permutation by the body-projected key,
// answering with the receiver reordered. It takes no result shape, producing no
// element the receiver did not already hold, and the clause defines it on the
// ordinally indexed containers alone.
template <typename Container>
[[nodiscard]] auto RuntimeArraySort(
    const Container& receiver, const ArrayMethodBody& body) -> Container {
  return detail::ErasedOrder(receiver, body, LocatorKeyLess);
}

template <typename Container>
[[nodiscard]] auto RuntimeArrayRsort(
    const Container& receiver, const ArrayMethodBody& body) -> Container {
  return detail::ErasedOrder(
      receiver, body, [](const RuntimeValue& a, const RuntimeValue& b) {
        return LocatorKeyLess(b, a);
      });
}

// LRM 7.12.2 reverse: the receiver in the opposite order. It projects nothing,
// so it runs no body.
template <typename Container>
[[nodiscard]] auto RuntimeArrayReverse(const Container& receiver) -> Container {
  std::vector<RuntimeValue> elements = detail::ErasedElementsOf(receiver);
  detail::ArrayReverse(elements);
  return detail::ContainerOf<Container>(
      std::move(elements), receiver.ElementDefault());
}

}  // namespace lyra::value
