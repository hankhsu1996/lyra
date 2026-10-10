#pragma once

#include <cstddef>
#include <functional>
#include <vector>

#include "lyra/value/any_value.hpp"
#include "lyra/value/array_manipulation.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/runtime_associative_array.hpp"
#include "lyra/value/runtime_dynamic_array.hpp"
#include "lyra/value/runtime_queue.hpp"
#include "lyra/value/runtime_unpacked_array.hpp"
#include "lyra/value/value_type.hpp"

// LRM 7.12 array manipulation over the containers the library holds, running
// the algorithms every container shares. The clause defines one family of
// methods that applies across the unpacked-array containers, and the four
// differ only in what an entry's index is and what container a result shapes
// into, so each method here is one body over the entries a container walks.
//
// A result's element type is the library's to be handed, since it is what the
// `with` expression or the receiver's declaration chose: it arrives as a
// prototype value, where it lies, and the type it is a value of.
namespace lyra::value {

// The body a method runs once per entry, handed the element and its index
// where each lies, answering with the value the `with` expression settles on
// (LRM 7.12.4). It is supplied by the caller, the body being compiled where the
// source was rather than here.
using ArrayMethodBody =
    std::function<AnyValue(const void* item, const void* index)>;

// What the shared algorithms ask of a key a body answered that an `AnyValue`
// does not answer as an operator, found by argument-dependent lookup and
// answered by the key's own type: which of two orders first, whether one holds
// as a locator's condition, and the five reductions.
[[nodiscard]] auto KeyBefore(const AnyValue& a, const AnyValue& b) -> bool;
[[nodiscard]] auto KeyHolds(const AnyValue& condition) -> bool;
[[nodiscard]] auto KeyReduce(
    Reduction reduction, const AnyValue& a, const AnyValue& b) -> AnyValue;

namespace detail {

// A container's entries in its natural order: where each element lies, and
// where its index does -- the key a keyed container stores it under, the
// position itself for every other one, which this holds.
struct ArrayEntries {
  std::vector<const void*> elements;
  std::vector<const void*> indices;
  std::vector<Int> positions;
};

[[nodiscard]] auto EntriesOf(const RuntimeQueue& queue) -> ArrayEntries;
[[nodiscard]] auto EntriesOf(const RuntimeDynamicArray& array) -> ArrayEntries;
[[nodiscard]] auto EntriesOf(const RuntimeUnpackedArray& array) -> ArrayEntries;
[[nodiscard]] auto EntriesOf(const RuntimeAssociativeArray& array)
    -> ArrayEntries;

// What `body` answers for the entry at a position.
[[nodiscard]] inline auto KeyOf(
    const ArrayEntries& entries, const ArrayMethodBody& body) {
  return [&entries, &body](std::size_t position) {
    return body(entries.elements[position], entries.indices[position]);
  };
}

// The queue a located family answers with: copies of what lies at `places`,
// in that order, of `type`, whose default `prototype` is.
[[nodiscard]] auto LocatedQueue(
    const ValueType& type, const void* prototype,
    const std::vector<const void*>& places) -> RuntimeQueue;

// What lies at `positions` among `places`, in that order.
[[nodiscard]] inline auto At(
    const std::vector<const void*>& places,
    const std::vector<std::size_t>& positions) -> std::vector<const void*> {
  std::vector<const void*> found;
  found.reserve(positions.size());
  for (const std::size_t position : positions) {
    found.push_back(places[position]);
  }
  return found;
}

// Where each of `values` lies.
[[nodiscard]] auto PlacesOf(const std::vector<AnyValue>& values)
    -> std::vector<const void*>;

// A container of `Container`'s kind holding copies of `elements`, elements of
// `type` whose default `prototype` is.
template <typename Container>
[[nodiscard]] auto ContainerOf(
    const std::vector<AnyValue>& elements, const ValueType& type,
    const void* prototype) -> Container;
template <>
[[nodiscard]] auto ContainerOf<RuntimeQueue>(
    const std::vector<AnyValue>& elements, const ValueType& type,
    const void* prototype) -> RuntimeQueue;
template <>
[[nodiscard]] auto ContainerOf<RuntimeDynamicArray>(
    const std::vector<AnyValue>& elements, const ValueType& type,
    const void* prototype) -> RuntimeDynamicArray;
template <>
[[nodiscard]] auto ContainerOf<RuntimeUnpackedArray>(
    const std::vector<AnyValue>& elements, const ValueType& type,
    const void* prototype) -> RuntimeUnpackedArray;

template <typename Container>
[[nodiscard]] auto KeysFolded(
    const Container& receiver, const ArrayMethodBody& body,
    const ValueType& type, const void* prototype, Reduction reduction)
    -> AnyValue {
  const ArrayEntries entries = EntriesOf(receiver);
  return Folded(
      entries.elements.size(), KeyOf(entries, body), reduction,
      AnyValue::CopyOf(type, prototype));
}

// A locator's answer: the elements, or the indices, at the positions `locate`
// finds among the entries.
template <typename Container, typename Locate>
[[nodiscard]] auto LocatedElements(
    const Container& receiver, const ArrayMethodBody& body,
    const ValueType& type, const void* prototype, Locate locate)
    -> RuntimeQueue {
  const ArrayEntries entries = EntriesOf(receiver);
  return LocatedQueue(
      type, prototype,
      At(entries.elements,
         locate(entries.elements.size(), KeyOf(entries, body))));
}
template <typename Container, typename Locate>
[[nodiscard]] auto LocatedIndices(
    const Container& receiver, const ArrayMethodBody& body,
    const ValueType& type, const void* prototype, Locate locate)
    -> RuntimeQueue {
  const ArrayEntries entries = EntriesOf(receiver);
  return LocatedQueue(
      type, prototype,
      At(entries.indices,
         locate(entries.elements.size(), KeyOf(entries, body))));
}

}  // namespace detail

// LRM 7.12.3 reduction. `prototype` is the answer for a receiver with no
// entries, which the clause leaves open.
template <typename Container>
[[nodiscard]] auto RuntimeArraySum(
    const Container& receiver, const ArrayMethodBody& body,
    const ValueType& type, const void* prototype) -> AnyValue {
  return detail::KeysFolded(receiver, body, type, prototype, Reduction::kSum);
}
template <typename Container>
[[nodiscard]] auto RuntimeArrayProduct(
    const Container& receiver, const ArrayMethodBody& body,
    const ValueType& type, const void* prototype) -> AnyValue {
  return detail::KeysFolded(
      receiver, body, type, prototype, Reduction::kProduct);
}
template <typename Container>
[[nodiscard]] auto RuntimeArrayAnd(
    const Container& receiver, const ArrayMethodBody& body,
    const ValueType& type, const void* prototype) -> AnyValue {
  return detail::KeysFolded(receiver, body, type, prototype, Reduction::kAnd);
}
template <typename Container>
[[nodiscard]] auto RuntimeArrayOr(
    const Container& receiver, const ArrayMethodBody& body,
    const ValueType& type, const void* prototype) -> AnyValue {
  return detail::KeysFolded(receiver, body, type, prototype, Reduction::kOr);
}
template <typename Container>
[[nodiscard]] auto RuntimeArrayXor(
    const Container& receiver, const ArrayMethodBody& body,
    const ValueType& type, const void* prototype) -> AnyValue {
  return detail::KeysFolded(receiver, body, type, prototype, Reduction::kXor);
}

// LRM 7.12.1 locator family. Each answers with a queue of what it located, in
// the order it was located, of `type`; nothing located is the empty queue. The
// index forms answer with the indices instead of the elements, which for a
// keyed receiver are its own indices.
template <typename Container>
[[nodiscard]] auto RuntimeArrayFind(
    const Container& receiver, const ArrayMethodBody& body,
    const ValueType& type, const void* prototype) -> RuntimeQueue {
  return detail::LocatedElements(
      receiver, body, type, prototype, [](std::size_t count, auto key) {
        return detail::MatchingPositions(count, key);
      });
}
template <typename Container>
[[nodiscard]] auto RuntimeArrayFindIndex(
    const Container& receiver, const ArrayMethodBody& body,
    const ValueType& type, const void* prototype) -> RuntimeQueue {
  return detail::LocatedIndices(
      receiver, body, type, prototype, [](std::size_t count, auto key) {
        return detail::MatchingPositions(count, key);
      });
}
template <typename Container>
[[nodiscard]] auto RuntimeArrayFindFirst(
    const Container& receiver, const ArrayMethodBody& body,
    const ValueType& type, const void* prototype) -> RuntimeQueue {
  return detail::LocatedElements(
      receiver, body, type, prototype, [](std::size_t count, auto key) {
        return detail::FirstMatching(count, key);
      });
}
template <typename Container>
[[nodiscard]] auto RuntimeArrayFindFirstIndex(
    const Container& receiver, const ArrayMethodBody& body,
    const ValueType& type, const void* prototype) -> RuntimeQueue {
  return detail::LocatedIndices(
      receiver, body, type, prototype, [](std::size_t count, auto key) {
        return detail::FirstMatching(count, key);
      });
}
template <typename Container>
[[nodiscard]] auto RuntimeArrayFindLast(
    const Container& receiver, const ArrayMethodBody& body,
    const ValueType& type, const void* prototype) -> RuntimeQueue {
  return detail::LocatedElements(
      receiver, body, type, prototype, [](std::size_t count, auto key) {
        return detail::LastMatching(count, key);
      });
}
template <typename Container>
[[nodiscard]] auto RuntimeArrayFindLastIndex(
    const Container& receiver, const ArrayMethodBody& body,
    const ValueType& type, const void* prototype) -> RuntimeQueue {
  return detail::LocatedIndices(
      receiver, body, type, prototype, [](std::size_t count, auto key) {
        return detail::LastMatching(count, key);
      });
}
template <typename Container>
[[nodiscard]] auto RuntimeArrayMin(
    const Container& receiver, const ArrayMethodBody& body,
    const ValueType& type, const void* prototype) -> RuntimeQueue {
  return detail::LocatedElements(
      receiver, body, type, prototype, [](std::size_t count, auto key) {
        return detail::LeastPosition(count, key);
      });
}
template <typename Container>
[[nodiscard]] auto RuntimeArrayMax(
    const Container& receiver, const ArrayMethodBody& body,
    const ValueType& type, const void* prototype) -> RuntimeQueue {
  return detail::LocatedElements(
      receiver, body, type, prototype, [](std::size_t count, auto key) {
        return detail::GreatestPosition(count, key);
      });
}
template <typename Container>
[[nodiscard]] auto RuntimeArrayUnique(
    const Container& receiver, const ArrayMethodBody& body,
    const ValueType& type, const void* prototype) -> RuntimeQueue {
  return detail::LocatedElements(
      receiver, body, type, prototype, [](std::size_t count, auto key) {
        return detail::UniquePositions(count, key);
      });
}
template <typename Container>
[[nodiscard]] auto RuntimeArrayUniqueIndex(
    const Container& receiver, const ArrayMethodBody& body,
    const ValueType& type, const void* prototype) -> RuntimeQueue {
  return detail::LocatedIndices(
      receiver, body, type, prototype, [](std::size_t count, auto key) {
        return detail::UniquePositions(count, key);
      });
}

// LRM 7.12.5 projection, in entry order, into a container of the receiver's own
// kind: a keyed receiver keeps each entry's index, a sequence one drops it.
// `type` is the element type the `with` expression chose.
template <typename Container>
[[nodiscard]] auto RuntimeArrayMap(
    const Container& receiver, const ArrayMethodBody& body,
    const ValueType& type, const void* prototype) -> Container {
  const detail::ArrayEntries entries = detail::EntriesOf(receiver);
  return detail::ContainerOf<Container>(
      detail::KeysOf(entries.elements.size(), detail::KeyOf(entries, body)),
      type, prototype);
}

// The projected element type answers a miss as well as naming the shape:
// mapping produces no `default:` clause of its own, so the absent-key answer
// is that type's own default. The indices are the receiver's own, so the
// order its index type imposes is the projection's too.
[[nodiscard]] auto RuntimeArrayMap(
    const RuntimeAssociativeArray& receiver, const ArrayMethodBody& body,
    const ValueType& type, const void* prototype) -> RuntimeAssociativeArray;

// LRM 7.12.2 ordering: a positional permutation by the body-projected key,
// applied to the receiver where it lies. It takes no result type, producing no
// element the receiver did not already hold, and the clause defines it on the
// ordinally indexed containers alone.
template <typename Container>
void RuntimeArraySort(Container& receiver, const ArrayMethodBody& body) {
  const detail::ArrayEntries entries = detail::EntriesOf(receiver);
  receiver.Permute(
      detail::SortedPositions(
          detail::KeysOf(entries.elements.size(), detail::KeyOf(entries, body)),
          false));
}
template <typename Container>
void RuntimeArrayRsort(Container& receiver, const ArrayMethodBody& body) {
  const detail::ArrayEntries entries = detail::EntriesOf(receiver);
  receiver.Permute(
      detail::SortedPositions(
          detail::KeysOf(entries.elements.size(), detail::KeyOf(entries, body)),
          true));
}

// LRM 7.12.2 reverse: the receiver in the opposite order. It projects nothing,
// so it runs no body.
template <typename Container>
void RuntimeArrayReverse(Container& receiver) {
  receiver.Permute(detail::ReversedPositions(receiver.Count()));
}

}  // namespace lyra::value
