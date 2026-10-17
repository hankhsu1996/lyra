#pragma once

#include <cstddef>
#include <functional>
#include <vector>

#include "lyra/value/any_value.hpp"
#include "lyra/value/array_manipulation.hpp"
#include "lyra/value/element_type.hpp"
#include "lyra/value/fixed_array_type.hpp"
#include "lyra/value/fixed_array_value.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/queue_type.hpp"
#include "lyra/value/runtime_associative_array.hpp"
#include "lyra/value/runtime_dynamic_array.hpp"
#include "lyra/value/runtime_queue.hpp"
#include "lyra/value/value_type.hpp"

// LRM 7.12 array manipulation over the containers the library holds, running
// the algorithms every container shares. The clause defines one family of
// methods that applies across the unpacked-array containers, and the four
// differ only in what an entry's index is and what container a result shapes
// into, so each method here is one body over the entries a container walks.
//
// The type of what a method builds is the library's to be handed, since it is
// what the `with` expression or the receiver's declaration chose: the type of
// a queue a locator builds, the element type of an array a projection builds,
// and the type a reduction answers, whose default is its answer for a
// receiver with no entries.
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
[[nodiscard]] auto EntriesOf(const FixedArrayView& array) -> ArrayEntries;
[[nodiscard]] auto EntriesOf(const RuntimeAssociativeArray& array)
    -> ArrayEntries;

// What `body` answers for the entry at a position.
[[nodiscard]] inline auto KeyOf(
    const ArrayEntries& entries, const ArrayMethodBody& body) {
  return [&entries, &body](std::size_t position) {
    return body(entries.elements[position], entries.indices[position]);
  };
}

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

// A container of the kind `answer` is the type of, holding copies of
// `elements`: a queue of a queue type, a dynamic array of an element type.
template <typename Container, typename Answer>
[[nodiscard]] auto ContainerOf(
    const std::vector<AnyValue>& elements, const Answer& answer) -> Container;
template <>
[[nodiscard]] auto ContainerOf<RuntimeQueue, QueueType>(
    const std::vector<AnyValue>& elements, const QueueType& answer)
    -> RuntimeQueue;
template <>
[[nodiscard]] auto ContainerOf<RuntimeDynamicArray, ElementType>(
    const std::vector<AnyValue>& elements, const ElementType& answer)
    -> RuntimeDynamicArray;

template <typename Container>
[[nodiscard]] auto KeysFolded(
    const Container& receiver, const ArrayMethodBody& body,
    const ValueType& answer_type, Reduction reduction) -> AnyValue {
  const ArrayEntries entries = EntriesOf(receiver);
  return Folded(
      entries.elements.size(), KeyOf(entries, body), reduction,
      AnyValue::Built(
          answer_type, [&](void* out) { answer_type.BuildDefault(out); }));
}

// A locator's answer: the elements, or the indices, at the positions `locate`
// finds among the entries.
template <typename Container, typename Locate>
[[nodiscard]] auto LocatedElements(
    const Container& receiver, const ArrayMethodBody& body,
    const QueueType& answer_type, Locate locate) -> RuntimeQueue {
  const ArrayEntries entries = EntriesOf(receiver);
  return RuntimeQueue::FromElements(
      answer_type, At(entries.elements,
                      locate(entries.elements.size(), KeyOf(entries, body))));
}
template <typename Container, typename Locate>
[[nodiscard]] auto LocatedIndices(
    const Container& receiver, const ArrayMethodBody& body,
    const QueueType& answer_type, Locate locate) -> RuntimeQueue {
  const ArrayEntries entries = EntriesOf(receiver);
  return RuntimeQueue::FromElements(
      answer_type, At(entries.indices,
                      locate(entries.elements.size(), KeyOf(entries, body))));
}

}  // namespace detail

// LRM 7.12.3 reduction, answering a value of `answer_type`. The clause states
// no answer for a receiver with no entries, which answers that type's default.
template <typename Container>
[[nodiscard]] auto RuntimeArraySum(
    const Container& receiver, const ArrayMethodBody& body,
    const ValueType& answer_type) -> AnyValue {
  return detail::KeysFolded(receiver, body, answer_type, Reduction::kSum);
}
template <typename Container>
[[nodiscard]] auto RuntimeArrayProduct(
    const Container& receiver, const ArrayMethodBody& body,
    const ValueType& answer_type) -> AnyValue {
  return detail::KeysFolded(receiver, body, answer_type, Reduction::kProduct);
}
template <typename Container>
[[nodiscard]] auto RuntimeArrayAnd(
    const Container& receiver, const ArrayMethodBody& body,
    const ValueType& answer_type) -> AnyValue {
  return detail::KeysFolded(receiver, body, answer_type, Reduction::kAnd);
}
template <typename Container>
[[nodiscard]] auto RuntimeArrayOr(
    const Container& receiver, const ArrayMethodBody& body,
    const ValueType& answer_type) -> AnyValue {
  return detail::KeysFolded(receiver, body, answer_type, Reduction::kOr);
}
template <typename Container>
[[nodiscard]] auto RuntimeArrayXor(
    const Container& receiver, const ArrayMethodBody& body,
    const ValueType& answer_type) -> AnyValue {
  return detail::KeysFolded(receiver, body, answer_type, Reduction::kXor);
}

// LRM 7.12.1 locator family. Each answers with a queue of `answer_type`
// holding what it located, in the order it was located; nothing located is the
// empty queue. The index forms answer with the indices instead of the
// elements, which for a keyed receiver are its own indices.
template <typename Container>
[[nodiscard]] auto RuntimeArrayFind(
    const Container& receiver, const ArrayMethodBody& body,
    const QueueType& answer_type) -> RuntimeQueue {
  return detail::LocatedElements(
      receiver, body, answer_type, [](std::size_t count, auto key) {
        return detail::MatchingPositions(count, key);
      });
}
template <typename Container>
[[nodiscard]] auto RuntimeArrayFindIndex(
    const Container& receiver, const ArrayMethodBody& body,
    const QueueType& answer_type) -> RuntimeQueue {
  return detail::LocatedIndices(
      receiver, body, answer_type, [](std::size_t count, auto key) {
        return detail::MatchingPositions(count, key);
      });
}
template <typename Container>
[[nodiscard]] auto RuntimeArrayFindFirst(
    const Container& receiver, const ArrayMethodBody& body,
    const QueueType& answer_type) -> RuntimeQueue {
  return detail::LocatedElements(
      receiver, body, answer_type, [](std::size_t count, auto key) {
        return detail::FirstMatching(count, key);
      });
}
template <typename Container>
[[nodiscard]] auto RuntimeArrayFindFirstIndex(
    const Container& receiver, const ArrayMethodBody& body,
    const QueueType& answer_type) -> RuntimeQueue {
  return detail::LocatedIndices(
      receiver, body, answer_type, [](std::size_t count, auto key) {
        return detail::FirstMatching(count, key);
      });
}
template <typename Container>
[[nodiscard]] auto RuntimeArrayFindLast(
    const Container& receiver, const ArrayMethodBody& body,
    const QueueType& answer_type) -> RuntimeQueue {
  return detail::LocatedElements(
      receiver, body, answer_type, [](std::size_t count, auto key) {
        return detail::LastMatching(count, key);
      });
}
template <typename Container>
[[nodiscard]] auto RuntimeArrayFindLastIndex(
    const Container& receiver, const ArrayMethodBody& body,
    const QueueType& answer_type) -> RuntimeQueue {
  return detail::LocatedIndices(
      receiver, body, answer_type, [](std::size_t count, auto key) {
        return detail::LastMatching(count, key);
      });
}
template <typename Container>
[[nodiscard]] auto RuntimeArrayMin(
    const Container& receiver, const ArrayMethodBody& body,
    const QueueType& answer_type) -> RuntimeQueue {
  return detail::LocatedElements(
      receiver, body, answer_type, [](std::size_t count, auto key) {
        return detail::LeastPosition(count, key);
      });
}
template <typename Container>
[[nodiscard]] auto RuntimeArrayMax(
    const Container& receiver, const ArrayMethodBody& body,
    const QueueType& answer_type) -> RuntimeQueue {
  return detail::LocatedElements(
      receiver, body, answer_type, [](std::size_t count, auto key) {
        return detail::GreatestPosition(count, key);
      });
}
template <typename Container>
[[nodiscard]] auto RuntimeArrayUnique(
    const Container& receiver, const ArrayMethodBody& body,
    const QueueType& answer_type) -> RuntimeQueue {
  return detail::LocatedElements(
      receiver, body, answer_type, [](std::size_t count, auto key) {
        return detail::UniquePositions(count, key);
      });
}
template <typename Container>
[[nodiscard]] auto RuntimeArrayUniqueIndex(
    const Container& receiver, const ArrayMethodBody& body,
    const QueueType& answer_type) -> RuntimeQueue {
  return detail::LocatedIndices(
      receiver, body, answer_type, [](std::size_t count, auto key) {
        return detail::UniquePositions(count, key);
      });
}

// LRM 7.12.5 projection, in entry order, into a container of the receiver's own
// kind: a keyed receiver keeps each entry's index, a sequence one drops it.
// `answer` is the type the projection builds, over the element type the `with`
// expression chose: a queue's own type, a dynamic array's element type.
template <typename Container, typename Answer>
[[nodiscard]] auto RuntimeArrayMap(
    const Container& receiver, const ArrayMethodBody& body,
    const Answer& answer) -> Container {
  const detail::ArrayEntries entries = detail::EntriesOf(receiver);
  return detail::ContainerOf<Container>(
      detail::KeysOf(entries.elements.size(), detail::KeyOf(entries, body)),
      answer);
}

// The same over a fixed-size unpacked array, whose projection is an array of
// `answer_type`: as many elements, of the type the `with` expression chose.
[[nodiscard]] auto RuntimeArrayMap(
    const FixedArrayView& receiver, const ArrayMethodBody& body,
    const FixedArrayType& answer_type) -> LaidOutValue;

// Mapping produces no `default:` clause of its own, so an absent key of the
// projection reads the projected element type's default. The indices are the
// receiver's own, so the order its index type imposes is the projection's too.
[[nodiscard]] auto RuntimeArrayMap(
    const RuntimeAssociativeArray& receiver, const ArrayMethodBody& body,
    const ElementType& answer_element) -> RuntimeAssociativeArray;

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

// The same three over a fixed-size unpacked array of `type` where it lies at
// `array`, which is its elements and no object a method could be asked of.
void RuntimeArraySort(
    const FixedArrayType& type, void* array, const ArrayMethodBody& body);
void RuntimeArrayRsort(
    const FixedArrayType& type, void* array, const ArrayMethodBody& body);
void RuntimeArrayReverse(const FixedArrayType& type, void* array);

}  // namespace lyra::value
