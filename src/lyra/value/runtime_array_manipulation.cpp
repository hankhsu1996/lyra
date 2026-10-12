#include "lyra/value/runtime_array_manipulation.hpp"

#include <cstddef>
#include <cstdint>
#include <vector>

#include "lyra/value/any_value.hpp"
#include "lyra/value/element_type.hpp"
#include "lyra/value/fixed_array_type.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/queue_type.hpp"
#include "lyra/value/runtime_associative_array.hpp"
#include "lyra/value/runtime_dynamic_array.hpp"
#include "lyra/value/runtime_fixed_array.hpp"
#include "lyra/value/runtime_queue.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::value {

namespace detail {

namespace {

// The entries of a container walked by position, whose index is the position.
template <typename Ordered>
auto OrdinalEntries(const Ordered& container) -> ArrayEntries {
  const std::size_t count = container.Count();
  ArrayEntries entries;
  entries.elements.reserve(count);
  entries.positions.reserve(count);
  for (std::size_t i = 0; i < count; ++i) {
    entries.elements.push_back(container.ElementAt(i));
    entries.positions.push_back(Int::FromInt(static_cast<std::int64_t>(i)));
  }
  // Every position is in place before any is pointed at.
  entries.indices.reserve(count);
  for (const Int& position : entries.positions) {
    entries.indices.push_back(&position);
  }
  return entries;
}

}  // namespace

auto EntriesOf(const RuntimeQueue& queue) -> ArrayEntries {
  return OrdinalEntries(queue);
}

auto EntriesOf(const RuntimeDynamicArray& array) -> ArrayEntries {
  return OrdinalEntries(array);
}

auto EntriesOf(const FixedArrayView& array) -> ArrayEntries {
  return OrdinalEntries(array);
}

auto EntriesOf(const RuntimeAssociativeArray& array) -> ArrayEntries {
  ArrayEntries entries;
  for (const auto& [index, element] : array.Entries()) {
    entries.elements.push_back(element);
    entries.indices.push_back(index->Bytes());
  }
  return entries;
}

auto PlacesOf(const std::vector<AnyValue>& values) -> std::vector<const void*> {
  std::vector<const void*> places;
  places.reserve(values.size());
  for (const AnyValue& value : values) {
    places.push_back(value.Bytes());
  }
  return places;
}

template <>
auto ContainerOf<RuntimeQueue, QueueType>(
    const std::vector<AnyValue>& elements, const QueueType& answer)
    -> RuntimeQueue {
  return RuntimeQueue::FromElements(answer, PlacesOf(elements));
}

template <>
auto ContainerOf<RuntimeDynamicArray, ElementType>(
    const std::vector<AnyValue>& elements, const ElementType& answer)
    -> RuntimeDynamicArray {
  return RuntimeDynamicArray::FromElements(answer, PlacesOf(elements));
}

}  // namespace detail

auto RuntimeArrayMap(
    const FixedArrayView& receiver, const ArrayMethodBody& body,
    const FixedArrayType& answer_type) -> RuntimeFixedArray {
  const detail::ArrayEntries entries = detail::EntriesOf(receiver);
  return RuntimeFixedArray::FromElements(
      answer_type,
      detail::PlacesOf(
          detail::KeysOf(
              entries.elements.size(), detail::KeyOf(entries, body))));
}

auto RuntimeArrayMap(
    const RuntimeAssociativeArray& receiver, const ArrayMethodBody& body,
    const ElementType& answer_element) -> RuntimeAssociativeArray {
  RuntimeAssociativeArray projected(receiver.IndexOrder(), answer_element);
  for (const auto& [index, element] : receiver.Entries()) {
    const AnyValue answered = body(element, index->Bytes());
    projected.Store(WitnessedKey::ViewOf(*index), answered.Bytes());
  }
  return projected;
}

namespace {

// Every key is projected before any element moves, and the permutation the
// keys settle is applied to the elements where they lie.
void SortWhereItLies(
    const FixedArrayType& type, void* array, const ArrayMethodBody& body,
    bool descending) {
  const detail::ArrayEntries entries =
      detail::EntriesOf(FixedArrayView{.type = &type, .bytes = array});
  type.Permute(
      array,
      detail::SortedPositions(
          detail::KeysOf(entries.elements.size(), detail::KeyOf(entries, body)),
          descending));
}

}  // namespace

void RuntimeArraySort(
    const FixedArrayType& type, void* array, const ArrayMethodBody& body) {
  SortWhereItLies(type, array, body, false);
}

void RuntimeArrayRsort(
    const FixedArrayType& type, void* array, const ArrayMethodBody& body) {
  SortWhereItLies(type, array, body, true);
}

void RuntimeArrayReverse(const FixedArrayType& type, void* array) {
  type.Permute(array, detail::ReversedPositions(type.Count()));
}

auto KeyBefore(const AnyValue& a, const AnyValue& b) -> bool {
  return a.Type().OrderBefore(a.Bytes(), b.Bytes());
}

auto KeyHolds(const AnyValue& condition) -> bool {
  return condition.Type().IsTrue(condition.Bytes());
}

auto KeyReduce(Reduction reduction, const AnyValue& a, const AnyValue& b)
    -> AnyValue {
  const ValueType& type = a.Type();
  return AnyValue::Built(type, [&](void* out) {
    type.Reduce(reduction, a.Bytes(), b.Bytes(), out);
  });
}

}  // namespace lyra::value
