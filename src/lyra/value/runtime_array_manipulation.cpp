#include "lyra/value/runtime_array_manipulation.hpp"

#include <cstddef>
#include <cstdint>
#include <vector>

#include "lyra/value/any_value.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/runtime_associative_array.hpp"
#include "lyra/value/runtime_dynamic_array.hpp"
#include "lyra/value/runtime_queue.hpp"
#include "lyra/value/runtime_unpacked_array.hpp"
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
    entries.positions.push_back(PackedArray::Int(static_cast<std::int32_t>(i)));
  }
  // Every position is in place before any is pointed at.
  entries.indices.reserve(count);
  for (const PackedArray& position : entries.positions) {
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

auto EntriesOf(const RuntimeUnpackedArray& array) -> ArrayEntries {
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

auto LocatedQueue(
    const ValueType& type, const void* prototype,
    const std::vector<const void*>& places) -> RuntimeQueue {
  return RuntimeQueue::FromElements(type, prototype, places);
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
auto ContainerOf<RuntimeQueue>(
    const std::vector<AnyValue>& elements, const ValueType& type,
    const void* prototype) -> RuntimeQueue {
  return LocatedQueue(type, prototype, PlacesOf(elements));
}

template <>
auto ContainerOf<RuntimeDynamicArray>(
    const std::vector<AnyValue>& elements, const ValueType& type,
    const void* prototype) -> RuntimeDynamicArray {
  return RuntimeDynamicArray::FromElements(type, prototype, PlacesOf(elements));
}

template <>
auto ContainerOf<RuntimeUnpackedArray>(
    const std::vector<AnyValue>& elements, const ValueType& type,
    const void* prototype) -> RuntimeUnpackedArray {
  return {type, prototype, PlacesOf(elements)};
}

}  // namespace detail

auto RuntimeArrayMap(
    const RuntimeAssociativeArray& receiver, const ArrayMethodBody& body,
    const ValueType& type, const void* prototype) -> RuntimeAssociativeArray {
  RuntimeAssociativeArray projected(
      receiver.IndexOrder(), type, prototype, prototype);
  for (const auto& [index, element] : receiver.Entries()) {
    const AnyValue answered = body(element, index->Bytes());
    projected.Store(WitnessedKey::ViewOf(*index), answered.Bytes());
  }
  return projected;
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
