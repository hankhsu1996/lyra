#include "lyra/value/queue_type.hpp"

#include <cstddef>

#include "lyra/base/internal_error.hpp"
#include "lyra/value/container_core.hpp"

namespace lyra::value {

namespace {

auto CoreOf(const void* queue) -> const QueueCore& {
  return *static_cast<const QueueCore*>(queue);
}

auto StorageAt(const QueueCore& core, std::size_t index) -> void* {
  if (index >= core.count) {
    throw InternalError(
        "the element of a queue is asked at an index past its last -- "
        "please report this as a bug");
  }
  return QueueGetUnchecked(core, index);
}

auto QueueOf(const ValueType& type) -> const QueueType& {
  const QueueType* queue = type.AsQueue();
  if (queue == nullptr) {
    throw InternalError(
        "a queue's parts are asked of a type that is none -- please report "
        "this as a bug");
  }
  return *queue;
}

}  // namespace

auto QueueType::Count(const void* queue) -> std::size_t {
  return CoreOf(queue).count;
}

auto QueueType::GetUnchecked(const void* queue, std::size_t index) -> const
    void* {
  return StorageAt(CoreOf(queue), index);
}

auto QueueType::GetUnchecked(void* queue, std::size_t index) -> void* {
  return StorageAt(CoreOf(queue), index);
}

extern "C" {
constinit const PartsWitnessTable lyra_rt_queue_parts_witness_table{
    .count = [](const ValueType&, const void* value) -> std::size_t {
      return QueueType::Count(value);
    },
    .type = [](const ValueType& type, const void*) -> const ValueType& {
      return QueueOf(type).ElementType().Type();
    },
    .at = [](const ValueType&, const void* value, std::size_t position)
        -> const void* { return QueueType::GetUnchecked(value, position); },
    .ref_at = [](const ValueType&, void* value, std::size_t position) -> void* {
      return QueueType::GetUnchecked(value, position);
    }};
}

}  // namespace lyra::value
