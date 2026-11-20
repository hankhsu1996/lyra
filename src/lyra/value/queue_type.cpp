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

}  // namespace lyra::value
