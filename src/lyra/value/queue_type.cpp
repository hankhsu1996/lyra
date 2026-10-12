#include "lyra/value/queue_type.hpp"

#include "lyra/value/value_type.hpp"

namespace lyra::value {

auto QueueType::ElementStatedAt() const -> StatedAt {
  return StatedAtIn(this, element_);
}

auto QueueType::BoundStatedAt() const -> StatedAt {
  return StatedAtIn(this, bound_);
}

}  // namespace lyra::value
