#include "lyra/runtime/observable.hpp"

#include <cstdint>

#include "lyra/runtime/trigger.hpp"

namespace lyra::runtime {

Observable::Observable() = default;
Observable::~Observable() = default;

auto MakeWholeValueProjectionTest() -> ProjectionUnchanged {
  return [](std::uint64_t, std::uint64_t) -> bool { return false; };
}

}  // namespace lyra::runtime
