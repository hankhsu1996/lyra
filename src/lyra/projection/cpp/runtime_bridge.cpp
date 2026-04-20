#include "lyra/projection/cpp/runtime_bridge.hpp"

namespace lyra::projection::cpp {

auto RunSingleInitial(void* object_ptr, SingleEntryFn entry_fn) -> int {
  entry_fn(object_ptr);
  return 0;
}

}  // namespace lyra::projection::cpp
