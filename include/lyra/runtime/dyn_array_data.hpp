#pragma once

#include <cstdint>

namespace lyra::runtime {

struct DynArrayData {
  void* data;
  int64_t size;
  int32_t elem_size;
  void (*clone_fn)(void*, const void*);
  void (*destroy_fn)(void*);
};

}  // namespace lyra::runtime
