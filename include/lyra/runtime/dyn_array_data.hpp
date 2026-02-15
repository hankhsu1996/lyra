#pragma once

#include <cstdint>

namespace lyra::runtime {

struct DynArrayData {
  static constexpr uint64_t kMagic = 0x4C59524144594E41;  // "LYRADYNA"
  static constexpr uint64_t kPoison = 0xDEADDEADDEADDEAD;

  uint64_t magic = kMagic;
  uint64_t epoch = 0;
  void* data = nullptr;
  int64_t size = 0;
  int32_t elem_size = 0;
  void (*clone_fn)(void*, const void*) = nullptr;
  void (*destroy_fn)(void*) = nullptr;
};

}  // namespace lyra::runtime
