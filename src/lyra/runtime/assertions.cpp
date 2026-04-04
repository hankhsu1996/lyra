#include "lyra/runtime/assertions.hpp"

#include "lyra/runtime/engine.hpp"

extern "C" void LyraRecordImmediateCoverHit(void* engine, uint32_t site_index) {
  static_cast<lyra::runtime::Engine*>(engine)->RecordImmediateCoverHit(
      site_index);
}
