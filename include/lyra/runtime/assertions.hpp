#pragma once

#include <cstdint>

// Runtime helpers for immediate assertion support.
// Callable from LLVM-generated code via extern "C" linkage.

extern "C" {

// Record a hit for an immediate cover statement.
// engine: pointer to Engine (cast from void*)
// site_index: design-global cover site index
void LyraRecordImmediateCoverHit(void* engine, uint32_t site_index);

}  // extern "C"
