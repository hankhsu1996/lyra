#pragma once

#include <cstdint>

using LyraStringHandle = void*;

extern "C" {

// TODO(hankhsu): Phase 2 - strings are currently leaked
auto LyraStringFromLiteral(const char* data, int64_t len) -> LyraStringHandle;

// Returns <0 if a < b, 0 if equal, >0 if a > b (memcmp semantics)
auto LyraStringCmp(LyraStringHandle a, LyraStringHandle b) -> int32_t;
}
