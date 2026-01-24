#pragma once

#include <cstdint>

using LyraStringHandle = void*;

extern "C" {

// Creates a string from a literal. Returns owned handle (+1 refcount).
auto LyraStringFromLiteral(const char* data, int64_t len) -> LyraStringHandle;

// Returns <0 if a < b, 0 if equal, >0 if a > b (memcmp semantics)
auto LyraStringCmp(LyraStringHandle a, LyraStringHandle b) -> int32_t;

// Increment refcount. No-op for null. Returns the same handle.
auto LyraStringRetain(LyraStringHandle handle) -> LyraStringHandle;

// Concatenates N strings. Returns owned handle (+1 refcount). count==0 → empty
// string.
auto LyraStringConcat(const LyraStringHandle* elems, int64_t count)
    -> LyraStringHandle;

// Decrement refcount, free if 0. No-op for null.
void LyraStringRelease(LyraStringHandle handle);
}
