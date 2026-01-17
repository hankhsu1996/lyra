#pragma once

#include <cstddef>
#include <cstdint>
#include <vector>

#include "lyra/mir/interp/runtime_value.hpp"

namespace lyra::mir::interp {

// INVARIANT: All RuntimeIntegral values have bits above bit_width set to zero.
// Operations maintain this by calling MaskTopWord() on results.

constexpr uint32_t kBitsPerWord = 64;

// Calculate number of 64-bit words needed for a given bit width
auto WordsNeeded(uint32_t bit_width) -> size_t;

// Get a mask for the lower N bits (returns all-ones if N >= 64)
auto GetMask(uint32_t bit_width) -> uint64_t;

// Mask off garbage bits in the top word
void MaskTopWord(std::vector<uint64_t>& words, uint32_t bit_width);

// Debug helper to verify normalization invariant
void AssertNormalized(const RuntimeIntegral& val, uint32_t bit_width);

// Extract single bit (returns false for out-of-range)
auto GetBit(const RuntimeIntegral& val, uint32_t bit_pos, uint32_t bit_width)
    -> bool;

// Set single bit (asserts word exists)
void SetBit(RuntimeIntegral& val, uint32_t bit_pos);

// Get sign bit (bit_width must be > 0)
auto GetSignBit(const RuntimeIntegral& val, uint32_t bit_width) -> bool;

// Normalize to bit_width (ensures value.size() == WordsNeeded and top word
// masked)
auto NormalizeToWidth(const RuntimeIntegral& val, uint32_t bit_width)
    -> RuntimeIntegral;

}  // namespace lyra::mir::interp
