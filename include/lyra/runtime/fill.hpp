#pragma once

#include <cstdint>

extern "C" {

// Fill a packed array with repeated element values.
// dst_val_plane: pointer to value plane storage (little-endian word array)
// dst_unk_plane: pointer to unknown plane storage (null for 2-state targets)
// total_bits: total target bit width
// src_val_words: element value words (little-endian)
// src_unk_words: element unknown words (null for 2-state sources)
// elem_bits: element bit width
// elem_count: number of elements to fill
void LyraFillPackedElements(
    void* dst_val_plane, void* dst_unk_plane, uint32_t total_bits,
    const uint64_t* src_val_words, const uint64_t* src_unk_words,
    uint32_t elem_bits, uint32_t elem_count);
}
