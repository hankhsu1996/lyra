#pragma once

#include <span>
#include <vector>

#include "lyra/value/packed_array.hpp"
#include "lyra/value/runtime_unpacked_array.hpp"
#include "lyra/value/runtime_value.hpp"

// An unpacked memory of any nesting depth, read and rebuilt through the
// coordinates its declaration names. A memory task addresses words in ascending
// address at every dimension, row-major (LRM 21.4.3), and an erased memory
// carries its depth as a run-time fact -- so the bounds the declaration
// supplies are the whole of what drives the walk.
namespace lyra::value {

// The word an element holds. Every memory's element is a packed vector
// (LRM 21.4.1 / 21.5.1) and the front end rejects anything else, so an element
// of another domain is a compiler bug.
[[nodiscard]] auto MemoryWordOf(const RuntimeValue& element)
    -> const PackedArray&;

// The words of `memory` in address order. `dims` is
// `[left0, right0, left1, right1, ...]`, the addressed dimension first and the
// rest describing the leaves each address expands to.
[[nodiscard]] auto MemoryWords(
    const RuntimeUnpackedArray& memory, std::span<const PackedArray> dims)
    -> std::vector<PackedArray>;

// `memory` holding `words` in that same order. The two are inverse, so a word
// a caller leaves as it read it comes back where it was -- which is what keeps
// an address the file does not reach holding what it held (LRM 21.4).
[[nodiscard]] auto MemoryWithWords(
    const RuntimeUnpackedArray& memory, std::span<const PackedArray> dims,
    std::span<const PackedArray> words) -> RuntimeUnpackedArray;

}  // namespace lyra::value
