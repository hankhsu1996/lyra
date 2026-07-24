#pragma once

#include "lyra/value/packed_array.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/unpacked_array.hpp"

namespace lyra::runtime {

class RuntimeEffects;

// LRM 21.4 $readmemh / $readmemb. Loads the memory `dest` from the text file
// named `filename`: whitespace-separated radix-`base` words (base 16 for
// $readmemh, 2 for $readmemb), optional `@hex` address directives, `//` and
// `/* */` comments, and per-digit x / z / ?. `declared_left` / `declared_right`
// are dest's declared index bounds; each word is written by declared index, so
// a descending or non-zero-based memory resolves correctly. Words the file does
// not address keep their prior value. `dest` is the lowering's copy-out temp,
// committed to the SV memory after the call returns.
//
// Addressing (LRM 21.4): the no-range form fills from the lowest declared index
// upward; the `start`-only form fills upward from `start`; the `start`/`finish`
// form fills from `start` toward `finish`, descending when `start > finish`. An
// `@address` in the file repositions the write cursor and must fall inside the
// active range, else the load stops with an error.
void ReadMem(
    RuntimeEffects& runtime, value::UnpackedArray<value::PackedArray>& dest,
    const value::String& filename, const value::PackedArray& declared_left,
    const value::PackedArray& declared_right, const value::PackedArray& base);
void ReadMem(
    RuntimeEffects& runtime, value::UnpackedArray<value::PackedArray>& dest,
    const value::String& filename, const value::PackedArray& declared_left,
    const value::PackedArray& declared_right, const value::PackedArray& base,
    const value::PackedArray& start);
void ReadMem(
    RuntimeEffects& runtime, value::UnpackedArray<value::PackedArray>& dest,
    const value::String& filename, const value::PackedArray& declared_left,
    const value::PackedArray& declared_right, const value::PackedArray& base,
    const value::PackedArray& start, const value::PackedArray& finish);

}  // namespace lyra::runtime
