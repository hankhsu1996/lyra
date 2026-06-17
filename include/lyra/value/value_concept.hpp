#pragma once

#include <concepts>

namespace lyra::value {

// The contract every Lyra runtime value type satisfies so it can live in the
// STL containers the runtime stores it in (`std::vector` / `std::deque` /
// `std::map`) and survive their relocation (insert / erase / grow). `std::
// copyable` requires move- and copy-construction, move- and copy-assignment,
// and swappability -- and, per the standard library contract it builds on, a
// moved-from object that remains valid for assignment and destruction. That
// moved-from validity is the property that actually bites: a value type whose
// move leaves an unusable husk crashes inside container relocation.
//
// This is a C++-mechanics contract, NOT a statement about assignment meaning.
// For shape-bearing types like `PackedArray`, assignment preserves the
// destination's declared shape and copies the source's bits in (it is not a
// pure value adopt); see docs/decisions/integral-representation.md. The
// concept only guarantees the operations exist and relocation is safe.
template <typename T>
concept LyraValue = std::copyable<T>;

}  // namespace lyra::value
