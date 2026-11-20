#pragma once

#include <array>
#include <span>

#include "lyra/support/builtin_fn.hpp"

namespace lyra::support {

// What a construct of the language asks of the type of a value handed to it
// as an operand: the type's side of a protocol whose other side the construct
// defines, as Rust's `for` calls `IntoIterator::into_iter` on what it
// iterates and `+` asks `Add` of its operand, items rustc knows as lang items.
// A type implementing one answers each requirement with its own operation of
// the requirement's name; the construct asks it of every type that does, and
// answers any other operand it takes its own way. Unlike a library trait, no
// code holding a value of the type asks it, so a type's record holds none of
// its items: each is an operation the call naming it builds where it calls it.
struct LanguageTrait {
  std::span<const BuiltinFn> requirements;
};

// A memory's side of a memory task (LRM 21.4, 21.5) and of `$fread` into a
// memory (LRM 21.3.4.4): the memory loaded from the words the task read, and
// its words handed to a dump.
inline constexpr auto kMemoryWordsRequirements =
    std::to_array<BuiltinFn>({BuiltinFn::kLoadWords, BuiltinFn::kStoreWords});
inline constexpr LanguageTrait kMemoryWords{
    .requirements = kMemoryWordsRequirements};

// An actual's side of a DPI-C open array formal (LRM 35.5.6.1): the image the
// foreign side reads filled from the actual, and the actual read back out of
// it. A single packed value, which is the image's one element, implements
// none of it.
inline constexpr auto kOpenArrayImageRequirements =
    std::to_array<BuiltinFn>({BuiltinFn::kToImage, BuiltinFn::kFromImage});
inline constexpr LanguageTrait kOpenArrayImage{
    .requirements = kOpenArrayImageRequirements};

// A random variable's side of a call of `randomize()` (LRM 18.4): the variable
// a reference names, walked part by part for the problem the call is solving.
// A variable of a type implementing none of it is one part, which the library
// names to the problem as its type's record describes it.
inline constexpr auto kRandomWalkRequirements =
    std::to_array<BuiltinFn>({BuiltinFn::kWalkRandom});
inline constexpr LanguageTrait kRandomWalk{
    .requirements = kRandomWalkRequirements};

}  // namespace lyra::support
