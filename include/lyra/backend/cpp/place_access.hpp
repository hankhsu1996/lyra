#pragma once

#include <string>
#include <string_view>

#include "lyra/mir/type.hpp"

namespace lyra::backend::cpp {

// Reaching the storage a capability wrapper stands for, in C++:
//
//   read  `sig`      ->  (self->sig).Get()
//   write `sig = v`  ->  (*(self->sig).Mutate(current_runtime())) = v
//
// `self->sig` is the wrapper, rendered elsewhere. These two supply only the
// step past it, and the wrapper's type is what picks that step -- one dispatch
// per MIR type variant, sibling to type mapping. It is the only entry allowed
// to spell a runtime library's access protocol, so no emitter has to ask which
// kind of wrapper it is looking at.
//
// A caller does not pick between the two. An operand position needs the value;
// a store target or a by-reference argument needs the lvalue. MIR already says
// which position the place sits in.

// `wrapper` -> the value it holds.
[[nodiscard]] auto RenderLoadThrough(
    const mir::Type& wrapper_type, std::string_view wrapper) -> std::string;

// `wrapper` -> the same storage as an assignable lvalue. A store writes into
// it; a by-reference argument binds to it. Where a write fires an update event,
// the handle this yields publishes once, when the full expression ends -- which
// is why a whole store, a partial store, and an argument all take this one
// form.
[[nodiscard]] auto RenderLendThrough(
    const mir::Type& wrapper_type, std::string_view wrapper) -> std::string;

}  // namespace lyra::backend::cpp
