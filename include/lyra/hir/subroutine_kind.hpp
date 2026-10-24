#pragma once

#include <cstdint>

namespace lyra::hir {

// LRM 13.1: a subroutine is a function or a task. The distinction is the call
// protocol -- a task enable may consume time and suspends its caller until
// completion (LRM 13.3), a function completes in zero time -- so it is read at
// the call site to decide whether the enable awaits. A constraint block (LRM
// 18.5) is a member of its class called by the same protocol a method is, and
// overridden by name as one (LRM 18.5.2), but only the randomization of an
// object of the class calls it, handing it the problem it states its
// constraints to. Whether one applies at a call is its mode (LRM 18.9), which
// is the object's for a block and the class's for a static one (LRM 18.5.10);
// a static block constrains the object it is called on all the same.
enum class SubroutineKind : std::uint8_t {
  kTask,
  kFunction,
  kConstraintBlock,
  kStaticConstraintBlock,
};

}  // namespace lyra::hir
