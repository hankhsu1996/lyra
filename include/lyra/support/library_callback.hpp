#pragma once

#include <cstdint>
#include <string_view>

namespace lyra::support {

// A body the library calls on an object of a class extending one of its
// classes, which it finds through the object's definition: the three phases
// the runtime drives every scope through, in the order it enters them -- every
// route and alias is bound, then every cell takes the value its declaration
// gives it (LRM 10.5), then every process is created (LRM 9.2) -- and what an
// instance describes to a value dump starting (LRM 21.7.1.2); and what
// `randomize()` asks of every source class (LRM 18.6): the random variables it
// declares, the constraints it states, the random objects its handles reach,
// and the hooks before and after (LRM 18.6.2).
enum class LibraryCallback : std::uint8_t {
  kScopeResolve,
  kScopeInitialize,
  kScopeCreateProcesses,
  kScopeDescribe,
  kClassRandVariables,
  kClassConstrain,
  kClassRandObjects,
  kClassPreRandomize,
  kClassPostRandomize,
};

// The name a body answering it is spelled with. Every one starts with `sv_`,
// which a name the source writes never does once it reaches C++, so no
// subroutine of a design can answer one by having its name.
auto LibraryCallbackName(LibraryCallback callback) -> std::string_view;

}  // namespace lyra::support
