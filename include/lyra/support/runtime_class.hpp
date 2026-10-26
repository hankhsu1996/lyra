#pragma once

#include <cstdint>
#include <string_view>

namespace lyra::support {

// A class the runtime library defines whose objects generated code reaches but
// never lays out: the scope every object of the design hierarchy is (LRM 23.3),
// the part every object the program builds starts with, the part every object
// of a source class starts with beyond it -- its own generator (LRM 8.3,
// 18.14.1) -- the object a process is (LRM 9.7), the synchronization object
// an event names (LRM 6.17), and the problem a call of `randomize()` fills
// (LRM 18.6). It belongs to no compilation unit, so which one it
// is is the whole of its identity, and how a target spells it is that target's
// own answer. Where a class of the source extends one, generated code places
// the class's own storage after it and nothing inside it.
//
// Two layers name it -- a type of each stands for a value of one -- so it lives
// beside them rather than in either.
enum class RuntimeClass : std::uint8_t {
  kScope,
  kObject,
  kClassObject,
  kProcess,
  kSynchronizationObject,
  kConstraintProblem,
};

// What a dump calls it.
auto RuntimeClassName(RuntimeClass klass) -> std::string_view;

// Whether an object of `klass` is one of the root every object the library
// holds is, which starts with its class's definition and is ended through it,
// whatever class the one holding it names.
auto IsObjectOfTheRoot(RuntimeClass klass) -> bool;

// A body the library calls on an object of a class extending one of those
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
