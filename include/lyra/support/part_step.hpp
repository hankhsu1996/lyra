#pragma once

#include <cstdint>
#include <utility>

namespace lyra::support {

// What one step of the way into a part of a variable selects: an element of
// an array by its position (LRM 7.4), a member of a structure (LRM 7.2), a
// member of a union, tagged or not (LRM 7.3), a character of a string (LRM
// 6.16), or an entry of an associative array (LRM 7.8).
enum class PartStep : std::uint8_t {
  kElement,
  kComponent,
  kUnionMember,
  kCharacter,
  kAssocElement,
};

// Whether the path naming a part goes on through a step of one kind, a step
// being its number -- an element's position, a member's index -- or stops
// there and stands for the whole of what the step selects from: once for the
// part a wait watches, or a procedure reports it reads or writes, and once for
// the part a write reports it changed.
//
// The two differ only where a write of the part changes more than the part.
// Every member of a union is one storage (LRM 7.3), so a write of a member
// changes the union, while a wait on a member watches that member; a write of
// a character is a write of the string. An entry of an associative array has
// no position (LRM 7.8), so neither path names it. A part's written path is so
// a prefix of its own watched path, and a write, which wakes the waits on what
// it reports, on what that lies in and on what lies in it, reaches every wait
// on a part it can change: a write of `u.a` the wait on `u.b`.
//
// The wait's lowering and the write's steps both read this, so the two cannot
// name one part by different paths.
struct StepFollowing {
  bool watched;
  bool written;
};

[[nodiscard]] constexpr auto FollowingOf(PartStep step) -> StepFollowing {
  switch (step) {
    case PartStep::kElement:
    case PartStep::kComponent:
      return {.watched = true, .written = true};
    case PartStep::kUnionMember:
    case PartStep::kCharacter:
      return {.watched = true, .written = false};
    case PartStep::kAssocElement:
      return {.watched = false, .written = false};
  }
  std::unreachable();
}

}  // namespace lyra::support
