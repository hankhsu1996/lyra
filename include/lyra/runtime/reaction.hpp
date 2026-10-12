#pragma once

#include "lyra/runtime/coroutine.hpp"

namespace lyra::runtime {

// A process body that stops only at the wait at the head of its loop, as two
// bodies: a setup that builds the wait into what it is handed, and a pass that
// runs the loop's statements once and answers whether the wait has still to
// end. Each is handed the object the body belongs to and the wait with their
// types erased, because one runner serves the bodies of every class, and
// restores them itself.
using ReactionPart = bool (*)(void* receiver, void* wait);

// The execution the engine schedules for such a body whose loop runs its
// statements and then stops: the setup once, then a pass each time the wait
// the previous pass parked on ends. The setup answers whether a pass follows
// at once, a body that returned before its loop answering no. The execution
// holds the wait, as an activation holds every wait it stops at, so it ends
// with the execution on every way out of it.
auto EnterReactionRunningFirst(
    void* receiver, ReactionPart setup, ReactionPart pass) -> Coroutine<void>;

// The same for a body whose loop stops first and then runs its statements: the
// setup runs the statements before the loop and parks on the loop's first stop,
// answering whether that stop has still to end, as each pass does after it.
auto EnterReactionStoppingFirst(
    void* receiver, ReactionPart setup, ReactionPart pass) -> Coroutine<void>;

}  // namespace lyra::runtime
