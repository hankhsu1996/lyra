#pragma once

#include <coroutine>

namespace lyra::runtime {

// Giving up control after a call has already arranged this execution's
// resumption. The call is the whole of the wait -- what it is waiting for, and
// the enrolment -- so nothing is left here but the suspension itself, and only
// where the call says it parked: one whose wait was already satisfied carries
// on without ever leaving.
//
// It carries no construct. A delay, an event control, a level wait and a join
// reach the same object, because what differs between them was settled by the
// call that ran before it. That is what makes this a spelling rather than a
// mechanism: a target whose suspension is a control edge in its own graph needs
// no counterpart, since the edge is this same nothing.
//
// Coming back is where an effect arises: a target this execution is inside can
// be disabled only while the execution is away (LRM 9.6.2). An execution that
// never left cannot have had one land under it, which is why the question is
// asked only where control was actually given up.
//
// Its members are the library's: every wait a unit states suspends through one,
// and a definition written here would be compiled again by each such unit.
class Suspension {
 public:
  explicit Suspension(bool parked);

  [[nodiscard]] auto await_ready() const noexcept -> bool;

  static void await_suspend(std::coroutine_handle<> frame) noexcept;

  void await_resume() const;

 private:
  bool parked_;
};

}  // namespace lyra::runtime
