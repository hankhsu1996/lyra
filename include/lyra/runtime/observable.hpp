#pragma once

#include "lyra/runtime/intrusive_list.hpp"
#include "lyra/runtime/wait.hpp"

namespace lyra::runtime {

// A place where something happens that activations wait for: a variable cell or
// a net taking a new value (LRM 4.3 calls that an update event), a named event
// being triggered (LRM 15.5.1). The waits enrolled here are the whole of what
// such an occurrence is reported to -- nothing else reads one -- so an
// occurrence where none is enrolled has nothing to report, and may skip the
// work of describing itself. A wait stays enrolled for as long as the frame
// holding it does, parked there or not, so an occurrence where one is enrolled
// is described whether or not anything is parked at it yet.
//
// Deriving from this has to leave the derived cell's own address equal to the
// address of what waits on it: generated code hands a cell's address across a C
// boundary, where the pointer carries no type to adjust by.
class Observable {
 public:
  // Defined in this class's own source file: every cell a unit holds constructs
  // and destroys one, and a definition written here would be compiled again by
  // each such unit.
  Observable();
  Observable(const Observable&) = delete;
  auto operator=(const Observable&) -> Observable& = delete;
  Observable(Observable&&) = delete;
  auto operator=(Observable&&) -> Observable& = delete;
  ~Observable();

  // Written here, and not in the library's own source, because every store
  // asks it and the library's own writes fold it. The price is that a unit
  // writing a cell of a type its design shaped carries a copy of it.
  [[nodiscard]] auto HasMembers() const noexcept -> bool {
    return !members_.Empty();
  }

  // What an occurrence here is reported to: the memberships of the waits
  // enrolled here, each for its awaiter's whole life. Defined in this class's
  // own source file, since only a write that found a wait enrolled asks it.
  [[nodiscard]] auto Members() noexcept -> IntrusiveList<WaitMembership>&;

 private:
  IntrusiveList<WaitMembership> members_;
};

}  // namespace lyra::runtime
