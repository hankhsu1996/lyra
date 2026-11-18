#pragma once

#include <memory>
#include <vector>

#include "lyra/runtime/intrusive_list.hpp"
#include "lyra/runtime/kept_original.hpp"
#include "lyra/runtime/part_path.hpp"
#include "lyra/runtime/wait.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::runtime {

class RuntimeEffects;

// A place where something happens that activations wait for: a variable cell or
// a net taking a new value (LRM 4.3 calls that an update event), a named event
// being triggered (LRM 15.5.1). The waits enrolled here are the whole of what
// such an occurrence is reported to -- nothing else reads one -- so an
// occurrence where none is enrolled has nothing to report, and may skip the
// work of describing itself. A wait stays enrolled for as long as the frame
// holding it does, parked there or not, so an occurrence where one is enrolled
// is described whether or not anything is parked at it yet.
//
// A wait is enrolled at the part of the place it watches (LRM 9.2.2.2.1 has a
// procedure's list hold parts of a variable, and 9.2.2.2 has two procedures
// assign two of them). The place keeps those waits as a tree shaped like its
// parts: the waits on the whole are at the root, and the waits on a part are
// at the node the part's path leads to, each node made when a wait first
// needs it. A write then tells the waits at the node it wrote, at every node
// above it and at every node below it, and visits no other.
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
    return !members_.Empty() || children_ != nullptr;
  }

  // Enrols `membership` at the part `path` leads to, the whole of this place
  // where the path is empty, for its awaiter's whole life. The node is made if
  // no wait has needed it yet. The membership of an `always_comb` or
  // `always_latch` has the place keep, from then on, what the first write of
  // each event's execution to a part of it changes.
  void Enrol(const PartPath& path, WaitMembership& membership);

  // Tells the waits a write to the part at `written` reached, `change` being
  // what the write did to that part. The waits on the part itself are told the
  // change. The waits on each part holding it are told which of that part's
  // elements or members the write went into, so one watching only others is
  // passed over. The waits on the parts inside it are told they were reached,
  // those inside elements the change shows untouched excepted. No other wait
  // of the place is visited. Where the path is inexact the write was at or
  // inside the part, so every wait on it and inside it is told only that.
  // `kept` says whether what the place kept holds all the write changed, as
  // it was before the event's execution, which is how a wait triggered by
  // what an execution leaves is judged once it ends.
  void WakeAffectedBy(
      RuntimeEffects& effects, const PartPath& written, const Change& change,
      bool kept);

  // Tells the waits on the part at `at`, on each part holding it and on each
  // part inside it that an event's execution left the part changed, `change`
  // being what became of it measured from what it held before the execution.
  void TellWhatWasLeft(
      RuntimeEffects& effects, const PartPath& at, const Change& change);

  // Whether an `always_comb` or `always_latch` watches the place, which is
  // when the first write of an event's execution to a part keeps what the part
  // held before it.
  // Written here because every watched write asks it.
  [[nodiscard]] auto WatchedCombinationally() const noexcept -> bool {
    return combinational_ != nullptr;
  }

  // Has the place keep what the part at `at` holds, which `make` copies,
  // before the event's execution under way first changes it -- where an
  // `always_comb` or `always_latch` watches the place, and nothing the
  // execution kept here holds the part already. Asked by a write before it
  // changes the part. A part an inexact path reaches is not kept, since what
  // such a path names is more than the part.
  template <class Make>
  void KeepOriginal(const PartPath& at, Make make) {
    if (WatchedCombinationally()) {
      KeepOriginalBy(at, OriginalMadeBy(make));
    }
  }
  // The same of the bits `bits` of the integral value at `at`, whose words
  // `live` names: only the words they lie in are kept, so a write of a few bits
  // of a wide value keeps those few.
  void KeepBitsOriginal(
      const PartPath& at, value::IntegralView live, value::BitPositions bits);

  // Whether what the event's execution under way kept here holds all that a
  // write to the part at `written` changed, as `change` says, so that what the
  // write did can be judged once the execution ends: the part, or each element
  // a write of elements reached.
  [[nodiscard]] auto KeepsWhatWasWritten(
      const PartPath& written, const Change& change) const -> bool;

  // One wait enrolled here, with the path of the part it is enrolled at.
  struct Enrolled {
    PartPath path;
    WaitMembership* membership = nullptr;
  };
  // Every wait enrolled on this place and on any part of it.
  [[nodiscard]] auto EveryEnrolled() -> std::vector<Enrolled>;

 private:
  struct Node;
  struct Children;

  // The memberships at the part `path` leads to, the node made if no wait has
  // needed it yet.
  [[nodiscard]] auto MembersAt(const PartPath& path)
      -> IntrusiveList<WaitMembership>&;

  template <class Tell>
  void TellAround(const PartPath& at, const Change& change, Tell tell);

  void KeepOriginalBy(const PartPath& at, MakeOriginal make);
  // Whether what the event's execution under way kept here holds the part at
  // `written` from before the execution.
  [[nodiscard]] auto KeepsOriginalOf(const PartPath& written) const -> bool;

  // The waits on the whole place: the root of the tree.
  IntrusiveList<WaitMembership> members_;
  // The nodes one step down, none until a wait is enrolled on a part.
  std::unique_ptr<Children> children_;
  // None until an `always_comb` or `always_latch` enrols here.
  std::unique_ptr<CombinationalWatch> combinational_;
};

}  // namespace lyra::runtime
