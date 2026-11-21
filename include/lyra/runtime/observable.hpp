#pragma once

#include <memory>
#include <vector>

#include "lyra/runtime/interval_accumulator.hpp"
#include "lyra/runtime/intrusive_list.hpp"
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
    return !root_.members.Empty() || root_.children != nullptr;
  }

  // Whether a wait has been enrolled at a part inside the part at `at`, or at
  // that part reading some of its positions rather than all of it -- a slice of
  // an array (LRM 7.4.5) -- which a write replacing that part keeps what it
  // replaced for, so each such wait is told whether what it reads changed.
  [[nodiscard]] auto WatchedInside(const PartPath& at) const -> bool;

  // Enrols `membership` at the part `path` leads to, the whole of this place
  // where the path is empty, for its awaiter's whole life. The node is made if
  // no wait has needed it yet. A per-interval observer's membership has the
  // place keep, from then on, the original of each part an interval
  // modifies.
  void Enrol(const PartPath& path, WaitMembership& membership);

  // Tells the waits a write to the part at `written` reached, `change` being
  // what the write did to that part. The waits on the part itself are told the
  // change. The waits on each part holding it are told which of that part's
  // elements or members the write went into, so one watching only others is
  // passed over. The waits on each part inside it are told what the change
  // says became of that part, and a part it shows unchanged is passed over with
  // everything inside it. No other wait of the place is visited. Where the path
  // is inexact the write was at or inside the part, so every wait on it and
  // inside it is told only that it may have changed.
  // `accumulated` says whether the open interval holds the original of
  // what the write modified, so that a per-interval observer is told the write
  // only by the interval's net change when it ends.
  void TellChange(
      RuntimeEffects& effects, const PartPath& written, const Change& change,
      bool accumulated);

  // Tells the waits on the part at `at`, on each part holding it and on each
  // part inside it that an interval left the part changed, `change`
  // being what became of it measured from its original.
  void TellIntervalChange(
      RuntimeEffects& effects, const PartPath& at, const Change& change);

  // Whether a per-interval observer has enrolled on the place, which it goes on
  // answering for the place's life, and which is when a modification of it may
  // keep an original. Written here because every watched write asks it.
  [[nodiscard]] auto WatchedPerInterval() const noexcept -> bool {
    return accumulator_ != nullptr;
  }

  // Asked by every modification of the part at `at` immediately before it
  // changes the part, with no code run in between: has the open interval keep
  // the part's original, which `copy` makes, where none of a part holding it is
  // kept already, and answers whether the interval accumulated the modification
  // -- an interval is open, a per-interval observer watches the place, and the
  // original of the part or of a part holding it is kept. A part an inexact
  // path reaches is never kept itself, since what such a path names is more
  // than the part; it is accumulated only where a part holding it is. Where no
  // per-interval observer watches, which is every place of a design built
  // without the dialect, it is a null test.
  template <class Copy>
  [[nodiscard]] auto BeforeModify(const PartPath& at, Copy copy) -> bool {
    if (accumulator_ == nullptr) {
      return false;
    }
    return Accumulate(at, CopyOriginalBy(copy));
  }
  // The same of the bits `bits` of the integral value at `at`, whose words
  // `live` names: only the words they lie in are kept, so a write of a few bits
  // of a wide value keeps those few. Out of line, since only a write that found
  // a per-interval observer enrolled on the place as it opened asks it.
  [[nodiscard]] auto BeforeModifyBits(
      const PartPath& at, value::IntegralView live, value::BitPositions bits)
      -> bool;

  // One wait enrolled here, with the path of the part it is enrolled at.
  struct Enrolled {
    PartPath path;
    WaitMembership* membership = nullptr;
  };
  // Every wait enrolled on this place and on any part of it.
  [[nodiscard]] auto EveryEnrolled() -> std::vector<Enrolled>;

 private:
  struct Children;
  // One part some wait is enrolled at or under, the whole place at the root:
  // the waits on it, the nodes of the parts inside it, none until a wait is
  // enrolled on one, and whether a wait on it reads some of its positions
  // rather than all of it, so a write replacing it keeps what it held for that
  // wait.
  struct Node {
    IntrusiveList<WaitMembership> members;
    std::unique_ptr<Children> children;
    bool positions_read = false;
  };

  // The memberships at the part `path` leads to, the node made if no wait has
  // needed it yet, noted from then on as one some of whose positions a wait
  // reads where `reads_positions` says so.
  [[nodiscard]] auto MembersAt(const PartPath& path, bool reads_positions)
      -> IntrusiveList<WaitMembership>&;

  template <class Tell>
  void TellAround(const PartPath& at, const Change& change, Tell tell);

  [[nodiscard]] auto Accumulate(const PartPath& at, CopyOriginal copy) -> bool;

  // The whole place: the root of the tree.
  Node root_;
  // None until a per-interval observer enrols here.
  std::unique_ptr<IntervalAccumulator> accumulator_;
};

}  // namespace lyra::runtime
