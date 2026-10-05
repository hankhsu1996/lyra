#pragma once

#include <cstdint>
#include <span>
#include <string_view>
#include <vector>

#include "lyra/runtime/observation.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/value/packed.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::runtime {

class Observable;
class RuntimeEffects;

// The places one evaluation of a waited expression reached (LRM 9.4.2), which
// the process then waits on: reaching any of them is a candidacy, and the
// process decides by evaluating again. A function the expression calls is
// handed it and states what it reads before it runs, handing it on to the
// functions it calls in turn, which state theirs instead of running; so what a
// call reads is stated by the unit that declares the function and asked where
// the wait stands.
//
// Every member is defined in the library: a unit stating a wait builds and
// destroys these, and a definition written here would be compiled again by
// each such unit.
//
// What is reached through a handle -- a class object, a variable of the
// interface instance a virtual interface holds, and whatever a call made on
// either reads -- is kept apart from what is read directly. A wait watches
// both (LRM 9.4.2).
//
// The implicit list of an `always_comb` or `always_latch` is collected into one
// too, once, before the procedure first runs (LRM 9.2.2.2.1), and is told what
// is written as well as what is read. That list takes nothing reached through a
// handle -- the clause adds nothing for a class object or a call made on one,
// and a list collected once cannot follow a handle that changes -- and leaves
// out what the procedure or its functions write.
class ReadReport {
 public:
  // A report nothing has been stated into yet.
  [[nodiscard]] static auto Empty() -> ReadReport;

  ReadReport(const ReadReport&) = delete;
  auto operator=(const ReadReport&) -> ReadReport& = delete;
  ReadReport(ReadReport&&) noexcept;
  auto operator=(ReadReport&&) noexcept -> ReadReport&;
  ~ReadReport();

  // A place the expression reads, and which bits of its flat-bit encoding, as
  // a trigger reads them; reached through a handle where a call made on one
  // is reporting.
  void Add(
      Observable* place, const value::PackedArray& lsb_bit_offset,
      const value::PackedArray& bit_width);

  // A place reached through a handle: an object a chain of reads passes
  // through, whose event source `place` is, or a variable of the instance a
  // virtual interface holds.
  void AddThroughHandle(
      Observable* place, const value::PackedArray& lsb_bit_offset,
      const value::PackedArray& bit_width);

  // Every object at once, for a read that reaches one along no chain a report
  // could follow.
  void AddEveryObject();

  // The bracket around a call made on an object or through a virtual
  // interface, while which everything reported is reached through a handle.
  void EnterCallOnHandle();
  void LeaveCallOnHandle();

  // A place the evaluation writes, and which bits of it, as a read names them.
  // Only an implicit list reads these, and it leaves them out.
  void AddWrite(
      Observable* place, const value::PackedArray& lsb_bit_offset,
      const value::PackedArray& bit_width);

  // Makes what was reported a procedure's implicit list (LRM 9.2.2.2.1), once
  // everything is reported: nothing reached through a handle is in it, what
  // was written is taken out of what was read, and each place is listed once.
  // A write of a whole place takes every read of it; a write of some bits
  // takes those bits out of a read naming its bits, and leaves a read of the
  // whole place alone, since what is left of a whole is not known here -- a
  // wake more, never one fewer.
  void SettleAsImplicitList();

  // Whether a function handed this report goes on to report into it: one or
  // zero, the carrier a machine answer crosses as. Reports nest as deep as the
  // calls making them, and a cycle of calls would nest without end, so past a
  // bound the report stops following calls and watches every object instead.
  // Every function past the bound is one a shallower level of the same cycle
  // already reported, so objects are all that is left to cover.
  auto Enter() -> std::int64_t;
  void Leave();

  // Whether a function that has just reported into this goes on to run its
  // body, one or zero: the one the waited evaluation called does, the
  // evaluation needing its value, while one another function's report called
  // stands in for a body that does not run.
  [[nodiscard]] auto RunsTheBody() const -> std::int64_t;

  // What was stated, read directly and through handles alike, handed to the
  // wait that parks on it and leaving the report empty for the evaluation
  // after.
  [[nodiscard]] auto TakeTriggers() -> std::vector<Trigger>;

  // The implicit list, once settled.
  [[nodiscard]] auto ImplicitList() const -> std::span<const Trigger> {
    return read_directly_;
  }

 private:
  // Bits of a place the evaluation writes, which no wait watches.
  struct Written {
    Observable* place = nullptr;
    value::BitPositions bits;
  };

  ReadReport();

  void Reach(Trigger trigger);

  std::vector<Trigger> read_directly_;
  std::vector<Trigger> through_handles_;
  std::vector<Written> writes_;
  std::int64_t depth_ = 0;
  // How many calls made on a handle are reporting now.
  std::int64_t calls_on_handles_ = 0;
};

// A function reporting what a call of it reads meets a read no leaf watches
// yet. The function compiled whether or not a wait calls it, so the design's
// request fails here, where one does (LRM 9.4.2).
[[noreturn]] void RefuseReport(std::string_view why);

// An event control its process decides: the frame resumes on every candidacy
// the reports' places see, and evaluates its observations again to learn
// whether it was an event and what it reaches now. The observations are held
// for a restart (LRM 9.7), which leaves them to be armed by that evaluation
// rather than wherever the restart is asked for.
auto WaitRecollecting(
    RuntimeEffects& services, std::span<ReadReport* const> reports,
    std::span<const Observation* const> observations) -> bool;

// A `wait (cond)` waiting on what the last test of its condition reached (LRM
// 9.4.3).
auto WaitUntil(RuntimeEffects& services, std::span<ReadReport* const> reports)
    -> bool;

// A procedure's implicit list, collected once into `report`: the wait each time
// the procedure finishes its body, watching the same leaves every time.
auto WaitAny(RuntimeEffects& services, const ReadReport* report) -> bool;

}  // namespace lyra::runtime
