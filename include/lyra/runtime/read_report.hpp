#pragma once

#include <cstdint>
#include <span>
#include <string_view>
#include <vector>

#include "lyra/runtime/observation.hpp"
#include "lyra/runtime/trigger.hpp"
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
  // a trigger reads them.
  void Add(
      Observable* place, const value::PackedArray& lsb_bit_offset,
      const value::PackedArray& bit_width);

  // Every object at once, for a read that reaches one along no chain a report
  // could follow.
  void AddEveryObject();

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

  // What was stated, handed to the wait that parks on it and leaving the report
  // empty for the evaluation after.
  [[nodiscard]] auto TakeTriggers() -> std::vector<Trigger>;

 private:
  ReadReport();

  std::vector<Trigger> triggers_;
  std::int64_t depth_ = 0;
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

}  // namespace lyra::runtime
