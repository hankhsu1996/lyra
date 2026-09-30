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

// What a wait learns each time it collects its leaves about what one of its
// event expressions can read (LRM 9.4.2): every place a leaf watches, decided
// by that expression's observation. A function the expression calls is handed
// it and reports into it instead of running, handing it on to the functions it
// calls in turn, so what a call reads is stated by the unit that declares the
// function and asked where the wait stands.
//
// Every member is defined in the library: a unit stating a wait builds and
// destroys these, and a definition written here would be compiled again by
// each such unit.
class ReadReport {
 public:
  // The report for the event expression `observation` watches, begun empty
  // each time the wait collects its leaves.
  [[nodiscard]] static auto For(Observation observation) -> ReadReport;

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

  [[nodiscard]] auto Triggers() const -> std::span<const Trigger> {
    return triggers_;
  }

 private:
  explicit ReadReport(Observation observation);

  Observation observation_;
  std::vector<Trigger> triggers_;
  std::int64_t depth_ = 0;
};

// A function reporting what a call of it reads meets a read no leaf watches
// yet. The function compiled whether or not a wait calls it, so the design's
// request fails here, where one does (LRM 9.4.2).
[[noreturn]] void RefuseReport(std::string_view why);

// An event control whose leaves are collected afresh on every candidacy: the
// frame resumes on each, and the observations say whether it was an event.
auto WaitRecollecting(
    RuntimeEffects& services, std::span<ReadReport* const> reports) -> bool;

// A `wait (cond)` whose leaves are collected afresh each time the condition is
// tested (LRM 9.4.3).
auto WaitUntil(RuntimeEffects& services, std::span<ReadReport* const> reports)
    -> bool;

}  // namespace lyra::runtime
