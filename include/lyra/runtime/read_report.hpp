#pragma once

#include <cstdint>
#include <memory>
#include <span>
#include <string_view>
#include <vector>

#include "lyra/runtime/object_ref.hpp"
#include "lyra/runtime/part_path.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/object_ref.hpp"

namespace lyra::runtime {

class Observable;
class RuntimeEffects;

// What one evaluation reached, as a wait takes it: the places it watches, and
// the objects among them, which it keeps for as long as it watches them.
struct ReachedPlaces {
  std::vector<Trigger> triggers;
  std::vector<std::shared_ptr<GcObject>> objects;
};

// The places one evaluation of a waited expression reached (LRM 9.4.2), which
// the process then waits on: reaching any of them is a candidacy, and the
// process decides by evaluating again. Every function and construction the
// evaluation runs is handed it, states the cells its text reads as it is
// entered and what it reaches through a handle as it reaches it, and hands it
// on to what it calls; so the places are what this evaluation read, wherever
// the code reading them was compiled.
//
// Every member is defined in the library: a unit stating a wait builds and
// destroys these, and a definition written here would be compiled again by
// each such unit.
//
// The implicit list of an `always_comb` or `always_latch` is collected into one
// too, once, before the procedure first runs (LRM 9.2.2.2.1), and is told what
// is written as well as what is read. Nothing runs to collect it: each
// function called states what its text reads and writes and has the functions
// it calls do the same, and the list leaves out what is written.
class ReadReport {
 public:
  // A report nothing has been stated into yet.
  [[nodiscard]] static auto Empty() -> ReadReport;
  // The same for a procedure's implicit list (LRM 9.2.2.2.1), which no
  // evaluation makes: a function called to report into it states what it reads
  // and never runs, so collecting the list does nothing the procedure would
  // not.
  [[nodiscard]] static auto ForImplicitList() -> ReadReport;

  ReadReport(const ReadReport&) = delete;
  auto operator=(const ReadReport&) -> ReadReport& = delete;
  ReadReport(ReadReport&&) noexcept;
  auto operator=(ReadReport&&) noexcept -> ReadReport&;
  ~ReadReport();

  // A place the expression reads: which part of it, by the steps that reach
  // the part, and which positions of that part, as a trigger reads them. A
  // variable of the instance a virtual interface holds is stated whole.
  void Add(
      Observable* place, std::span<const std::int64_t> steps,
      std::int64_t lsb_bit_offset, std::int64_t bit_width);
  void Add(
      WatchedPlace place, std::span<const std::int64_t> steps,
      std::int64_t lsb_bit_offset, std::int64_t bit_width);

  // An object a property is read on, which one event source covers whole (LRM
  // 9.4.2). The wait keeps it while it waits on it, as a subscription keeps
  // what it is subscribed to: the evaluation may have reached it through
  // nothing but a variable of a function that has since returned. One nothing
  // owns yet is being constructed by this evaluation, which a later one
  // constructs afresh, so it is not watched. Reaching it through a handle
  // naming no object is the design's own failure (LRM 8.4).
  void AddObject(GcObject* object);
  void AddObject(const value::ObjectRef& object);

  // A place the evaluation writes: which part of it and which bits of that
  // part, as a read names them. Only an implicit list reads these, and it
  // leaves them out.
  void AddWrite(
      Observable* place, std::span<const std::int64_t> steps,
      std::int64_t lsb_bit_offset, std::int64_t bit_width);
  void AddWrite(
      WatchedPlace place, std::span<const std::int64_t> steps,
      std::int64_t lsb_bit_offset, std::int64_t bit_width);

  // Makes what was reported a procedure's implicit list (LRM 9.2.2.2.1), once
  // everything is reported: what was written is taken out of what was read,
  // and each part of a place is listed once. A write of the whole of a part
  // takes every read of that part and of anything in it; a write of some bits
  // of a part takes those bits out of a read naming bits of it, and leaves a
  // read of the whole part alone, since what is left of a whole is not known
  // here -- a wake more, never one fewer. A write of part of what was read
  // leaves that read alone the same way.
  void SettleAsImplicitList();

  // Whether a function collecting into an implicit list goes on to state what
  // it reads and has its callees do the same: one or zero, the carrier a
  // machine answer crosses as. The functions nest as deep as their calls, and
  // a cycle of calls would nest without end, so past a bound they stop: every
  // function past it is one a shallower level of the same cycle already
  // stated.
  auto Enter() -> std::int64_t;
  void Leave();

  // Whether a function handed this report runs its body, one or zero: one
  // handed it by a waited evaluation does, the evaluation needing its value,
  // and one collecting into an implicit list does not, since nothing the
  // procedure would not do may happen to collect it.
  [[nodiscard]] auto RunsTheBody() const -> std::int64_t;

  // The extent of the evaluation this report is made for. Foreign code reads
  // SystemVerilog data only through the exported functions it calls (LRM
  // 35.5.3), and a report cannot be handed across the foreign boundary, so
  // while the evaluation runs, an exported function called from foreign code
  // reports here, as the function the evaluation called does. Ending it hands
  // that back to whatever was in progress when it began.
  void BeginEvaluation(RuntimeEffects& effects);
  void EndEvaluation(RuntimeEffects& effects) noexcept;

  // What was stated, handed to the wait that parks on it and leaving the
  // report empty for the evaluation after.
  [[nodiscard]] auto TakeReached() -> ReachedPlaces;

  // The implicit list, once settled.
  [[nodiscard]] auto ImplicitList() const -> std::span<const Trigger> {
    return reached_;
  }

 private:
  // Bits of a part of a place the evaluation writes, which no wait watches.
  struct Written {
    Observable* place = nullptr;
    PartPath part;
    value::BitPositions bits;
  };

  explicit ReadReport(bool an_evaluation_reports);

  std::vector<Trigger> reached_;
  std::vector<std::shared_ptr<GcObject>> objects_;
  std::vector<Written> writes_;
  std::int64_t depth_ = 0;
  // Whether an evaluation is what reports here, which needs the value of the
  // function it calls and so runs it.
  bool an_evaluation_reports_;
  // The report of the evaluation in progress when this one's began.
  ReadReport* displaced_ = nullptr;
};

// The report of the evaluation the running execution is making, and null
// where it is making none: what an exported function called from foreign code
// reports into (LRM 35.5.3).
[[nodiscard]] auto EvaluationInProgress(RuntimeEffects& effects) -> ReadReport*;

// A body handed a report reads something no wait can watch yet. The body
// compiled whether or not a wait reaches it, so the design's request fails
// here, where one does (LRM 9.4.2).
[[noreturn]] void RefuseReport(std::string_view why);

}  // namespace lyra::runtime
