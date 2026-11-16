#pragma once

#include <cstddef>
#include <cstdint>
#include <memory>
#include <optional>
#include <span>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/diag_message.hpp"
#include "lyra/base/source_location.hpp"
#include "lyra/base/time.hpp"
#include "lyra/runtime/dump_writer.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/runtime/wait.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/object_ref.hpp"

namespace lyra::runtime {

class Scope;
class ValueDump;

// The record one time step of a dump is made of, built on the simulation's
// own thread and handed to the one that writes the file.
class DumpCapture {
 public:
  void Bits(DumpCode code, const value::ConstIntegralView& value);
  void Real(DumpCode code, double value);
  void Triggered(DumpCode code);
  // What a dump being switched off records of a thing (LRM 21.7.1.3).
  void Unknown(DumpCode code);

  // An integral value, as its width, whether it is four-state and so has an
  // unknown plane, and where its planes start among the capture's words.
  struct Words {
    std::uint64_t width = 0;
    bool four_state = false;
    std::size_t at = 0;
  };
  struct Change {
    DumpCode code;
    std::variant<Words, double, DumpTriggered, DumpUnknown> value;
  };
  [[nodiscard]] auto Changes() const -> std::span<const Change> {
    return changes_;
  }
  [[nodiscard]] auto Value(const Change& change) const -> DumpValue;
  void Clear();

 private:
  std::vector<Change> changes_;
  std::vector<std::uint64_t> words_;
};

// One thing a dump records, where it is read: the instance declaring it, none
// for one a package or the compilation unit's scope keeps, and the record its
// declarer states of it, whose bodies read it and watch it there.
struct DumpedObject {
  Scope* instance = nullptr;
  const abi::DataObjectRecord* object = nullptr;
};

// What a dump finds a variable a `$dumpvars` names by among those it walks
// (LRM 21.7.1.2): the member a wait for a change of it is reached through,
// which a force rebinds and the wait follows (LRM 10.6.2), or where a wait
// reached by itself is reported to; none where a wait on it is reported
// nowhere.
auto DumpIdentityOf(const Trigger& trigger) -> const void*;
auto DumpIdentityOf(const DumpedObject& dumped) -> const void*;

// One thing a running dump records: enrolled on its place for as long as the
// dump runs, and told when the thing changes so the dump records it once at
// the end of the time step (LRM 21.7.1.4).
class DumpWatch {
 public:
  DumpWatch(ValueDump& dump, DumpCode code, DumpedObject dumped);
  DumpWatch(const DumpWatch&) = delete;
  auto operator=(const DumpWatch&) -> DumpWatch& = delete;
  DumpWatch(DumpWatch&&) = delete;
  auto operator=(DumpWatch&&) -> DumpWatch& = delete;
  ~DumpWatch();

  void Reached();
  // Records what the thing holds now, and follows what it refers to there.
  void Read(DumpCapture& into);
  [[nodiscard]] auto Code() const -> DumpCode {
    return code_;
  }
  // Answers whether the thing changed in the time step now ending, which it
  // then no longer has.
  auto TakeChanged() -> bool {
    return std::exchange(changed_, false);
  }

 private:
  // Enrols on what the thing refers to, `now`, where that moved. It moves only
  // with a change of the thing, and the dump reads the thing before it records
  // anything more of it.
  void FollowReferent(value::ObjectRef now);

  bool changed_ = false;
  ValueDump* dump_;
  DumpCode code_;
  DumpedObject dumped_;
  WaitMembership membership_;
  // What the thing refers to, held for as long as the watch is enrolled on
  // it, and moved when the thing comes to refer to another.
  value::ObjectRef referent_;
  WaitMembership referent_membership_;
};

// What a dump records (LRM 21.7.1.2): each instance named and as many levels
// of the hierarchy below it as asked for, none meaning every level, and each
// variable named on its own, by what a dump tells it from another by.
struct DumpSelection {
  struct Instances {
    Scope* scope;
    // 0 is every level below.
    std::int64_t levels;
  };
  std::vector<Instances> instances;
  std::vector<const void*> variables;
  // Each package and compilation unit's scope recorded whole, which stand
  // beside the tops (LRM 37.10) and hold no instance below them.
  std::vector<const abi::ScopeDeclarations*> namespaces;
};

// What an extended dump records (LRM 21.7.3.1): the ports of each instance
// named, and none of the instances below it.
struct PortSelection {
  std::vector<Scope*> instances;
};

class DumpOutput;

// One dump file and what it records (LRM 21.7.1): it starts at the end of a
// time step, recording the first values of what it selects, records each
// change at the end of the time step it happened in, and is switched off and
// on, checkpointed, bounded and flushed as the design or the run asks. What is
// recorded is handed to a thread of its own that writes the file, so the
// simulation only copies the values that changed, and so that a run ending
// where the simulation cannot go on still has that thread to finish the file.
//
// A file the system refuses to write, or a dump the system will not set up,
// is the failure of whoever asked for the dump at `asked_at`, as a file that
// cannot be opened is.
class ValueDump {
 public:
  ValueDump(
      std::unique_ptr<DumpWriter> writer, std::int8_t precision_power,
      std::string file, base::SourceLocation asked_at);
  ValueDump(const ValueDump&) = delete;
  auto operator=(const ValueDump&) -> ValueDump& = delete;
  ValueDump(ValueDump&&) = delete;
  auto operator=(ValueDump&&) -> ValueDump& = delete;
  ~ValueDump();

  // Declares what `selection` names among `namespaces` and the tops below
  // `root`, records its first values, and records every change from now on.
  void Start(
      const DumpSelection& selection, Scope& root,
      std::span<const abi::ScopeDeclarations* const> namespaces, SimTime now);
  // The same for the ports `selection` names, each instance a scope of its
  // own named by its hierarchical name and its ports in the order its
  // declaration gives them (LRM 21.7.4.2).
  void Start(const PortSelection& selection, SimTime now);

  // A watched thing changed; it is recorded once at the end of the step.
  void Changed(DumpWatch& watch);
  // Records what changed in the step ending at `now`.
  void EndStep(SimTime now);

  void Off(SimTime now);
  void On(SimTime now);
  void All(SimTime now);
  void Limit(std::uint64_t bytes);
  // Returns once everything recorded so far is in the file.
  void Flush();
  // Ends the file at `now`, reporting a failure to write it.
  void Close(SimTime now);
  // The run is ending where the simulation cannot go on, at `now`: returns
  // once the file holds every time step recorded and is ended, the ending's
  // own work done by the thread writing it. Nothing here is beyond what a
  // signal handler may call.
  void FinishAtEnding(SimTime now) noexcept;

 private:
  void Checkpoint(DumpCheckpoint checkpoint, SimTime now);
  void Begin(SimTime now);
  void At(SimTime now);
  // What has been recorded since the last is whole, and may be written.
  void Publish();
  // Reports what the writer met: a defect, and, once, a refusal of the system
  // to write the file.
  void RaiseIfRefused();

  std::unique_ptr<DumpOutput> output_;
  std::string file_;
  base::SourceLocation asked_at_;
  bool refusal_raised_ = false;
  std::vector<std::unique_ptr<DumpWatch>> watches_;
  std::vector<DumpWatch*> changed_;
  std::optional<SimTime> step_submitted_for_;
  bool on_ = true;
};

// Raises `message` as the failure of whoever asked for a dump: of the call at
// `asked_at`, or, where that names no place, of the run's own options, which
// no statement wrote.
[[noreturn]] void RaiseForWhoeverAsked(
    base::SourceLocation asked_at, base::DiagMessage message);

}  // namespace lyra::runtime
