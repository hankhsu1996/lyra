#pragma once

#include <cstddef>
#include <cstdint>
#include <memory>
#include <optional>
#include <span>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/time.hpp"
#include "lyra/runtime/dump_writer.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/runtime/wait.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::runtime {

class Scope;
class ValueDump;
class ValueDumps;

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

// One thing a dump records, as the dump reads it: what it holds now, and the
// place a change to it is reported at. Each storage the library realizes has
// its own, made where an instance is described.
class DumpProbe {
 public:
  DumpProbe();
  DumpProbe(const DumpProbe&) = delete;
  auto operator=(const DumpProbe&) -> DumpProbe& = delete;
  DumpProbe(DumpProbe&&) = delete;
  auto operator=(DumpProbe&&) -> DumpProbe& = delete;
  virtual ~DumpProbe();

  // Records what the thing holds now under `code`.
  virtual void Read(DumpCapture& into, DumpCode code) const = 0;
  // Where a change to the thing is reported.
  [[nodiscard]] virtual auto Watched() const -> Trigger = 0;
  // The storage the thing is, which two names standing for one thing share.
  [[nodiscard]] virtual auto Identity() const -> const void* = 0;
  // The member of the instance the thing was described from, which is how a
  // `$dumpvars` naming a variable names it.
  [[nodiscard]] virtual auto Member() const -> const void* = 0;
};

// What an instance of the design hierarchy is described to (LRM 21.7.1.2): the
// things it declares that a dump may record, in the order it declares them,
// with the named scopes it declares around their static variables. The
// instances below it are described on their own.
class Describer {
 public:
  Describer() = default;
  Describer(const Describer&) = delete;
  auto operator=(const Describer&) -> Describer& = delete;
  Describer(Describer&&) = delete;
  auto operator=(Describer&&) -> Describer& = delete;
  virtual ~Describer();

  virtual void Var(
      const DumpEntry& entry, std::unique_ptr<DumpProbe> probe) = 0;
  void BeginBlock(const DumpBlock& block);
  void EndBlock();

 protected:
  // The named scopes around what is being described, outermost first.
  [[nodiscard]] auto Enclosing() const -> std::span<const DumpBlock* const> {
    return enclosing_;
  }

 private:
  // A named scope was entered, which is now the innermost of `Enclosing`, or
  // the innermost is about to be left. A describer that records nothing of
  // the scopes themselves has nothing to do on either.
  virtual void Entered() {
  }
  virtual void Leaving() {
  }

  std::vector<const DumpBlock*> enclosing_;
};

// One thing a running dump records: enrolled on its place for as long as the
// dump runs, and told when the thing changes so the dump records it once at
// the end of the time step (LRM 21.7.1.4).
class DumpWatch {
 public:
  DumpWatch(ValueDump& dump, DumpCode code, std::unique_ptr<DumpProbe> probe);
  DumpWatch(const DumpWatch&) = delete;
  auto operator=(const DumpWatch&) -> DumpWatch& = delete;
  DumpWatch(DumpWatch&&) = delete;
  auto operator=(DumpWatch&&) -> DumpWatch& = delete;
  ~DumpWatch();

  void Reached();
  void Read(DumpCapture& into) const {
    probe_->Read(into, code_);
  }
  [[nodiscard]] auto Code() const -> DumpCode {
    return code_;
  }
  // Answers whether the thing changed in the time step now ending, which it
  // then no longer has.
  auto TakeChanged() -> bool {
    return std::exchange(changed_, false);
  }

 private:
  bool changed_ = false;
  ValueDump* dump_;
  DumpCode code_;
  std::unique_ptr<DumpProbe> probe_;
  WaitMembership membership_;
};

// What a dump records (LRM 21.7.1.2): each instance named and as many levels
// of the hierarchy below it as asked for, none meaning every level, and each
// variable named on its own, by the member it is of the instance declaring
// it. Nothing named is the whole design.
struct DumpSelection {
  struct Instances {
    Scope* scope;
    // 0 is every level below.
    std::int64_t levels;
  };
  std::vector<Instances> instances;
  std::vector<const void*> variables;
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
// simulation only copies the values that changed.
class ValueDump {
 public:
  ValueDump(std::unique_ptr<DumpWriter> writer, std::int8_t precision_power);
  ValueDump(const ValueDump&) = delete;
  auto operator=(const ValueDump&) -> ValueDump& = delete;
  ValueDump(ValueDump&&) = delete;
  auto operator=(ValueDump&&) -> ValueDump& = delete;
  ~ValueDump();

  // Declares what `selection` names under `roots`, the design's top scopes,
  // records its first values, and records every change from now on. Each
  // instance describes itself to the describer `dumps` holds meanwhile.
  void Start(
      const DumpSelection& selection, std::span<Scope* const> roots,
      SimTime now, ValueDumps& dumps);
  // The same for the ports `selection` names, each instance a scope of its
  // own named by its hierarchical name and its ports in the order its
  // declaration gives them (LRM 21.7.4.2).
  void Start(const PortSelection& selection, SimTime now, ValueDumps& dumps);

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

 private:
  void Checkpoint(DumpCheckpoint checkpoint, SimTime now);
  void Begin(SimTime now);
  void At(SimTime now);

  std::unique_ptr<DumpOutput> output_;
  std::vector<std::unique_ptr<DumpWatch>> watches_;
  std::vector<DumpWatch*> changed_;
  std::optional<SimTime> step_submitted_for_;
  std::optional<SimTime> time_sent_;
  bool on_ = true;
};

}  // namespace lyra::runtime
