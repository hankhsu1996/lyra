#include "lyra/runtime/value_dump.hpp"

#include <algorithm>
#include <array>
#include <atomic>
#include <cerrno>
#include <cstdint>
#include <exception>
#include <expected>
#include <fcntl.h>
#include <memory>
#include <optional>
#include <span>
#include <string>
#include <sys/types.h>
#include <system_error>
#include <thread>
#include <unistd.h>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/diag_message.hpp"
#include "lyra/base/errno_as_error_code.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/base/source_location.hpp"
#include "lyra/base/time.hpp"
#include "lyra/base/write_all.hpp"
#include "lyra/runtime/dump_kind.hpp"
#include "lyra/runtime/dump_scope_kind.hpp"
#include "lyra/runtime/dump_writer.hpp"
#include "lyra/runtime/file.hpp"
#include "lyra/runtime/hierarchical_name.hpp"
#include "lyra/runtime/hierarchy_levels.hpp"
#include "lyra/runtime/object_change.hpp"
#include "lyra/runtime/object_ref.hpp"
#include "lyra/runtime/reflection.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/scope.hpp"
#include "lyra/runtime/synchronization_object.hpp"
#include "lyra/runtime/value_dumps.hpp"
#include "lyra/support/region.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/object_ref.hpp"

namespace lyra::runtime {

void DumpCapture::Bits(DumpCode code, const value::ConstIntegralView& value) {
  const std::size_t at = words_.size();
  words_.insert(words_.end(), value.value.begin(), value.value.end());
  words_.insert(words_.end(), value.unknown.begin(), value.unknown.end());
  changes_.push_back(
      {.code = code,
       .value = Words{
           .width = value.width, .four_state = value.IsFourState(), .at = at}});
}

void DumpCapture::Real(DumpCode code, double value) {
  changes_.push_back({.code = code, .value = value});
}

void DumpCapture::Triggered(DumpCode code) {
  changes_.push_back({.code = code, .value = DumpTriggered{}});
}

void DumpCapture::Unknown(DumpCode code) {
  changes_.push_back({.code = code, .value = DumpUnknown{}});
}

auto DumpCapture::Value(const Change& change) const -> DumpValue {
  return std::visit(
      Overloaded{
          [&](const Words& words) -> DumpValue {
            const auto count =
                static_cast<std::size_t>((words.width + 63) / 64);
            const std::span<const std::uint64_t> all(words_);
            return value::ConstIntegralView{
                .value = all.subspan(words.at, count),
                .unknown = words.four_state
                               ? all.subspan(words.at + count, count)
                               : std::span<const std::uint64_t>{},
                .width = words.width};
          },
          [](double real) -> DumpValue { return real; },
          [](const DumpTriggered& triggered) -> DumpValue { return triggered; },
          [](const DumpUnknown& unknown) -> DumpValue { return unknown; }},
      change.value);
}

void DumpCapture::Clear() {
  changes_.clear();
  words_.clear();
}

namespace {

// Records what a data object's reading code hands it into a dump's capture,
// under the object's code: an event variable in a time step the object it
// names was triggered in (LRM 21.7.2.2), keeping that object, whose triggers
// are changes of the variable too.
class CaptureSerializer final : public ValueSerializer {
 public:
  CaptureSerializer(DumpCapture& into, DumpCode code)
      : into_(&into), code_(code) {
  }

  void SerializeBits(const value::ConstIntegralView& value) override {
    into_->Bits(code_, value);
  }
  void SerializeReal(double value) override {
    into_->Real(code_, value);
  }
  void SerializeEvent(const value::ObjectRef& event) override {
    referent_ = event;
    switch (EventTriggered(event, current_runtime(), nullptr).Truth()) {
      case value::Truthiness::kKnownNonzero:
        into_->Triggered(code_);
        return;
      case value::Truthiness::kKnownZero:
      case value::Truthiness::kUnknown:
        return;
    }
  }

  // The object an event variable names; none for any other.
  [[nodiscard]] auto TakeReferent() -> value::ObjectRef {
    return std::move(referent_);
  }

 private:
  DumpCapture* into_;
  DumpCode code_;
  value::ObjectRef referent_;
};

// Keeps the trigger a data object's watching code hands it.
class TriggerSubscriber final : public Subscriber {
 public:
  void Subscribe(const Trigger& handed) override {
    trigger = handed;
  }

  Trigger trigger;
};

// The trigger a wait for a change of the thing would enrol (LRM 9.4.2), which
// a dump enrols on too, so it learns of a change where the design's own wait
// does.
auto WatchedTriggerOf(const DumpedObject& dumped) -> Trigger {
  TriggerSubscriber subscriber;
  dumped.object->subscribe(dumped.instance, subscriber);
  return subscriber.trigger;
}

}  // namespace

auto DumpIdentityOf(const Trigger& trigger) -> const void* {
  if (trigger.through != nullptr) {
    return trigger.through;
  }
  return trigger.observable;
}

auto DumpIdentityOf(const DumpedObject& dumped) -> const void* {
  return DumpIdentityOf(WatchedTriggerOf(dumped));
}

DumpWatch::DumpWatch(ValueDump& dump, DumpCode code, DumpedObject dumped)
    : dump_(&dump), code_(code), dumped_(dumped) {
  EnrolOnPlace(membership_, this, WatchedTriggerOf(dumped_));
}

DumpWatch::~DumpWatch() = default;

void DumpWatch::Read(DumpCapture& into) {
  CaptureSerializer serializer(into, code_);
  dumped_.object->read(dumped_.instance, serializer);
  FollowReferent(serializer.TakeReferent());
}

void DumpWatch::FollowReferent(value::ObjectRef now) {
  if (now.IsBitIdentical(referent_)) {
    return;
  }
  referent_membership_.Unlink();
  referent_ = std::move(now);
  Trigger watched;
  if (referent_) {
    watched.observable = &ObjectRootOf(referent_)->EventSource();
  }
  EnrolOnPlace(referent_membership_, this, watched);
}

void DumpWatch::Reached() {
  if (changed_) {
    return;
  }
  changed_ = true;
  dump_->Changed(*this);
}

namespace {

struct ScopeCommand {
  DumpScopeKind kind;
  std::string name;
};
struct UpscopeCommand {};
// The name a variable is declared under is a constant of the class declaring
// it, which outlives the dump.
struct VarCommand {
  DumpCode code;
  DumpVarDeclaration variable;
};
struct TimescaleCommand {
  std::int8_t precision_power;
};
struct EndDefinitionsCommand {};
struct TimeCommand {
  SimTime time;
};
// The changes the unit's capture holds.
struct ChangesCommand {};
struct BeginCheckpointCommand {
  DumpCheckpoint checkpoint;
};
struct EndCheckpointCommand {};
struct LimitCommand {
  std::uint64_t bytes;
};
struct FlushCommand {};
struct CloseCommand {};

using DumpCommand = std::variant<
    ScopeCommand, UpscopeCommand, VarCommand, TimescaleCommand,
    EndDefinitionsCommand, TimeCommand, ChangesCommand, BeginCheckpointCommand,
    EndCheckpointCommand, LimitCommand, FlushCommand, CloseCommand>;

// What the simulation records between two points where it is whole: the
// definitions, a time step's changes, a checkpoint, a bound, a flush, the
// end. At most one capture of values is among it.
struct DumpUnit {
  std::vector<DumpCommand> commands;
  DumpCapture capture;
  bool captured = false;
};

// A pipe one side writes a byte into to have the other look again. Both
// `write` and `read` are async-signal-safe (POSIX.1-2024, 2.4.3), so it is
// the one way to wake a thread that a signal handler and a thread may both
// take, and to wait in a handler for one; a pipe whose writing end is closed
// wakes every reader at once, for good.
class Wakeup {
 public:
  // A wake-up on a pipe of its own, or why the system made none.
  static auto Open() -> std::expected<Wakeup, std::error_code> {
    std::array<int, 2> ends{};
    if (::pipe2(ends.data(), O_CLOEXEC) != 0) {
      return std::unexpected(base::ErrnoAsErrorCode());
    }
    return Wakeup(ends[0], ends[1]);
  }
  Wakeup(const Wakeup&) = delete;
  auto operator=(const Wakeup&) -> Wakeup& = delete;
  Wakeup(Wakeup&& other) noexcept
      : read_end_(std::exchange(other.read_end_, -1)),
        write_end_(std::exchange(other.write_end_, -1)) {
  }
  auto operator=(Wakeup&&) -> Wakeup& = delete;
  ~Wakeup() {
    if (read_end_ >= 0) {
      ::close(read_end_);
    }
    CloseForGood();
  }

  // Wakes one reader. A byte is written only where a side is known to wait,
  // or once by an ending, so the pipe never holds more than a few and the
  // write never blocks.
  void Wake() const noexcept {
    const char byte = 0;
    base::WriteAsMuchAsTaken(write_end_, std::span(&byte, 1));
  }

  // Wakes every reader now and from now on.
  void CloseForGood() noexcept {
    if (write_end_ >= 0) {
      ::close(std::exchange(write_end_, -1));
    }
  }

  // Returns once woken, or once the pipe is closed for good.
  void Await() const noexcept {
    char byte = 0;
    while (::read(read_end_, &byte, 1) < 0 && errno == EINTR) {
    }
  }

  // Returns once the pipe is closed for good, whatever wakes it before.
  void AwaitClosedForGood() const noexcept {
    char byte = 0;
    while (true) {
      const ssize_t got = ::read(read_end_, &byte, 1);
      if (got > 0 || (got < 0 && errno == EINTR)) continue;
      return;
    }
  }

 private:
  Wakeup(int read_end, int write_end)
      : read_end_(read_end), write_end_(write_end) {
  }

  int read_end_ = -1;
  int write_end_ = -1;
};

}  // namespace

// What one dump records, on its way from the simulation to the thread that
// writes the file, and that thread.
//
// The simulation fills one unit at a time and publishes it whole by counting
// it, so whoever reads the count reads only units nothing is still filling --
// what a file's finished bytes are to an ending (`File`), a published unit is
// here. A ring of them stands between the two sides, so a writer that falls
// behind makes the simulation wait rather than grow, and the writer is woken
// once enough units wait to be worth it, or one must be written now.
//
// Nothing between the two sides takes a lock. An ending reaches this from a
// signal handler, which may have stopped the simulation anywhere, holding
// anything; it may only set a flag, write a byte and read one. The writer it
// hands the dump to encodes, and so allocates, from its own thread's arena: a
// refused request for memory therefore finishes no dump, and an ending that
// stopped the simulation inside the allocator itself can leave the writer
// waiting on it, until a stop from outside ends the process.
class DumpOutput {
 public:
  // The output of a dump written by `writer`, its thread started, or why the
  // system would not give it what it waits on.
  static auto Start(std::unique_ptr<DumpWriter> writer)
      -> std::expected<std::unique_ptr<DumpOutput>, std::error_code> {
    auto to_writer = Wakeup::Open();
    auto to_simulation = Wakeup::Open();
    auto finished = Wakeup::Open();
    for (const auto* opened : {&to_writer, &to_simulation, &finished}) {
      if (!opened->has_value()) {
        return std::unexpected(opened->error());
      }
    }
    return std::unique_ptr<DumpOutput>(new DumpOutput(
        std::move(writer), *std::move(to_writer), *std::move(to_simulation),
        *std::move(finished)));
  }
  DumpOutput(const DumpOutput&) = delete;
  auto operator=(const DumpOutput&) -> DumpOutput& = delete;
  DumpOutput(DumpOutput&&) = delete;
  auto operator=(DumpOutput&&) -> DumpOutput& = delete;
  ~DumpOutput() {
    if (thread_.joinable()) {
      Close();
    }
  }

  void Send(DumpCommand command) {
    Filling().commands.push_back(std::move(command));
  }

  // The capture the unit's values go into, recorded by `ChangesCommand`.
  auto Capture() -> DumpCapture& {
    DumpUnit& unit = Filling();
    if (unit.captured) {
      throw InternalError("a value dump unit holds a second capture");
    }
    unit.captured = true;
    return unit.capture;
  }

  // The unit being filled is whole.
  void Publish() {
    PublishFilling(false);
  }

  // Returns once everything published before it is in the file.
  void Flush() {
    Send(FlushCommand{});
    PublishFilling(true);
    AwaitWriter([&] { return written_.load() >= filling_; });
  }

  // Returns once the file is ended and its thread gone.
  void Close() {
    Send(CloseCommand{});
    PublishFilling(true);
    thread_.join();
  }

  // What the system refused, once the writer has met it; nothing before.
  [[nodiscard]] auto Refused() const -> std::error_code {
    if (!refused_seen_.load(std::memory_order_acquire)) {
      return {};
    }
    return refused_;
  }

  // A defect the writer met, raised where the simulation next hands it work.
  void RethrowBroken() const {
    if (broken_seen_.load(std::memory_order_acquire)) {
      std::rethrow_exception(broken_);
    }
  }

  // The ending's half: everything published is written, the file ended at
  // `now`, and only then does this return. On the writer's own thread there
  // is no one left to finish it, and it is left as it stands.
  void FinishAtEnding(SimTime now) noexcept {
    if (WrittenHere() == this) {
      return;
    }
    finish_at_.store(now);
    finishing_.store(true);
    // Woken whether or not it is seen to sleep: the stop may have landed
    // between the simulation finding the writer asleep and waking it, and a
    // spare byte only makes the writer look again.
    to_writer_.Wake();
    finished_.AwaitClosedForGood();
  }

 private:
  DumpOutput(
      std::unique_ptr<DumpWriter> writer, Wakeup to_writer,
      Wakeup to_simulation, Wakeup finished)
      : writer_(std::move(writer)),
        units_(kUnits),
        to_writer_(std::move(to_writer)),
        to_simulation_(std::move(to_simulation)),
        finished_(std::move(finished)),
        thread_([this] { Run(); }) {
  }

  // The writer may fall behind by a burst of time steps before the simulation
  // waits for it, and is woken for a quarter of that, so a wake-up's system
  // call is paid once for many steps.
  static constexpr std::uint64_t kUnits = 1024;
  static constexpr std::uint64_t kWakeAfter = 256;

  // The dump whose file the calling thread writes; none on any other thread.
  static auto WrittenHere() noexcept -> const DumpOutput*& {
    thread_local constinit const DumpOutput* written_here = nullptr;
    return written_here;
  }

  // The unit the simulation fills, made empty where it starts, once the
  // writer is done with what the slot held last.
  auto Filling() -> DumpUnit& {
    DumpUnit& unit = units_[filling_ % kUnits];
    if (!filling_started_) {
      AwaitWriter([&] { return filling_ - written_.load() < kUnits; });
      unit.commands.clear();
      unit.capture.Clear();
      unit.captured = false;
      filling_started_ = true;
    }
    return unit;
  }

  void PublishFilling(bool now) {
    Filling();
    filling_started_ = false;
    ++filling_;
    published_.store(filling_);
    if (now) {
      urgent_.store(filling_);
    }
    if (now || filling_ - written_.load() >= kWakeAfter) {
      WakeWriter();
    }
  }

  void WakeWriter() noexcept {
    if (writer_sleeps_.exchange(false)) {
      to_writer_.Wake();
    }
  }

  // Waits on the writer until `ready` holds, waking it first: what it waits
  // for may be fewer units than wake it on their own.
  template <typename Ready>
  void AwaitWriter(Ready ready) {
    while (!ready()) {
      simulation_waits_.store(true);
      if (ready()) {
        simulation_waits_.store(false);
        return;
      }
      WakeWriter();
      to_simulation_.Await();
    }
  }

  void Run() {
    // A stop from outside is handled by the thread running the design, never
    // by this one: the ending it starts waits on this thread to end the file.
    const StopSignalsHeld stop_signals_held;
    WrittenHere() = this;
    std::uint64_t written = 0;
    while (true) {
      // Read before the count, so that an ending seen here sees every unit
      // published before it.
      const bool finishing = finishing_.load();
      const std::uint64_t ready = published_.load();
      bool closed = false;
      for (; written < ready; ++written) {
        closed = Write(units_[written % kUnits]) || closed;
        written_.store(written + 1);
        if (simulation_waits_.exchange(false)) {
          to_simulation_.Wake();
        }
      }
      if (closed) {
        break;
      }
      if (finishing) {
        FinishAt(finish_at_.load());
        break;
      }
      writer_sleeps_.store(true);
      if (finishing_.load() || urgent_.load() > written ||
          published_.load() - written >= kWakeAfter) {
        writer_sleeps_.store(false);
        continue;
      }
      to_writer_.Await();
      writer_sleeps_.store(false);
    }
    finished_.CloseForGood();
  }

  // Writes one unit, answering whether it ended the file.
  auto Write(DumpUnit& unit) -> bool {
    bool closed = false;
    for (DumpCommand& command : unit.commands) {
      closed = Write(command, unit) || closed;
    }
    return closed;
  }

  // Writes one command, answering whether it ended the file. What follows a
  // refusal or a defect is not written.
  auto Write(DumpCommand& command, const DumpUnit& unit) -> bool {
    return std::visit(
        Overloaded{
            [&](const ScopeCommand& c) {
              Attempt([&] { writer_->Scope(c.kind, c.name); });
              return false;
            },
            [&](const UpscopeCommand&) {
              Attempt([&] { writer_->Upscope(); });
              return false;
            },
            [&](const VarCommand& c) {
              Attempt([&] { writer_->Var(c.code, c.variable); });
              return false;
            },
            [&](const TimescaleCommand& c) {
              Attempt([&] { writer_->Timescale(c.precision_power); });
              return false;
            },
            [&](const EndDefinitionsCommand&) {
              Attempt([&] { writer_->EndDefinitions(); });
              defined_ = true;
              return false;
            },
            [&](const TimeCommand& c) {
              WriteTime(c.time);
              return false;
            },
            [&](const ChangesCommand&) {
              Attempt([&] { WriteChanges(unit.capture); });
              return false;
            },
            [&](const BeginCheckpointCommand& c) {
              WhileRoom([&] { writer_->BeginCheckpoint(c.checkpoint); });
              return false;
            },
            [&](const EndCheckpointCommand&) {
              WhileRoom([&] { writer_->EndCheckpoint(); });
              return false;
            },
            [&](const LimitCommand& c) {
              Attempt([&] { writer_->LimitTo(c.bytes); });
              return false;
            },
            [&](const FlushCommand&) {
              Attempt([&] { writer_->Flush(); });
              return false;
            },
            [&](const CloseCommand&) {
              Attempt([&] { writer_->Close(); });
              return true;
            }},
        command);
  }

  // A time step is named once, however many of its records follow: a change,
  // a checkpoint and the file's end in one step are all under the one `#time`
  // (LRM 21.7.2.1).
  void WriteTime(SimTime time) {
    if (time_written_ == time) {
      return;
    }
    time_written_ = time;
    WhileRoom([&] { writer_->Time(time); });
  }

  // The file ends where the run was, as a close at the end of the run ends
  // it, once its definitions are there for a time to follow. An ending on a
  // thread other than the simulation's leaves the simulation running, which
  // may have published a later step since, and a file's time never goes back.
  void FinishAt(SimTime now) {
    if (defined_ && (!time_written_.has_value() || now > *time_written_)) {
      WriteTime(now);
    }
    Attempt([&] { writer_->Close(); });
  }

  // What the file records only while it has room for more.
  template <class Body>
  void WhileRoom(Body body) {
    if (!full_) {
      Attempt(body);
    }
  }

  // LRM 21.7.1.5: once the file reaches its bound, dumping stops, which the
  // file records as a comment.
  void WriteChanges(const DumpCapture& capture) {
    for (const DumpCapture::Change& change : capture.Changes()) {
      if (full_) {
        return;
      }
      writer_->Change(change.code, capture.Value(change));
      if (writer_->ReachedLimit()) {
        writer_->Comment("dump limit reached");
        full_ = true;
      }
    }
  }

  // Runs `body` unless something before it went wrong, and keeps what went
  // wrong in it for the simulation.
  template <class Body>
  void Attempt(Body body) {
    if (stopped_) {
      return;
    }
    try {
      body();
    } catch (const InternalError&) {
      stopped_ = true;
      broken_ = std::current_exception();
      broken_seen_.store(true, std::memory_order_release);
      return;
    }
    if (const std::error_code refused = writer_->Refused()) {
      stopped_ = true;
      refused_ = refused;
      refused_seen_.store(true, std::memory_order_release);
    }
  }

  std::unique_ptr<DumpWriter> writer_;
  std::vector<DumpUnit> units_;

  // Read and written by the simulation alone.
  std::uint64_t filling_ = 0;
  bool filling_started_ = false;

  // Read and written by the writer alone.
  bool stopped_ = false;
  bool defined_ = false;
  bool full_ = false;
  std::optional<SimTime> time_written_;

  // Shared, each written by one side: the counts, and what the writer met,
  // written before the flag that says so.
  std::atomic<std::uint64_t> published_ = 0;
  std::atomic<std::uint64_t> urgent_ = 0;
  std::atomic<std::uint64_t> written_ = 0;
  std::error_code refused_;
  std::atomic<bool> refused_seen_ = false;
  std::exception_ptr broken_;
  std::atomic<bool> broken_seen_ = false;
  // Which side waits for the other, each set by the waiting side and taken
  // by the one that wakes it.
  std::atomic<bool> writer_sleeps_ = false;
  std::atomic<bool> simulation_waits_ = false;
  // Set by an ending.
  std::atomic<bool> finishing_ = false;
  std::atomic<SimTime> finish_at_ = 0;

  Wakeup to_writer_;
  Wakeup to_simulation_;
  // Closed for good once the file is ended.
  Wakeup finished_;
  std::thread thread_;
};

namespace {

// LRM 21.7.1.3: the checkpoint `$dumpoff` makes records every thing as
// unknown; every other one records each as it stands.
auto RecordsUnknown(DumpCheckpoint checkpoint) -> bool {
  switch (checkpoint) {
    case DumpCheckpoint::kDumpoff:
      return true;
    case DumpCheckpoint::kDumpvars:
    case DumpCheckpoint::kDumpall:
    case DumpCheckpoint::kDumpon:
      return false;
  }
  throw InternalError("dump: unknown checkpoint");
}

// Declares the selected part of the design to a dump: each package and
// compilation unit's scope, then every instance from the tops down in the
// order the tree holds them, each data object a scope declares in the order it
// declares them, and a scope only where something below it is recorded. Every
// name is recorded under a code of its own.
class SelectionDeclarer final {
 public:
  SelectionDeclarer(
      DumpOutput& output, ValueDump& dump,
      std::vector<std::unique_ptr<DumpWatch>>& watches)
      : output_(&output), dump_(&dump), watches_(&watches) {
  }

  void Walk(
      const DumpSelection& selection, Scope& root,
      std::span<const abi::ScopeDeclarations* const> namespaces) {
    for (const DumpSelection::Instances& named : selection.instances) {
      std::int64_t& levels = levels_[named.scope];
      levels = std::max(levels, LevelsNamed(named.levels));
    }
    variables_ = selection.variables;
    // A package and the compilation unit's scope each stand beside the tops
    // (LRM 37.10), declared first, as their storage comes up first.
    for (const abi::ScopeDeclarations* declared : namespaces) {
      WalkNamespace(
          *declared, std::ranges::contains(selection.namespaces, declared));
    }
    // A top's own scope is the outermost a dump declares: the root above the
    // tops is no scope of the design (LRM 23.3.1).
    root.ForEachChild([&](Scope& top) { WalkScope(top, 0); });
  }

 private:
  // Declares `object`, which the instance `scope` declares or, where there is
  // none, a namespace keeps, and records it from now on, where the selection
  // names it.
  void Declare(Scope* scope, const abi::DataObjectRecord& object) {
    const DumpedObject dumped{.instance = scope, .object = &object};
    if (!Records(dumped)) {
      return;
    }
    MoveToScopesAround(object);
    OpenPending();
    const DumpCode code{static_cast<std::uint32_t>(watches_->size())};
    output_->Send(
        VarCommand{.code = code, .variable = DumpVarDeclarationOf(object)});
    watches_->push_back(std::make_unique<DumpWatch>(*dump_, code, dumped));
  }

  // Leaves the named scopes the last thing recorded lay in that this one does
  // not, and enters those it lies in that the last did not.
  void MoveToScopesAround(const abi::DataObjectRecord& object) {
    // A scope the source gave no name is one a dump declares no scope for (LRM
    // 21.7.2.3).
    const std::vector<const abi::LexicalScopeRecord*> around =
        NamedEnclosingScopes(object.enclosing);
    const auto common = static_cast<std::size_t>(
        std::ranges::mismatch(blocks_, around).in1 - blocks_.begin());
    LeaveBlocksDownTo(common);
    for (std::size_t i = common; i < around.size(); ++i) {
      blocks_.push_back(around[i]);
      pending_.push_back(
          {.kind = DumpScopeKindOf(around[i]->Kind()),
           .name = around[i]->name});
    }
  }

  void LeaveBlocksDownTo(std::size_t count) {
    while (blocks_.size() > count) {
      blocks_.pop_back();
      CloseScope();
    }
  }

  // `remaining` is how many levels of instances from this scope down are
  // recorded whole (LRM 21.7.1.2).
  void WalkScope(Scope& scope, std::int64_t remaining) {
    if (const auto named = levels_.find(&scope); named != levels_.end()) {
      remaining = std::max(remaining, named->second);
    }
    pending_.push_back(
        {.kind = DumpScopeKindOf(scope.Segment().Declaration().Kind()),
         .name = scope.DisplaySegment()});
    whole_ = remaining > 0;
    DeclareAll(&scope, scope.Declarations());
    scope.ForEachChild(
        [&](Scope& child) { WalkScope(child, LevelsBelow(child, remaining)); });
    CloseScope();
  }

  // A package, and the compilation unit's scope, which VPI shows as one (LRM
  // 37.10 detail 5), is recorded as a package under its definition's name.
  void WalkNamespace(const abi::ScopeDeclarations& declared, bool whole) {
    pending_.push_back(
        {.kind = DumpScopeKind::kPackage, .name = declared.definition->name});
    whole_ = whole;
    DeclareAll(nullptr, declared);
    CloseScope();
  }

  // Declares each data object `declared` states that the selection names.
  void DeclareAll(Scope* scope, const abi::ScopeDeclarations& declared) {
    if (!whole_ && variables_.empty()) {
      return;
    }
    for (const abi::DataObjectRecord* object :
         declared.data_objects.Entries()) {
      Declare(scope, *object);
    }
    LeaveBlocksDownTo(0);
  }

  // Whether the selection names `dumped`: every object where it records
  // whole, and otherwise only those it named, whose identity is asked of the
  // object only then.
  [[nodiscard]] auto Records(const DumpedObject& dumped) const -> bool {
    if (whole_) {
      return true;
    }
    const void* identity = DumpIdentityOf(dumped);
    return identity != nullptr &&
           std::ranges::find(variables_, identity) != variables_.end();
  }

  // Declares every scope entered since the last declaration, which something
  // recorded is now known to lie in.
  void OpenPending() {
    for (; opened_ < pending_.size(); ++opened_) {
      output_->Send(
          ScopeCommand{
              .kind = pending_[opened_].kind, .name = pending_[opened_].name});
    }
  }

  void CloseScope() {
    if (opened_ == pending_.size()) {
      output_->Send(UpscopeCommand{});
      --opened_;
    }
    pending_.pop_back();
  }

  DumpOutput* output_;
  ValueDump* dump_;
  std::vector<std::unique_ptr<DumpWatch>>* watches_;
  std::unordered_map<const Scope*, std::int64_t> levels_;
  std::vector<const void*> variables_;
  std::vector<ScopeCommand> pending_;
  // The named scopes of the instance being described that the last thing
  // recorded lay in, outermost first, each among `pending_`.
  std::vector<const abi::LexicalScopeRecord*> blocks_;
  std::size_t opened_ = 0;
  bool whole_ = false;
};

// Declares the ports of the instances an extended dump names (LRM 21.7.3.1):
// each instance a module scope named by its hierarchical name, and in it each
// port, in the order the declaration gives them, with a code of its own -- a
// port's values are read by its direction, so two ports connected to one net
// are two things. What a named block, task or function inside declares is no
// port, and neither is an event, which the format has no value for.
class PortDeclarer final {
 public:
  PortDeclarer(
      DumpOutput& output, ValueDump& dump,
      std::vector<std::unique_ptr<DumpWatch>>& watches)
      : output_(&output), dump_(&dump), watches_(&watches) {
  }

  void Walk(const PortSelection& selection) {
    for (Scope* instance : selection.instances) {
      const std::span<const abi::DescribedPort* const> ports =
          instance->Declarations().ports.Entries();
      bool opened = false;
      for (std::size_t position = 0; position < ports.size(); ++position) {
        const abi::DescribedPort& port = *ports[position];
        const std::optional<DumpVarDeclaration> declared =
            DumpPortDeclarationOf(port, static_cast<std::uint32_t>(position));
        if (!declared.has_value()) {
          continue;
        }
        if (!opened) {
          output_->Send(
              ScopeCommand{
                  .kind = DumpScopeKind::kModule,
                  .name = std::string(instance->HierarchicalPath().View())});
          opened = true;
        }
        const DumpCode code{static_cast<std::uint32_t>(watches_->size())};
        output_->Send(VarCommand{.code = code, .variable = *declared});
        watches_->push_back(
            std::make_unique<DumpWatch>(
                *dump_, code,
                DumpedObject{.instance = instance, .object = port.connects}));
      }
      if (opened) {
        output_->Send(UpscopeCommand{});
      }
    }
  }

 private:
  DumpOutput* output_;
  ValueDump* dump_;
  std::vector<std::unique_ptr<DumpWatch>>* watches_;
};

}  // namespace

void RaiseForWhoeverAsked(
    base::SourceLocation asked_at, base::DiagMessage message) {
  if (!asked_at.IsValid()) {
    throw SimulationError(base::kNoSourceLocation, std::move(message));
  }
  const base::SourceLocationScope bound(asked_at);
  throw SimulationError(std::move(message));
}

ValueDump::ValueDump(
    std::unique_ptr<DumpWriter> writer, std::int8_t precision_power,
    std::string file, base::SourceLocation asked_at)
    : file_(std::move(file)), asked_at_(asked_at) {
  auto output = DumpOutput::Start(std::move(writer));
  if (!output.has_value()) {
    RaiseForWhoeverAsked(
        asked_at_, base::DiagMessage(
                       "the dump file '{}' could not be written ({})", file_,
                       output.error().message()));
  }
  output_ = *std::move(output);
  output_->Send(TimescaleCommand{.precision_power = precision_power});
}

ValueDump::~ValueDump() = default;

void ValueDump::Start(
    const DumpSelection& selection, Scope& root,
    std::span<const abi::ScopeDeclarations* const> namespaces, SimTime now) {
  SelectionDeclarer(*output_, *this, watches_)
      .Walk(selection, root, namespaces);
  Begin(now);
}

void ValueDump::Start(const PortSelection& selection, SimTime now) {
  PortDeclarer(*output_, *this, watches_).Walk(selection);
  Begin(now);
}

// The definitions end and the first values are recorded.
void ValueDump::Begin(SimTime now) {
  output_->Send(EndDefinitionsCommand{});
  Checkpoint(DumpCheckpoint::kDumpvars, now);
}

void ValueDump::Changed(DumpWatch& watch) {
  changed_.push_back(&watch);
  RuntimeEffects& runtime = current_runtime();
  const SimTime now = runtime.Now();
  if (step_submitted_for_ == now) {
    return;
  }
  step_submitted_for_ = now;
  runtime.Submit(
      now, support::Region::kPostponed, [this, now] { EndStep(now); });
}

void ValueDump::EndStep(SimTime now) {
  if (changed_.empty()) {
    return;
  }
  if (!on_) {
    for (DumpWatch* watch : changed_) {
      watch->TakeChanged();
    }
    changed_.clear();
    return;
  }
  At(now);
  DumpCapture& capture = output_->Capture();
  for (DumpWatch* watch : changed_) {
    watch->TakeChanged();
    watch->Read(capture);
  }
  changed_.clear();
  output_->Send(ChangesCommand{});
  Publish();
}

void ValueDump::Off(SimTime now) {
  if (!on_) {
    return;
  }
  Checkpoint(DumpCheckpoint::kDumpoff, now);
  on_ = false;
}

void ValueDump::On(SimTime now) {
  if (on_) {
    return;
  }
  on_ = true;
  Checkpoint(DumpCheckpoint::kDumpon, now);
}

void ValueDump::All(SimTime now) {
  if (!on_) {
    return;
  }
  Checkpoint(DumpCheckpoint::kDumpall, now);
}

void ValueDump::At(SimTime now) {
  output_->Send(TimeCommand{.time = now});
}

void ValueDump::Limit(std::uint64_t bytes) {
  output_->Send(LimitCommand{.bytes = bytes});
}

void ValueDump::Flush() {
  output_->Flush();
  RaiseIfRefused();
}

void ValueDump::Close(SimTime now) {
  At(now);
  output_->Close();
  RaiseIfRefused();
}

void ValueDump::FinishAtEnding(SimTime now) noexcept {
  output_->FinishAtEnding(now);
}

void ValueDump::Publish() {
  output_->Publish();
  RaiseIfRefused();
}

// Found by the thread writing the file, and said here, where the simulation
// next hands it something.
void ValueDump::RaiseIfRefused() {
  output_->RethrowBroken();
  const std::error_code refused = output_->Refused();
  if (!refused || std::exchange(refusal_raised_, true)) {
    return;
  }
  RaiseForWhoeverAsked(
      asked_at_, base::DiagMessage(
                     "the dump file '{}' could not be written ({})", file_,
                     refused.message()));
}

// A checkpoint records every thing as it stands, which is where each change
// made before it in the time step has already left it; those are not written
// again when the step ends.
void ValueDump::Checkpoint(DumpCheckpoint checkpoint, SimTime now) {
  const bool unknown = RecordsUnknown(checkpoint);
  for (DumpWatch* watch : changed_) {
    watch->TakeChanged();
  }
  changed_.clear();
  At(now);
  output_->Send(BeginCheckpointCommand{.checkpoint = checkpoint});
  DumpCapture& capture = output_->Capture();
  for (const std::unique_ptr<DumpWatch>& watch : watches_) {
    if (unknown) {
      capture.Unknown(watch->Code());
    } else {
      watch->Read(capture);
    }
  }
  output_->Send(ChangesCommand{});
  output_->Send(EndCheckpointCommand{});
  Publish();
}

}  // namespace lyra::runtime
