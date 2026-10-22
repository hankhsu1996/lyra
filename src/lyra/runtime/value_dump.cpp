#include "lyra/runtime/value_dump.hpp"

#include <algorithm>
#include <condition_variable>
#include <cstdint>
#include <deque>
#include <exception>
#include <future>
#include <limits>
#include <memory>
#include <mutex>
#include <optional>
#include <span>
#include <string>
#include <thread>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/base/time.hpp"
#include "lyra/runtime/dump_writer.hpp"
#include "lyra/runtime/object_change.hpp"
#include "lyra/runtime/object_ref.hpp"
#include "lyra/runtime/region.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/scope.hpp"
#include "lyra/runtime/value_dumps.hpp"
#include "lyra/support/dump_kind.hpp"
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

DumpProbe::DumpProbe() = default;
DumpProbe::~DumpProbe() = default;

auto DumpProbe::Referent() const -> value::ObjectRef {
  return nullptr;
}
Describer::~Describer() = default;

void Describer::BeginBlock(const DumpBlock& block) {
  enclosing_.push_back(&block);
  Entered();
}

void Describer::EndBlock() {
  Leaving();
  enclosing_.pop_back();
}

DumpWatch::DumpWatch(
    ValueDump& dump, DumpCode code, std::unique_ptr<DumpProbe> probe)
    : dump_(&dump), code_(code), probe_(std::move(probe)) {
  EnrolOnPlace(membership_, this, probe_->Watched());
  FollowReferent();
}

DumpWatch::~DumpWatch() = default;

void DumpWatch::FollowReferent() {
  value::ObjectRef now = probe_->Referent();
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
  // What the thing refers to moves only with a change of the thing, which is
  // a change whether or not one was already seen this time step.
  FollowReferent();
  if (changed_) {
    return;
  }
  changed_ = true;
  dump_->Changed(*this);
}

namespace {

struct ScopeCommand {
  support::DumpScopeKind kind;
  std::string name;
};
struct UpscopeCommand {};
// The entry is a constant of the class it describes, which outlives the dump.
struct VarCommand {
  DumpCode code;
  DumpEntry entry;
};
struct AliasCommand {
  DumpCode code;
  DumpEntry entry;
};
struct TimescaleCommand {
  std::int8_t precision_power;
};
struct EndDefinitionsCommand {};
struct TimeCommand {
  SimTime time;
};
// The changes of one capture of the batch, by its position there.
struct ChangesCommand {
  std::size_t capture;
};
struct BeginCheckpointCommand {
  DumpCheckpoint checkpoint;
};
struct EndCheckpointCommand {};
struct LimitCommand {
  std::uint64_t bytes;
};
struct FlushCommand {
  std::shared_ptr<std::promise<void>> done;
};
struct CloseCommand {};

using DumpCommand = std::variant<
    ScopeCommand, UpscopeCommand, VarCommand, AliasCommand, TimescaleCommand,
    EndDefinitionsCommand, TimeCommand, ChangesCommand, BeginCheckpointCommand,
    EndCheckpointCommand, LimitCommand, FlushCommand, CloseCommand>;

}  // namespace

// What one dump hands its writing thread at a time, and that thread. A batch
// is handed over once it holds enough values to be worth the hand-off, or
// where the simulation has to wait for the file; no more than a few are in
// flight, so a writer that falls behind makes the simulation wait rather than
// grow.
class DumpOutput {
 public:
  explicit DumpOutput(std::unique_ptr<DumpWriter> writer)
      : writer_(std::move(writer)), thread_([this] { Run(); }) {
  }
  DumpOutput(const DumpOutput&) = delete;
  auto operator=(const DumpOutput&) -> DumpOutput& = delete;
  DumpOutput(DumpOutput&&) = delete;
  auto operator=(DumpOutput&&) -> DumpOutput& = delete;
  ~DumpOutput() {
    if (thread_.joinable()) {
      Send(CloseCommand{});
      HandOver();
      thread_.join();
    }
  }

  void Send(DumpCommand command) {
    current_.commands.push_back(std::move(command));
  }

  // The capture the next step's changes go into.
  auto NextCapture() -> DumpCapture& {
    if (current_.used == current_.captures.size()) {
      current_.captures.emplace_back();
    }
    DumpCapture& capture = current_.captures[current_.used];
    capture.Clear();
    return capture;
  }
  // Records the capture last handed out as the step's changes.
  void SendCapture() {
    Send(ChangesCommand{.capture = current_.used});
    ++current_.used;
    if (current_.used >= kCapturesPerBatch) {
      HandOver();
    }
  }

  void Flush() {
    auto done = std::make_shared<std::promise<void>>();
    std::future<void> written = done->get_future();
    Send(FlushCommand{.done = std::move(done)});
    HandOver();
    written.wait();
    RethrowFailure();
  }

  void Close() {
    Send(CloseCommand{});
    HandOver();
    thread_.join();
    RethrowFailure();
  }

 private:
  static constexpr std::size_t kCapturesPerBatch = 256;
  static constexpr std::size_t kBatchesInFlight = 4;

  struct Batch {
    std::vector<DumpCommand> commands;
    std::vector<DumpCapture> captures;
    std::size_t used = 0;
  };

  void HandOver() {
    std::unique_lock lock(mutex_);
    space_.wait(lock, [&] { return pending_.size() < kBatchesInFlight; });
    pending_.push_back(std::move(current_));
    ready_.notify_one();
    if (!spare_.empty()) {
      current_ = std::move(spare_.back());
      spare_.pop_back();
    } else {
      current_ = Batch{};
    }
    current_.commands.clear();
    current_.used = 0;
  }

  void RethrowFailure() {
    std::lock_guard lock(mutex_);
    if (failure_ != nullptr) {
      std::rethrow_exception(std::exchange(failure_, nullptr));
    }
  }

  void Run() {
    for (bool open = true; open;) {
      Batch batch;
      {
        std::unique_lock lock(mutex_);
        ready_.wait(lock, [&] { return !pending_.empty(); });
        batch = std::move(pending_.front());
        pending_.pop_front();
        space_.notify_one();
      }
      for (DumpCommand& command : batch.commands) {
        open = Write(command, batch) && open;
      }
      std::lock_guard lock(mutex_);
      spare_.push_back(std::move(batch));
    }
  }

  // Writes one command, answering whether the file is still open. A failure
  // to write is kept for the simulation to report where it next waits on the
  // file, and nothing more is written.
  auto Write(DumpCommand& command, const Batch& batch) -> bool {
    return std::visit(
        Overloaded{
            [&](const ScopeCommand& c) {
              Attempt([&] { writer_->Scope(c.kind, c.name); });
              return true;
            },
            [&](const UpscopeCommand&) {
              Attempt([&] { writer_->Upscope(); });
              return true;
            },
            [&](const VarCommand& c) {
              Attempt([&] { writer_->Var(c.code, c.entry); });
              return true;
            },
            [&](const AliasCommand& c) {
              Attempt([&] { writer_->Alias(c.code, c.entry); });
              return true;
            },
            [&](const TimescaleCommand& c) {
              Attempt([&] { writer_->Timescale(c.precision_power); });
              return true;
            },
            [&](const EndDefinitionsCommand&) {
              Attempt([&] { writer_->EndDefinitions(); });
              return true;
            },
            [&](const TimeCommand& c) {
              WhileRoom([&] { writer_->Time(c.time); });
              return true;
            },
            [&](const ChangesCommand& c) {
              Attempt([&] { WriteChanges(batch.captures[c.capture]); });
              return true;
            },
            [&](const BeginCheckpointCommand& c) {
              WhileRoom([&] { writer_->BeginCheckpoint(c.checkpoint); });
              return true;
            },
            [&](const EndCheckpointCommand&) {
              WhileRoom([&] { writer_->EndCheckpoint(); });
              return true;
            },
            [&](const LimitCommand& c) {
              Attempt([&] { writer_->LimitTo(c.bytes); });
              return true;
            },
            [&](FlushCommand& c) {
              Attempt([&] { writer_->Flush(); });
              c.done->set_value();
              return true;
            },
            [&](const CloseCommand&) {
              Attempt([&] { writer_->Close(); });
              return false;
            }},
        command);
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

  template <class Body>
  void Attempt(Body body) {
    if (failed_) {
      return;
    }
    try {
      body();
    } catch (const SimulationError&) {
      Fail();
    } catch (const InternalError&) {
      Fail();
    }
  }

  void Fail() {
    failed_ = true;
    std::lock_guard lock(mutex_);
    failure_ = std::current_exception();
  }

  std::unique_ptr<DumpWriter> writer_;
  Batch current_;
  std::mutex mutex_;
  std::condition_variable ready_;
  std::condition_variable space_;
  std::deque<Batch> pending_;
  std::vector<Batch> spare_;
  std::exception_ptr failure_;
  // Read and written by the writing thread alone.
  bool failed_ = false;
  bool full_ = false;
  std::thread thread_;
};

namespace {

auto UnboundedLevels() -> std::int64_t {
  return std::numeric_limits<std::int64_t>::max();
}

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

// Describes the selected part of the design to a dump: every instance, from
// the tops down, in the order the tree holds them, declaring a scope only where
// something below it is recorded, and each thing once, a second name for it
// being an alias (LRM 21.7.2.3 `$var` b).
class SelectionDescriber final : public Describer {
 public:
  SelectionDescriber(
      DumpOutput& output, ValueDump& dump,
      std::vector<std::unique_ptr<DumpWatch>>& watches)
      : output_(&output), dump_(&dump), watches_(&watches) {
  }

  void Walk(const DumpSelection& selection, std::span<Scope* const> roots) {
    const bool whole =
        selection.instances.empty() && selection.variables.empty();
    for (const DumpSelection::Instances& named : selection.instances) {
      std::int64_t& levels = levels_[named.scope];
      levels = std::max(
          levels, named.levels == 0 ? UnboundedLevels() : named.levels);
    }
    variables_ = selection.variables;
    // A top's own scope is the outermost a dump declares: the root above the
    // tops is no scope of the design (LRM 23.3.1).
    for (Scope* root : roots) {
      root->ForEachChild(
          [&](Scope& top) { WalkScope(top, whole ? UnboundedLevels() : 0); });
    }
  }

  void Var(const DumpEntry& entry, std::unique_ptr<DumpProbe> probe) override {
    if (!Records(probe->Member())) {
      return;
    }
    OpenPending();
    const auto [known, inserted] =
        codes_.try_emplace(probe->Identity(), DumpCode{});
    if (!inserted) {
      output_->Send(AliasCommand{.code = known->second, .entry = entry});
      return;
    }
    const DumpCode code{static_cast<std::uint32_t>(watches_->size())};
    known->second = code;
    output_->Send(VarCommand{.code = code, .entry = entry});
    watches_->push_back(
        std::make_unique<DumpWatch>(*dump_, code, std::move(probe)));
  }

 private:
  void Entered() override {
    const DumpBlock& block = *Enclosing().back();
    pending_.push_back({.kind = block.Kind(), .name = block.identifier});
  }

  void Leaving() override {
    CloseScope();
  }

  // `remaining` is how many levels of instances from this scope down are
  // recorded whole. A level is an instance (LRM 21.7.1.2): a block a scope
  // holds is part of the instance holding it, and counts as no level of its
  // own.
  void WalkScope(Scope& scope, std::int64_t remaining) {
    if (const auto named = levels_.find(&scope); named != levels_.end()) {
      remaining = std::max(remaining, named->second);
    }
    const support::DumpScopeKind kind = scope.Info().metadata.DumpedAs();
    const bool addressable = scope.IsAddressable();
    if (addressable) {
      pending_.push_back({.kind = kind, .name = scope.DisplaySegment()});
    }
    whole_ = remaining > 0;
    if (whole_ || !variables_.empty()) {
      scope.Describe();
    }
    scope.ForEachChild(
        [&](Scope& child) { WalkScope(child, LevelsBelow(child, remaining)); });
    if (addressable) {
      CloseScope();
    }
  }

  // The levels left once the walk enters `child` from a scope with
  // `remaining`.
  static auto LevelsBelow(const Scope& child, std::int64_t remaining)
      -> std::int64_t {
    switch (child.Info().metadata.DumpedAs()) {
      case support::DumpScopeKind::kModule:
        return remaining == UnboundedLevels()
                   ? remaining
                   : std::max<std::int64_t>(remaining - 1, 0);
      case support::DumpScopeKind::kTask:
      case support::DumpScopeKind::kFunction:
      case support::DumpScopeKind::kBegin:
      case support::DumpScopeKind::kFork:
        return remaining;
    }
    throw InternalError("LevelsBelow: unknown scope kind");
  }

  [[nodiscard]] auto Records(const void* member) const -> bool {
    return whole_ || std::ranges::find(variables_, member) != variables_.end();
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

  struct PendingScope {
    support::DumpScopeKind kind;
    std::string name;
  };

  DumpOutput* output_;
  ValueDump* dump_;
  std::vector<std::unique_ptr<DumpWatch>>* watches_;
  std::unordered_map<const Scope*, std::int64_t> levels_;
  std::vector<const void*> variables_;
  std::unordered_map<const void*, DumpCode> codes_;
  std::vector<PendingScope> pending_;
  std::size_t opened_ = 0;
  bool whole_ = false;
};

// Describes the ports of the instances an extended dump names (LRM 21.7.3.1):
// each instance a module scope named by its hierarchical name, and in it each
// port, in the order the declaration gives them, with a code of its own -- a
// port's values are read by its direction, so two ports connected to one net
// are two things. What a named block, task or function inside declares is no
// port, and neither is an event, which the format has no value for.
class PortDescriber final : public Describer {
 public:
  PortDescriber(
      DumpOutput& output, ValueDump& dump,
      std::vector<std::unique_ptr<DumpWatch>>& watches)
      : output_(&output), dump_(&dump), watches_(&watches) {
  }

  void Walk(const PortSelection& selection) {
    for (Scope* instance : selection.instances) {
      ports_.clear();
      instance->Describe();
      std::ranges::sort(ports_, {}, &Port::position);
      if (ports_.empty()) {
        continue;
      }
      output_->Send(
          ScopeCommand{
              .kind = support::DumpScopeKind::kModule,
              .name = std::string(instance->HierarchicalPath().View())});
      for (Port& port : ports_) {
        const DumpCode code{static_cast<std::uint32_t>(watches_->size())};
        output_->Send(VarCommand{.code = code, .entry = port.entry});
        watches_->push_back(
            std::make_unique<DumpWatch>(*dump_, code, std::move(port.probe)));
      }
      output_->Send(UpscopeCommand{});
    }
  }

  void Var(const DumpEntry& entry, std::unique_ptr<DumpProbe> probe) override {
    const std::optional<DumpEntry::Connection> port = entry.Port();
    if (!port.has_value() || !HasPortValue(entry)) {
      return;
    }
    ports_.push_back(
        {.position = port->position,
         .entry = entry,
         .probe = std::move(probe)});
  }

 private:
  static auto HasPortValue(const DumpEntry& entry) -> bool {
    switch (support::DumpValueFormOf(entry.Kind())) {
      case support::DumpValueForm::kTriggered:
        return false;
      case support::DumpValueForm::kBits:
      case support::DumpValueForm::kReal:
        return true;
    }
    throw InternalError("PortDescriber: unknown value");
  }

  struct Port {
    std::uint32_t position;
    DumpEntry entry;
    std::unique_ptr<DumpProbe> probe;
  };

  DumpOutput* output_;
  ValueDump* dump_;
  std::vector<std::unique_ptr<DumpWatch>>* watches_;
  std::vector<Port> ports_;
};

}  // namespace

ValueDump::ValueDump(
    std::unique_ptr<DumpWriter> writer, std::int8_t precision_power)
    : output_(std::make_unique<DumpOutput>(std::move(writer))) {
  output_->Send(TimescaleCommand{.precision_power = precision_power});
}

ValueDump::~ValueDump() = default;

void ValueDump::Start(
    const DumpSelection& selection, std::span<Scope* const> roots, SimTime now,
    ValueDumps& dumps) {
  SelectionDescriber describer(*output_, *this, watches_);
  {
    const ValueDumps::InstalledDescriber installed(dumps, describer);
    describer.Walk(selection, roots);
  }
  Begin(now);
}

void ValueDump::Start(
    const PortSelection& selection, SimTime now, ValueDumps& dumps) {
  PortDescriber describer(*output_, *this, watches_);
  {
    const ValueDumps::InstalledDescriber installed(dumps, describer);
    describer.Walk(selection);
  }
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
  runtime.Submit(now, Region::kPostponed, [this, now] { EndStep(now); });
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
  DumpCapture& capture = output_->NextCapture();
  for (DumpWatch* watch : changed_) {
    watch->TakeChanged();
    watch->Read(capture);
  }
  changed_.clear();
  output_->SendCapture();
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

// A time step is named once, however many of its records follow: a change
// and a checkpoint of one step are both under the one `#time` (LRM 21.7.2.1).
void ValueDump::At(SimTime now) {
  if (time_sent_ == now) {
    return;
  }
  time_sent_ = now;
  output_->Send(TimeCommand{.time = now});
}

void ValueDump::Limit(std::uint64_t bytes) {
  output_->Send(LimitCommand{.bytes = bytes});
}

void ValueDump::Flush() {
  output_->Flush();
}

void ValueDump::Close(SimTime now) {
  At(now);
  output_->Close();
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
  DumpCapture& capture = output_->NextCapture();
  for (const std::unique_ptr<DumpWatch>& watch : watches_) {
    if (unknown) {
      capture.Unknown(watch->Code());
    } else {
      watch->Read(capture);
    }
  }
  output_->SendCapture();
  output_->Send(EndCheckpointCommand{});
}

}  // namespace lyra::runtime
