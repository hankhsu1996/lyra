#include "lyra/runtime/value_dumps.hpp"

#include <algorithm>
#include <array>
#include <charconv>
#include <cstdint>
#include <memory>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/base/diag_message.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/time.hpp"
#include "lyra/runtime/diagnostic.hpp"
#include "lyra/runtime/dump_writer.hpp"
#include "lyra/runtime/region.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/scope.hpp"
#include "lyra/runtime/value_dump.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

namespace {

constexpr std::string_view kFileOption = "lyra+dump=";
constexpr std::string_view kScopeOption = "lyra+dump+scope=";
constexpr std::string_view kWindowOption = "lyra+dump+window=";
constexpr std::string_view kForceOption = "lyra+dump+force";
constexpr std::string_view kNoneOption = "lyra+dump+none";

// LRM 21.7.1.1: the file a dump is written to where nothing names one.
constexpr std::string_view kDefaultFile = "dump.vcd";

// A count of a unit (`2200ns`) as a count of the design's precision, none
// where the text is not one or is finer than the precision can state.
auto TicksOf(std::string_view text, std::int8_t precision_power)
    -> std::optional<SimTime> {
  std::uint64_t count = 0;
  const auto [rest, error] =
      std::from_chars(text.data(), text.data() + text.size(), count);
  if (error != std::errc{}) {
    return std::nullopt;
  }
  const std::string_view unit(rest, text.data() + text.size());
  int unit_power = 0;
  if (unit == "s") {
    unit_power = 0;
  } else if (unit == "ms") {
    unit_power = -3;
  } else if (unit == "us") {
    unit_power = -6;
  } else if (unit == "ns") {
    unit_power = -9;
  } else if (unit == "ps") {
    unit_power = -12;
  } else if (unit == "fs") {
    unit_power = -15;
  } else {
    return std::nullopt;
  }
  for (int power = unit_power; power > precision_power; --power) {
    count *= 10;
  }
  for (int power = unit_power; power < precision_power; ++power) {
    if (count % 10 != 0) {
      return std::nullopt;
    }
    count /= 10;
  }
  return count;
}

// Finds, while one instance describes itself, the member that is the thing it
// declares under `name`: its identifier, after the names of the named blocks,
// tasks and functions it lies in, joined by `.` as a hierarchical name joins
// them.
class NameFinder final : public Describer {
 public:
  explicit NameFinder(std::string_view name) : name_(name) {
  }

  void Var(
      const abi::DumpEntry& entry, std::unique_ptr<DumpProbe> probe) override {
    std::string path;
    for (const abi::DumpBlock* block : Enclosing()) {
      path += block->identifier;
      path.push_back('.');
    }
    path += entry.identifier;
    if (path == name_) {
      found_ = probe->Member();
    }
  }

  [[nodiscard]] auto Found() const -> const void* {
    return found_;
  }

 private:
  std::string_view name_;
  const void* found_ = nullptr;
};

// The child of `scope` whose name as a hierarchical path writes it is
// `segment`, none where it has none.
auto ChildNamed(Scope& scope, std::string_view segment) -> Scope* {
  Scope* found = nullptr;
  scope.ForEachChild([&](Scope& child) {
    if (found == nullptr && child.IsAddressable() &&
        child.DisplaySegment() == segment) {
      found = &child;
    }
  });
  return found;
}

}  // namespace

ValueDumps::ValueDumps(std::span<const std::string> plusargs) {
  for (const std::string& argument : plusargs) {
    const std::string_view text(argument);
    if (text.starts_with(kFileOption)) {
      option_file_ = std::string(text.substr(kFileOption.size()));
    } else if (text.starts_with(kScopeOption)) {
      const std::string_view named = text.substr(kScopeOption.size());
      const std::size_t colon = named.rfind(':');
      std::int64_t levels = 0;
      std::string_view path = named;
      if (colon != std::string_view::npos) {
        const std::string_view count = named.substr(colon + 1);
        const auto [end, error] =
            std::from_chars(count.data(), count.data() + count.size(), levels);
        if (error == std::errc{} && end == count.data() + count.size()) {
          path = named.substr(0, colon);
        } else {
          levels = 0;
        }
      }
      option_scopes_.push_back({.path = std::string(path), .levels = levels});
    } else if (text.starts_with(kWindowOption)) {
      option_windows_.emplace_back(text.substr(kWindowOption.size()));
    } else if (text == kForceOption) {
      force_ = true;
    } else if (text == kNoneOption) {
      none_ = true;
    }
  }
}

ValueDumps::~ValueDumps() = default;

void ValueDumps::Begin(
    RuntimeEffects& runtime, Scope& root, std::int8_t precision_power) {
  runtime_ = &runtime;
  root_ = &root;
  precision_power_ = precision_power;
  for (const std::string& window : option_windows_) {
    const std::size_t colon = window.find(':');
    const std::optional<SimTime> from =
        colon == std::string::npos
            ? std::nullopt
            : TicksOf(
                  std::string_view(window).substr(0, colon), precision_power);
    const std::optional<SimTime> to =
        colon == std::string::npos
            ? std::nullopt
            : TicksOf(
                  std::string_view(window).substr(colon + 1), precision_power);
    if (!from.has_value() || !to.has_value() || *to < *from) {
      runtime.Diagnostic().Report(
          Severity::kWarning,
          base::DiagMessage(
              "the dump window '{}' is not two times such as '2200ns:2400ns'; "
              "it is ignored",
              window));
      continue;
    }
    windows_.push_back({.from = *from, .to = *to});
  }
  // Windows that overlap or meet are one stretch the dump is on, so each one
  // ends before the next one starts.
  std::ranges::sort(windows_, {}, &Window::from);
  std::vector<Window> joined;
  for (const Window& window : windows_) {
    if (!joined.empty() && window.from <= joined.back().to) {
      joined.back().to = std::max(joined.back().to, window.to);
    } else {
      joined.push_back(window);
    }
  }
  windows_ = std::move(joined);
  const bool asked = option_file_.has_value() || !option_scopes_.empty() ||
                     !windows_.empty() || force_;
  if (none_ || !asked) {
    return;
  }
  // The options' dump starts with the run, or where its first window opens,
  // at the end of that time step, unless the design's own `$dumpvars` has run
  // by then and so starts the dump itself.
  const SimTime start = windows_.empty() ? 0 : windows_.front().from;
  runtime.Submit(start, Region::kPostponed, [this, start] {
    if (!design_selected_at_.has_value()) {
      Start(start, OptionSelection());
    }
  });
}

void ValueDumps::End() {
  const SimTime now = runtime_->Now();
  if (dump_ != nullptr) {
    dump_->Close(now);
  }
  for (PortDump& ports : port_dumps_) {
    if (ports.dump != nullptr) {
      ports.dump->Close(now);
    }
  }
}

void ValueDumps::DumpFile(const value::String& name) {
  if (!DesignActs()) {
    return;
  }
  if (dump_ != nullptr) {
    runtime_->Diagnostic().Report(
        Severity::kWarning,
        base::DiagMessage(
            "a '$dumpfile' call after the dump started is ignored"));
    return;
  }
  design_file_ = std::string(name.View());
}

void ValueDumps::DumpVars() {
  if (!DesignActs()) {
    return;
  }
  Select(runtime_->Now());
}

// Levels alone count from the design's tops.
void ValueDumps::DumpVarsLevels(std::int64_t levels) {
  if (!DesignActs()) {
    return;
  }
  root_->ForEachChild([&](Scope& top) {
    design_selection_.instances.push_back({.scope = &top, .levels = levels});
  });
  Select(runtime_->Now());
}

void ValueDumps::DumpVarsOf(std::int64_t levels, Scope& instance) {
  if (!DesignActs()) {
    return;
  }
  design_selection_.instances.push_back({.scope = &instance, .levels = levels});
  Select(runtime_->Now());
}

void ValueDumps::DumpVarsOfVariable(const void* member) {
  if (!DesignActs()) {
    return;
  }
  design_selection_.variables.push_back(member);
  Select(runtime_->Now());
}

template <typename Act>
void ValueDumps::OnceStarted(Act act) {
  if (!DesignActs()) {
    return;
  }
  if (dump_ != nullptr) {
    act(*dump_);
    return;
  }
  const SimTime now = runtime_->Now();
  if (design_selected_at_ == now) {
    runtime_->Submit(now, Region::kPostponed, [this, act] { act(*dump_); });
  }
}

void ValueDumps::DumpOff() {
  OnceStarted([this](ValueDump& dump) { dump.Off(runtime_->Now()); });
}

void ValueDumps::DumpOn() {
  OnceStarted([this](ValueDump& dump) { dump.On(runtime_->Now()); });
}

void ValueDumps::DumpAll() {
  OnceStarted([this](ValueDump& dump) { dump.All(runtime_->Now()); });
}

void ValueDumps::DumpLimit(std::int64_t bytes) {
  if (!DesignActs() || bytes < 0) {
    return;
  }
  limit_ = static_cast<std::uint64_t>(bytes);
  if (dump_ != nullptr) {
    dump_->Limit(*limit_);
  }
}

void ValueDumps::DumpFlush() {
  OnceStarted([](ValueDump& dump) { dump.Flush(); });
}

// LRM 21.7.3.1: every `$dumpports` executes at one simulation time, each file
// starts at the end of it, and a file is named by one call only.
void ValueDumps::DumpPortsOf(Scope& instance, const value::String& name) {
  if (!DesignActs()) {
    return;
  }
  const SimTime now = runtime_->Now();
  if (ports_selected_at_.has_value() && *ports_selected_at_ != now) {
    runtime_->Diagnostic().Report(
        Severity::kWarning,
        base::DiagMessage(
            "a '$dumpports' call at a later time than the first one is "
            "ignored"));
    return;
  }
  ports_selected_at_ = now;
  const std::string file(name.View());
  const auto named = std::ranges::find(port_dumps_, file, &PortDump::file);
  if (named != port_dumps_.end()) {
    named->selection.instances.push_back(&instance);
    return;
  }
  const std::size_t index = port_dumps_.size();
  port_dumps_.push_back(
      {.file = file,
       .selection = {.instances = {&instance}},
       .limit = std::nullopt,
       .dump = nullptr});
  runtime_->Submit(now, Region::kPostponed, [this, index, now] {
    PortDump& ports = port_dumps_[index];
    ports.dump = std::make_unique<ValueDump>(
        OpenExtendedVcdWriter(ports.file), precision_power_);
    if (ports.limit.has_value()) {
      ports.dump->Limit(*ports.limit);
    }
    ports.dump->Start(ports.selection, now, *this);
  });
}

template <typename Act>
void ValueDumps::OnEachPortDump(const value::String& name, Act act) {
  if (!DesignActs()) {
    return;
  }
  const SimTime now = runtime_->Now();
  for (std::size_t index = 0; index < port_dumps_.size(); ++index) {
    PortDump& ports = port_dumps_[index];
    if (!name.View().empty() && ports.file != name.View()) {
      continue;
    }
    if (ports.dump != nullptr) {
      act(*ports.dump);
    } else {
      runtime_->Submit(now, Region::kPostponed, [this, index, act] {
        act(*port_dumps_[index].dump);
      });
    }
  }
}

void ValueDumps::DumpPortsOff(const value::String& name) {
  OnEachPortDump(name, [this](ValueDump& dump) { dump.Off(runtime_->Now()); });
}

void ValueDumps::DumpPortsOn(const value::String& name) {
  OnEachPortDump(name, [this](ValueDump& dump) { dump.On(runtime_->Now()); });
}

void ValueDumps::DumpPortsAll(const value::String& name) {
  OnEachPortDump(name, [this](ValueDump& dump) { dump.All(runtime_->Now()); });
}

void ValueDumps::DumpPortsLimit(std::int64_t bytes, const value::String& name) {
  if (!DesignActs() || bytes < 0) {
    return;
  }
  const auto limit = static_cast<std::uint64_t>(bytes);
  for (PortDump& ports : port_dumps_) {
    if (!name.View().empty() && ports.file != name.View()) {
      continue;
    }
    if (ports.dump != nullptr) {
      ports.dump->Limit(limit);
    } else {
      ports.limit = limit;
    }
  }
}

void ValueDumps::DumpPortsFlush(const value::String& name) {
  OnEachPortDump(name, [](ValueDump& dump) { dump.Flush(); });
}

auto ValueDumps::Describing() -> Describer& {
  if (describing_ == nullptr) {
    throw InternalError(
        "an instance described itself while no value dump was starting");
  }
  return *describing_;
}

// LRM 21.7.1.2: every `$dumpvars` executes at one simulation time, and the
// dump starts at the end of it.
void ValueDumps::Select(SimTime now) {
  if (dump_ != nullptr ||
      (design_selected_at_.has_value() && *design_selected_at_ != now)) {
    runtime_->Diagnostic().Report(
        Severity::kWarning,
        base::DiagMessage(
            "a '$dumpvars' call after the dump has started is ignored"));
    return;
  }
  if (design_selected_at_.has_value()) {
    return;
  }
  design_selected_at_ = now;
  runtime_->Submit(
      now, Region::kPostponed, [this, now] { Start(now, design_selection_); });
}

void ValueDumps::Start(SimTime now, const DumpSelection& selection) {
  const std::string file =
      force_ ? option_file_.value_or(std::string(kDefaultFile))
             : design_file_.value_or(
                   option_file_.value_or(std::string(kDefaultFile)));
  dump_ = std::make_unique<ValueDump>(OpenDumpWriter(file), precision_power_);
  if (limit_.has_value()) {
    dump_->Limit(*limit_);
  }
  const std::array<Scope*, 1> roots{root_};
  dump_->Start(selection, roots, now, *this);
  OpenWindowsFrom(0);
}

// Each window the options name has the dump on from its start up to its end
// and off between, as `$dumpon` and `$dumpoff` would at those two times; what
// changes at the end is not recorded, the end being where the dump is off.
// A dump starts no later than the first window opens, the design's own
// `$dumpvars` taking it over only by then, and each window ends before the
// next opens, so no window is over by the time it is reached.
void ValueDumps::OpenWindowsFrom(std::size_t index) {
  if (index >= windows_.size()) {
    return;
  }
  const Window window = windows_[index];
  const SimTime now = runtime_->Now();
  if (window.from > now) {
    dump_->Off(now);
    runtime_->Submit(
        window.from, Region::kPostponed,
        [this, from = window.from] { dump_->On(from); });
  }
  runtime_->Submit(
      window.to, Region::kPostponed, [this, index, to = window.to] {
        dump_->Off(to);
        OpenWindowsFrom(index + 1);
      });
}

// Each path names an instance, walked down from the tops by the names its
// scopes are written with, or a variable of the instance it stops at.
auto ValueDumps::OptionSelection() -> DumpSelection {
  DumpSelection selection;
  for (const NamedScope& named : option_scopes_) {
    Scope* instance = root_;
    std::string_view rest = named.path;
    while (!rest.empty()) {
      const std::string_view segment = rest.substr(0, rest.find('.'));
      Scope* child = ChildNamed(*instance, segment);
      if (child == nullptr) {
        break;
      }
      instance = child;
      rest.remove_prefix(std::min(rest.size(), segment.size() + 1));
    }
    if (rest.empty()) {
      selection.instances.push_back(
          {.scope = instance, .levels = named.levels});
      continue;
    }
    if (const void* member = FindVariable(*instance, rest); member != nullptr) {
      selection.variables.push_back(member);
      continue;
    }
    runtime_->Diagnostic().Report(
        Severity::kWarning,
        base::DiagMessage(
            "the dump scope '{}' names no instance or variable; it is ignored",
            named.path));
  }
  return selection;
}

auto ValueDumps::FindVariable(Scope& instance, std::string_view name) -> const
    void* {
  NameFinder finder(name);
  const InstalledDescriber installed(*this, finder);
  instance.Describe();
  return finder.Found();
}

void DumpFile(RuntimeEffects& runtime, const value::String& name) {
  runtime.Dumps().DumpFile(name);
}

void DumpVars(RuntimeEffects& runtime) {
  runtime.Dumps().DumpVars();
}

void DumpVarsLevels(RuntimeEffects& runtime, std::int64_t levels) {
  runtime.Dumps().DumpVarsLevels(levels);
}

void DumpVarsOf(RuntimeEffects& runtime, std::int64_t levels, Scope* instance) {
  runtime.Dumps().DumpVarsOf(levels, *instance);
}

void DumpVarsOfVariable(RuntimeEffects& runtime, const void* member) {
  runtime.Dumps().DumpVarsOfVariable(member);
}

void DumpOff(RuntimeEffects& runtime) {
  runtime.Dumps().DumpOff();
}

void DumpOn(RuntimeEffects& runtime) {
  runtime.Dumps().DumpOn();
}

void DumpAll(RuntimeEffects& runtime) {
  runtime.Dumps().DumpAll();
}

void DumpLimit(RuntimeEffects& runtime, std::int64_t bytes) {
  runtime.Dumps().DumpLimit(bytes);
}

void DumpFlush(RuntimeEffects& runtime) {
  runtime.Dumps().DumpFlush();
}

void DumpPortsOf(
    RuntimeEffects& runtime, Scope* instance, const value::String& name) {
  runtime.Dumps().DumpPortsOf(*instance, name);
}

void DumpPortsOff(RuntimeEffects& runtime, const value::String& name) {
  runtime.Dumps().DumpPortsOff(name);
}

void DumpPortsOn(RuntimeEffects& runtime, const value::String& name) {
  runtime.Dumps().DumpPortsOn(name);
}

void DumpPortsAll(RuntimeEffects& runtime, const value::String& name) {
  runtime.Dumps().DumpPortsAll(name);
}

void DumpPortsLimit(
    RuntimeEffects& runtime, std::int64_t bytes, const value::String& name) {
  runtime.Dumps().DumpPortsLimit(bytes, name);
}

void DumpPortsFlush(RuntimeEffects& runtime, const value::String& name) {
  runtime.Dumps().DumpPortsFlush(name);
}

}  // namespace lyra::runtime
