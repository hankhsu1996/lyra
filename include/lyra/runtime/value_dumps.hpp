#pragma once

#include <cstdint>
#include <memory>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/base/time.hpp"
#include "lyra/runtime/value_dump.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

class Describer;
class RuntimeEffects;
class Scope;

// The value dump of a run (LRM 21.7): the one file the design's `$dump*` tasks
// act on, and what the run's own options ask of it. The options are plusargs
// the program reads, so a program built once is dumped any way without being
// built again:
//
//   +lyra+dump=<file>                   the file, its format by its extension
//   +lyra+dump+scope=<path>[:<levels>]  an instance and levels below it, or a
//                                       variable; repeatable, none being the
//                                       whole design
//   +lyra+dump+window=<from>:<to>       a stretch of time, each end a number
//                                       and a unit (`2200ns`); repeatable
//   +lyra+dump+force                    the options decide alone, and the
//                                       design's own `$dump*` tasks do nothing
//   +lyra+dump+none                     nothing is dumped at all
//
// What the design's tasks state wins over the options where both state it,
// as the options are the run's defaults; `force` and `none` reverse that.
//
// Beside it are the extended dumps of ports the design's `$dumpports` tasks
// open, one a file (LRM 21.7.3); `force` and `none` turn those off as well.
class ValueDumps {
 public:
  explicit ValueDumps(std::span<const std::string> plusargs);
  ValueDumps(const ValueDumps&) = delete;
  auto operator=(const ValueDumps&) -> ValueDumps& = delete;
  ValueDumps(ValueDumps&&) = delete;
  auto operator=(ValueDumps&&) -> ValueDumps& = delete;
  ~ValueDumps();

  // The run has bound its design and fixed its precision: a dump the options
  // ask for, which the design does not take over, starts at time zero or where
  // its window opens.
  void Begin(RuntimeEffects& runtime, Scope& root, std::int8_t precision_power);
  // Ends every file, reporting a failure to write one.
  void End();

  // LRM 21.7.1.1.
  void DumpFile(const value::String& name);
  // LRM 21.7.1.2: the whole design, `levels` of it from its tops, an instance
  // and `levels` below it, or one variable, by the member it is of the
  // instance declaring it.
  void DumpVars();
  void DumpVarsLevels(std::int64_t levels);
  void DumpVarsOf(std::int64_t levels, Scope& instance);
  void DumpVarsOfVariable(const void* member);
  // LRM 21.7.1.3 - 21.7.1.6.
  void DumpOff();
  void DumpOn();
  void DumpAll();
  void DumpLimit(std::int64_t bytes);
  void DumpFlush();

  // LRM 21.7.3.1: the ports of `instance` go to the file `name`, which every
  // instance a `$dumpports` call names shares.
  void DumpPortsOf(Scope& instance, const value::String& name);
  // LRM 21.7.3.2 - 21.7.3.5, each acting on the file `name`, or on every one
  // where the name is empty: no file a `$dumpports` opens has an empty name.
  void DumpPortsOff(const value::String& name);
  void DumpPortsOn(const value::String& name);
  void DumpPortsAll(const value::String& name);
  void DumpPortsLimit(std::int64_t bytes, const value::String& name);
  void DumpPortsFlush(const value::String& name);

  // The describer an instance describes itself to while a dump starts.
  [[nodiscard]] auto Describing() -> Describer&;

  // For as long as it lives, the describer every instance asked to describe
  // itself describes itself to. An instance reaches it through the run
  // rather than being handed it, because the library's own scope class
  // defines the method too and describes nothing, so it would have no use
  // for it.
  class InstalledDescriber {
   public:
    InstalledDescriber(ValueDumps& dumps, Describer& describer)
        : dumps_(&dumps) {
      dumps_->describing_ = &describer;
    }
    InstalledDescriber(const InstalledDescriber&) = delete;
    auto operator=(const InstalledDescriber&) -> InstalledDescriber& = delete;
    InstalledDescriber(InstalledDescriber&&) = delete;
    auto operator=(InstalledDescriber&&) -> InstalledDescriber& = delete;
    ~InstalledDescriber() {
      dumps_->describing_ = nullptr;
    }

   private:
    ValueDumps* dumps_;
  };

 private:
  struct NamedScope {
    std::string path;
    std::int64_t levels;
  };
  struct Window {
    SimTime from;
    SimTime to;
  };

  // Whether the design's own `$dump*` tasks act, which `force` and `none`
  // turn off.
  [[nodiscard]] auto DesignActs() const -> bool {
    return !force_ && !none_;
  }
  // The `$dumpvars` of the design, made at the time its first call ran: the
  // dump starts at the end of that time step (LRM 21.7.1.2).
  void Select(SimTime now);
  // Has `act` act on the dump the design's tasks control: at once where it
  // has started, and where the `$dumpvars` starting it ran in this time step,
  // once it starts at the end of the step.
  template <typename Act>
  void OnceStarted(Act act);
  void Start(SimTime now, const DumpSelection& selection);
  [[nodiscard]] auto OptionSelection() -> DumpSelection;
  // The member of `instance` that is the variable it declares under `name`,
  // none where it declares none.
  [[nodiscard]] auto FindVariable(Scope& instance, std::string_view name)
      -> const void*;
  void OpenWindowsFrom(std::size_t index);
  // Has `act` act on each dump of ports `name` names: at once on one that has
  // started, and on one whose `$dumpports` ran in this time step once it
  // starts at the end of the step.
  template <typename Act>
  void OnEachPortDump(const value::String& name, Act act);

  // One file of ports, what it records, and the bound it is given before it
  // starts.
  struct PortDump {
    std::string file;
    PortSelection selection;
    std::optional<std::uint64_t> limit;
    std::unique_ptr<ValueDump> dump;
  };

  std::optional<std::string> option_file_;
  std::vector<NamedScope> option_scopes_;
  std::vector<std::string> option_windows_;
  bool force_ = false;
  bool none_ = false;

  RuntimeEffects* runtime_ = nullptr;
  Scope* root_ = nullptr;
  std::int8_t precision_power_ = kDefaultTimePrecisionPower;
  std::optional<std::string> design_file_;
  DumpSelection design_selection_;
  std::optional<SimTime> design_selected_at_;
  std::optional<std::uint64_t> limit_;
  std::vector<Window> windows_;
  std::unique_ptr<ValueDump> dump_;
  std::vector<PortDump> port_dumps_;
  std::optional<SimTime> ports_selected_at_;
  Describer* describing_ = nullptr;
};

// The entries generated code calls, each acting on the run's value dump (LRM
// 21.7.1). A `$dumpvars` naming an instance hands that instance, and one
// naming a variable the member it is of the instance declaring it.
void DumpFile(RuntimeEffects& runtime, const value::String& name);
void DumpVars(RuntimeEffects& runtime);
void DumpVarsLevels(RuntimeEffects& runtime, std::int64_t levels);
void DumpVarsOf(RuntimeEffects& runtime, std::int64_t levels, Scope* instance);
void DumpVarsOfVariable(RuntimeEffects& runtime, const void* member);
void DumpOff(RuntimeEffects& runtime);
void DumpOn(RuntimeEffects& runtime);
void DumpAll(RuntimeEffects& runtime);
void DumpLimit(RuntimeEffects& runtime, std::int64_t bytes);
void DumpFlush(RuntimeEffects& runtime);
void DumpPortsOf(
    RuntimeEffects& runtime, Scope* instance, const value::String& name);
void DumpPortsOff(RuntimeEffects& runtime, const value::String& name);
void DumpPortsOn(RuntimeEffects& runtime, const value::String& name);
void DumpPortsAll(RuntimeEffects& runtime, const value::String& name);
void DumpPortsLimit(
    RuntimeEffects& runtime, std::int64_t bytes, const value::String& name);
void DumpPortsFlush(RuntimeEffects& runtime, const value::String& name);

}  // namespace lyra::runtime
