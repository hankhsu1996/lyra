#include "lyra/runtime/simulation_entry.hpp"

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdio>
#include <cstdlib>
#include <exception>
#include <memory>
#include <sanitizer/common_interface_defs.h>
#include <span>
#include <string>
#include <string_view>
#include <unistd.h>
#include <utility>
#include <vector>

#include "lyra/base/diag_message.hpp"
#include "lyra/base/fault_watch.hpp"
#include "lyra/base/memory_watch.hpp"
#include "lyra/runtime/ambient_run_context.hpp"
#include "lyra/runtime/design.hpp"
#include "lyra/runtime/file_table.hpp"
#include "lyra/runtime/finished_output.hpp"
#include "lyra/runtime/hierarchy_segment.hpp"
#include "lyra/runtime/interrupt_report.hpp"
#include "lyra/runtime/plusargs.hpp"
#include "lyra/runtime/runtime.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/third_party_licenses.hpp"
#include "lyra/value/queue_bound.hpp"

namespace lyra::runtime {

namespace {

// For as long as the run lasts, a bounded queue that drops what the design
// wrote past its bound (LRM 7.10.5) is said as one of the run's own reports,
// placed and timed like any other.
class BoundOverflowReport {
 public:
  BoundOverflowReport() : replaced_(value::ListenForBoundOverflow(&Say)) {
  }
  ~BoundOverflowReport() {
    value::ListenForBoundOverflow(replaced_);
  }
  BoundOverflowReport(const BoundOverflowReport&) = delete;
  auto operator=(const BoundOverflowReport&) -> BoundOverflowReport& = delete;
  BoundOverflowReport(BoundOverflowReport&&) = delete;
  auto operator=(BoundOverflowReport&&) -> BoundOverflowReport& = delete;

 private:
  static void Say() {
    AmbientRunContext::Current().Effects().Diagnostic().Report(
        Severity::kWarning,
        base::DiagMessage(
            "a bounded queue grew past its declared bound, and the elements "
            "past it were dropped"));
  }

  value::BoundOverflowListener replaced_;
};

// Drives a bound Runtime to completion. The run accounts for a design's own
// run-time error and for a failure of the tool discovered while it is under
// way, so what is reported here is what happened outside that: an invariant
// the engine established for itself.
auto RunSimulation(Runtime& runtime) -> int {
  try {
    // Foreign code reaches the run with plain C arguments and nothing to find
    // it by, so what it resolves a scope against is anchored for as long as the
    // run lasts -- which starts at the first thing a run does, initializing
    // state, since a context import can already be reached from there (LRM
    // 10.5, 35.5.3).
    const AmbientRunContext run_context{runtime.DesignRoot(), runtime};
    const InterruptReport interrupt_report{runtime};
    const BoundOverflowReport bound_overflow_report;
    return runtime.Run();
  } catch (const std::exception&) {
    ReportRaisedError(runtime, std::current_exception());
    return EXIT_FAILURE;
  }
}

constexpr std::string_view kOutOfStack =
    "error: the simulation ran out of stack\n"
    "note: what the design was running needed more stack than the program "
    "was started with; start it with a larger one\n";

constexpr std::string_view kLicensesOption = "lyra+licenses";

constexpr std::string_view kOutOfMemory =
    "error: the simulation ran out of memory\n";

}  // namespace

// A program built to check its memory links the address sanitizer, which ends
// the process itself when it has reported, calling this first where one is
// registered. A program built without it links none of this, so the
// declaration is weak and nothing is registered.
#pragma weak __sanitizer_set_death_callback

auto RunDesignRoot(
    int argc, char** argv, std::string_view root_name, const RootFactory& make)
    -> int {
  // Every ending the process can still act on -- a fault, a report of the
  // checker, and, for as long as the run lasts, a signal from outside --
  // delivers what the run had finished before it ends: what the design wrote,
  // and every value dump the run records. A refused request for memory
  // delivers what the design wrote alone: finishing a dump is encoding, which
  // needs the memory the run has just been refused.
  base::InstallOutOfMemoryHandler(kOutOfMemory, DeliverFinishedFiles);
  // The simulation runs on the stack of the thread the program starts on. A
  // foreign task runs on a stack of its own, which is not registered.
  static std::array<std::byte, base::kAltStackSize> alt_stack;
  base::InstallFaultHandler(kOutOfStack, DeliverFinishedOutput);
  base::RegisterThreadStack(alt_stack);
  if (__sanitizer_set_death_callback != nullptr) {
    __sanitizer_set_death_callback(DeliverFinishedOutput);
  }

  const std::span<char*> args{argv, static_cast<std::size_t>(argc)};
  std::vector<std::string> words{args.begin(), args.end()};
  // The program carries third-party code inside the runtime it links, whose
  // notices it prints when asked, in place of a run.
  if (std::ranges::contains(PlusargsFrom(words), kLicensesOption)) {
    const std::string_view notices = ThirdPartyLicenses();
    std::fwrite(notices.data(), 1, notices.size(), stdout);
    return EXIT_SUCCESS;
  }
  Runtime runtime{RuntimeOptions{.command_line = std::move(words)}};

  // Building the design and resolving its references precede the simulation
  // (LRM 3.12). An error here has no activation to leave and no final procedure
  // to reach, so it is reported and the run never starts.
  try {
    runtime.BindDesign(
        std::make_unique<Design>(
            make(nullptr, HierarchySegment{std::string{root_name}, {}})));
  } catch (const std::exception&) {
    ReportRaisedError(runtime, std::current_exception());
    return EXIT_FAILURE;
  }

  return RunSimulation(runtime);
}
}  // namespace lyra::runtime
