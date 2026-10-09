#pragma once

#include <span>

#include "lyra/runtime/observation.hpp"
#include "lyra/runtime/read_report.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/runtime/wait.hpp"

namespace lyra::runtime {

// The waits woken by what happens on storage: an event control (LRM 9.4.2,
// 15.5.2), an implicit sensitivity (LRM 9.2.2.2.1, 9.4.2.2), and a `wait`
// statement (LRM 9.4.3). They part company in two places only: what they watch
// is fixed where the wait is built or known only once the process has
// evaluated, and what starting a stopped process again asks of them (LRM 9.7).

// A wait on one trigger per leaf, the storage it watches fixed for as long as
// the body holding it runs: what a change there is measured from is taken where
// the wait begins, and only an occurrence while the body is parked on it is
// one. An empty span is a wait that is never woken. The leaves are read here
// and not kept, laid out in one array or each where its builder left it.
[[nodiscard]] auto WaitOn(std::span<const Trigger> triggers) -> Wait;
[[nodiscard]] auto WaitOn(std::span<const Trigger* const> triggers) -> Wait;

// The same on the implicit list `report` settled.
[[nodiscard]] auto WaitOnImplicitList(const ReadReport* report) -> Wait;

// An event control its process decides: the frame resumes on every candidacy
// the reports' places see, and evaluates its observations again to learn
// whether it was an event and what it reaches now. The observations are held
// for a restart (LRM 9.7), which leaves them to be armed by that evaluation
// rather than wherever the restart is asked for. Each report is left empty for
// the evaluation after.
[[nodiscard]] auto WaitRecollecting(
    std::span<ReadReport* const> reports,
    std::span<const Observation* const> observations) -> Wait;

// A `wait (cond)` waiting on what the last test of its condition reached (LRM
// 9.4.3), each report left empty for the test after.
[[nodiscard]] auto WaitUntil(std::span<ReadReport* const> reports) -> Wait;

}  // namespace lyra::runtime
