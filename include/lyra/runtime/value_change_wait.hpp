#pragma once

#include <cstddef>
#include <span>

#include "lyra/runtime/observation.hpp"
#include "lyra/runtime/read_report.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/runtime/wait.hpp"

namespace lyra::runtime {

// The waits woken by what happens on storage: an event control (LRM 9.4.2,
// 15.5.2), an implicit sensitivity (LRM 9.2.2.2.1, 9.4.2.2), a `wait`
// statement (LRM 9.4.3), and the sensitivity of a continuous process (LRM
// 4.9.1, 4.9.2). They part company in five places: what they watch is fixed
// where the wait is built or known only once the process has evaluated,
// whether a change made while the process runs is an occurrence for it,
// whether each write is one or only the net change an interval leaves, whether
// resuming from them is a report flush point (LRM 12.4.2.1, 16.4.2), and what
// starting a stopped process again asks of them (LRM 9.7).

// A wait on one trigger per leaf, the storage it watches fixed for as long as
// the body holding it runs: what a change there is measured from is taken where
// the wait begins, and only an occurrence while the body is parked on it is
// one. An empty span is a wait that is never woken. The leaves are read here
// and not kept, laid out in one array or each where its builder left it.
[[nodiscard]] auto WaitOn(std::span<const Trigger> triggers) -> Wait;
[[nodiscard]] auto WaitOn(std::span<const Trigger* const> triggers) -> Wait;

// The wait of an `always_comb` or `always_latch` on its implicit list (LRM
// 9.2.2.2.1), which models combinational or latched logic and is triggered when
// what it reads changes (LRM 9.2.2.2, 9.2.2.3). Being reached decides it, so it
// begins with nothing to measure from. It is one of two observers, as the
// dialect the procedure's library is built in says.
class ImplicitListAwaiter : public Awaiter {
 public:
  auto Begin() -> Resumption override {
    return OnAnOccurrence{};
  }

  // Resuming from it is a violation report flush point (LRM 12.4.2.1).
  [[nodiscard]] auto IsReportFlushPoint() const -> bool override {
    return true;
  }

 protected:
  explicit ImplicitListAwaiter(std::size_t places) : Awaiter(places) {
  }
};

// The per-change observer, in a design built as the standard reads: told each
// change of what the procedure reads (LRM 4.3, 4.5).
class PerChangeAwaiter final : public ImplicitListAwaiter {
 public:
  explicit PerChangeAwaiter(std::span<const Trigger> triggers);
};

// The per-interval observer, in a design built to coalesce combinational
// updates. A procedure writing a default and then a value changes what it
// writes twice in one execution, and each change is an event (LRM 4.3, 4.5),
// so two such procedures each reading the other's default wake each other for
// ever in a schedule running each to its stop. This observer is told instead
// the net change over an interval -- one execution of another such procedure
// -- of what it reads, and a write no interval accumulated as it lands.
//
// An interval's originals are shared by every observer listening while it
// runs, so the interval ends, telling each the net change so far, whenever one
// of them begins or stops listening; a new one then begins. An observer is
// thereby never told what changed before it listened, at the cost of an extra
// wake for the others where an interval is cut short.
class PerIntervalAwaiter final : public ImplicitListAwaiter {
 public:
  explicit PerIntervalAwaiter(std::span<const Trigger> triggers);

  // It begins as the procedure's own execution ends, whose writes are no event
  // for it (LRM 9.4.2), and as it is started again, which waits afresh (LRM
  // 9.7): either way the interval under way ends first.
  auto Begin() -> Resumption override;

  void Suspending() override;
};

// The wait of an `always_comb` or `always_latch` on the implicit list `report`
// settled, for each dialect: a per-change observer, and a per-interval one.
// Each states the dialect of the process making it, whose executions are then
// intervals or not.
[[nodiscard]] auto WaitPerChangeOnImplicitList(const ReadReport* report)
    -> Wait;
[[nodiscard]] auto WaitPerIntervalOnImplicitList(const ReadReport* report)
    -> Wait;

// The wait of a continuous assignment or a procedural continuous assignment,
// on one trigger per source element of its expression. Such a process is
// sensitive to those elements whenever it executes, and its own update is a
// change of one of them like any other (LRM 4.5, 4.9.1, 4.9.2): a change that
// reaches the wait while the process runs is kept, and the process's next stop
// evaluates it again in the active region of this time step instead of
// waiting.
[[nodiscard]] auto WaitContinuouslyOn(std::span<const Trigger> triggers)
    -> Wait;
[[nodiscard]] auto WaitContinuouslyOn(std::span<const Trigger* const> triggers)
    -> Wait;

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
