#pragma once

#include <cstddef>
#include <cstdint>
#include <new>
#include <unordered_map>
#include <vector>

#include "lyra/base/source_location.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/support/assertion_control.hpp"

namespace lyra::runtime {

class CancellationTarget;
class EvaluationAttempts;
class RuntimeEffects;
class Scope;

// What the assertion control tasks set for one assertion, expect statement or
// violation report (LRM 20.11): whether it checks, which of its actions an
// outcome may run, and whether it is locked against every change but Unlock.
// There is one wherever the statement's static storage is, so each instance
// of a design element has its own and two instances are told apart by the
// tasks as they are by a hierarchical name.
//
// A statement reads what it may do once, where an execution of it begins, and
// keeps that, since a change made while an execution runs does not reach it.
// Kill is the one control that does reach what has already begun: it aborts
// the attempts in flight and withdraws the reports still pending, which hold
// the generation they were made under and act only while it stands, so a
// later On does not bring them back.
//
// It is also where the statement's pending reports are queued -- a deferred
// assertion's action (LRM 16.4) and a violation report (LRM 12.4.2.1) -- since
// this control's Kill withdraws both before they mature.
//
// It is usable from the moment its storage exists. A statement can run before
// the design's storage is brought up -- a function a parameter is computed
// with runs while the design is built -- and registering only makes the
// control one a task can find.
class AssertionControl {
 public:
  AssertionControl();
  AssertionControl(const AssertionControl&) = delete;
  auto operator=(const AssertionControl&) -> AssertionControl& = delete;
  AssertionControl(AssertionControl&&) = delete;
  auto operator=(AssertionControl&&) -> AssertionControl& = delete;
  ~AssertionControl();

  // Makes it one the tasks select, as a statement of `type` stating
  // `directive` (a `support::AssertionDirectiveType`, 0 for a statement that
  // is no assertion) that the instance `in` holds.
  void RegisterIn(
      RuntimeEffects& effects, Scope* in, std::uint64_t type,
      std::uint64_t directive);
  // The same for one no instance holds -- one a class method or a package
  // subroutine declares -- which only a task naming no scope reaches.
  void Register(
      RuntimeEffects& effects, std::uint64_t type, std::uint64_t directive);
  // Makes it one a task naming `block` selects: a named block it stands in,
  // its own label's included (LRM 16.3).
  void RegisterWithin(RuntimeEffects& effects, const CancellationTarget* block);
  // The attempts a Kill aborts.
  void Watch(EvaluationAttempts& attempts);

  // What an execution beginning now may do, as `support::kAssertion*` bits.
  [[nodiscard]] auto Allowed() const -> std::uint64_t {
    return allowed_;
  }

  // Whether a task asking for `types` and `directives` selects it (LRM 20.11):
  // its type is among them, and where it is an assertion its directive too.
  [[nodiscard]] auto SelectedBy(
      std::uint64_t types, std::uint64_t directives) const -> bool;
  // What `control` does to it, where a task has selected it.
  void Apply(support::AssertionControlType control);

  // A deferred assertion's action, queued as RuntimeEffects queues one and
  // withdrawn by a Kill before it matures (LRM 16.4.1). `at` is where an
  // observed one was written, which a refusal names.
  void SubmitObserved(Coroutine<void> body, base::SourceLocation at) const;
  void SubmitFinal(Coroutine<void> body) const;

  // Where what the statement queues now is kept: made in the effects of the
  // region it matures in and matured there unless a source that can flush it
  // moves first, and ended at once while checking is off. Which sources those
  // are is the construct's: a violation report is flushed by its process's
  // flush points (LRM 12.4.2.1), and a procedural concurrent assertion's
  // pending instance also by a `disable` of its label or of the process's
  // outermost scope (LRM 16.14.6.2, 16.14.6.4); either is withdrawn by a Kill
  // of this control (LRM 20.11).
  [[nodiscard]] auto Allocate(std::size_t size, std::size_t alignment) -> void*;
  void Commit(void* made) const;

  // The generation work made now holds, which a Kill ends.
  [[nodiscard]] auto Generation() const -> std::uint64_t {
    return generation_;
  }

 private:
  void Describe(std::uint64_t type, std::uint64_t directive);

  EvaluationAttempts* attempts_ = nullptr;
  support::AssertionType type_ = support::AssertionType::kConcurrent;
  std::uint64_t directive_ = 0;
  std::uint64_t allowed_ =
      support::kAssertionChecks | support::kAssertionPassesVacuously |
      support::kAssertionPassesNonvacuously | support::kAssertionFails;
  std::uint64_t generation_ = 0;
  bool locked_ = false;
};

// One control's generation as work recorded it where it was made, which holds
// for as long as no Kill has reached that control since.
struct CapturedControl {
  const AssertionControl* control;
  std::uint64_t generation;

  [[nodiscard]] auto Holds() const -> bool {
    return control->Generation() == generation;
  }
};

// Every control the design registered, by where a task naming a scope or a
// block finds it.
//
// A control an instance holds is recorded at that instance and at every scope
// above it, with how many levels below each it stands, so a task naming a
// scope reads the controls under it and nothing of the scopes between: most
// scopes of a design hold no control.
class AssertionControls {
 public:
  void AddIn(AssertionControl& control, const Scope& in);
  void Add(AssertionControl& control);
  void AddWithin(AssertionControl& control, const CancellationTarget& block);

  // A task naming no scope (LRM 20.11): every control the instances `levels`
  // below the tops hold, and every one no instance holds.
  void ApplyToDesign(
      Scope& root, support::AssertionControlType control, std::uint64_t types,
      std::uint64_t directives, std::int64_t levels);
  // One naming an instance or a generate block: the controls of the instances
  // `levels` from it down.
  void ApplyToScope(
      const Scope& scope, support::AssertionControlType control,
      std::uint64_t types, std::uint64_t directives, std::int64_t levels);
  // One naming a block or an assertion's label: the controls within it.
  void ApplyToBlock(
      const CancellationTarget& block, support::AssertionControlType control,
      std::uint64_t types, std::uint64_t directives);

 private:
  // A control under a scope, and the levels between: 0 where the scope holds
  // it, one more for each instance between them, the holding one included.
  struct Below {
    AssertionControl* control;
    std::int64_t levels;
  };

  static void ApplyEach(
      const std::vector<AssertionControl*>& controls,
      support::AssertionControlType control, std::uint64_t types,
      std::uint64_t directives);

  std::unordered_map<const Scope*, std::vector<Below>> under_;
  std::vector<AssertionControl*> held_by_none_;
  std::unordered_map<const CancellationTarget*, std::vector<AssertionControl*>>
      by_block_;
};

}  // namespace lyra::runtime

// The allocation functions a placement new-expression naming a control calls,
// so a violation report is made where the control keeps it, as a region's
// deferred effects have theirs.
[[nodiscard]] auto operator new(
    std::size_t size, lyra::runtime::AssertionControl* control) -> void*;
[[nodiscard]] auto operator new(
    std::size_t size, std::align_val_t alignment,
    lyra::runtime::AssertionControl* control) -> void*;
