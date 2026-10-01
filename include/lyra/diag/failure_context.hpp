#pragma once

#include <exception>
#include <expected>
#include <string_view>
#include <type_traits>
#include <variant>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"

namespace lyra::diag {

// What the compiler is working on, said by whoever is walking it. A failure is
// raised far below the walk, at a site holding an id or a type and neither a
// location nor the unit, so the walk says where it is and the failure collects
// what was said above it.
//
// What is said is one of two things. A place is a piece of source being worked
// on -- a declaration, a statement, an expression -- and a failure is reported
// at the innermost one, the rest only being wider views of the same spot. A
// name is what the source cannot show: which unit, since one module may be
// compiled as several, or which function once the source is behind. Every name
// is kept and the report carries them on its one line.
//
// One of these lives on the stack for as long as the work it names. When an
// exception leaves the work through it, it adds what it said to its thread's
// trail for whoever catches the exception to take. Nothing is composed unless
// that happens.
class FailureContext {
 public:
  // The piece of source being worked on.
  explicit FailureContext(SourceSpan place);
  ~FailureContext();

  // Work on something named, which the name outlives. A unit is named as it is
  // compiled; a function is one of the lowered program, which the compiler may
  // have named.
  static auto InUnit(std::string_view name) -> FailureContext;
  static auto InFunction(std::string_view name) -> FailureContext;

  FailureContext(const FailureContext&) = delete;
  auto operator=(const FailureContext&) -> FailureContext& = delete;
  FailureContext(FailureContext&&) = delete;
  auto operator=(FailureContext&&) -> FailureContext& = delete;

 private:
  struct Place {
    SourceSpan at;
  };
  struct Named {
    std::string_view what;
    std::string_view name;
  };
  using Said = std::variant<Place, Named>;

  explicit FailureContext(Said said);
  FailureContext(std::string_view what, std::string_view name);

  Said said_;
  int exceptions_in_flight_;
};

// The compiler's own failure as a report: what `failure` says and the names the
// work on this thread gave as the exception left it, standing at the innermost
// place it gave. `failure` is anything that should not have been thrown -- a
// broken invariant, or an exception from a library. What was said is taken, so
// the next failure on this thread starts from nothing.
auto InternalFailure(const std::exception& failure) -> Diagnostic;

// Runs `work`, which answers with a `Result`, as a piece of work whose failure
// stays its own: the compiler's own failure inside it becomes the error that
// answer carries. This is where a failure stops unwinding, so it is used where
// what lies outside the work is unaffected by the work having broken.
template <typename Work>
auto ContainFailure(Work work) -> std::invoke_result_t<Work> {
  try {
    return work();
  } catch (const std::exception& failure) {
    return std::unexpected(InternalFailure(failure));
  }
}

}  // namespace lyra::diag
