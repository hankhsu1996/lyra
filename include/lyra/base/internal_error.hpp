#pragma once

#include <stdexcept>
#include <string>

namespace lyra {

// Exception for compiler/runtime invariant violations: "impossible" states,
// unimplemented paths the rest of the pipeline must not reach. User-source
// errors must use diag::Diagnostic / diag::Result, not InternalError.
// Callers pass the bare invariant message (typically `"FunctionName: short
// detail"`); the constructor appends the bug-report URL, since every one of
// these is a defect to report. Naming it as an internal error is left to
// whichever surface reports it, so the reader never sees the label twice.
class InternalError final : public std::logic_error {
 public:
  explicit InternalError(std::string message);
};

}  // namespace lyra
