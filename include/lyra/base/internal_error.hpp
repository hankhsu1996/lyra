#pragma once

#include <stdexcept>
#include <string>

namespace lyra {

// Exception for compiler/runtime invariant violations: "impossible" states,
// unimplemented paths the rest of the pipeline must not reach. User-source
// errors must use diag::Diagnostic / diag::Result, not InternalError.
// Callers pass the bare invariant message (typically `"FunctionName: short
// detail"`); the constructor wraps it with an "Internal error:" prefix and
// the bug-report URL so every site stays consistent.
class InternalError final : public std::logic_error {
 public:
  explicit InternalError(std::string message);
};

}  // namespace lyra
