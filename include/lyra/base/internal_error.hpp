#pragma once

#include <stdexcept>
#include <string>

namespace lyra {

// Exception for compiler/runtime invariant violations: "impossible" states,
// unimplemented paths the rest of the pipeline must not reach. User-source
// errors must use diag::Diagnostic / diag::Result, not InternalError.
class InternalError final : public std::logic_error {
 public:
  explicit InternalError(std::string message);
};

}  // namespace lyra
