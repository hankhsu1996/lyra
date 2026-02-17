#pragma once

#include <regex>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

namespace lyra::test {

// Backend represents the semantic engine used to execute tests.
// This is the execution model, not the compilation target.
enum class BackendKind {
  kMir,  // MIR interpreter
  kJit,  // In-process ORC JIT
  kLli,  // Out-of-process LLI interpreter (for debugging/comparison)
};

// Parse backend kind from string (case-sensitive: "mir", "jit", "lli")
inline auto ParseBackendKind(std::string_view backend_string) -> BackendKind {
  if (backend_string == "mir") {
    return BackendKind::kMir;
  }
  if (backend_string == "jit") {
    return BackendKind::kJit;
  }
  if (backend_string == "lli") {
    return BackendKind::kLli;
  }
  throw std::runtime_error("Unknown backend: " + std::string(backend_string));
}

// Suite defines a test configuration: backend + test selection.
// Suites are closed contracts - the backend is fixed and cannot be overridden.
//
// Pattern matching uses ECMAScript regex (std::regex default).
// Patterns match against normalized relative paths (forward slashes only).
// Empty include_regex means "include all YAML files".
// Exclude takes precedence over include.
struct Suite {
  std::string name;
  BackendKind backend;
  bool force_two_state = false;
  std::vector<std::regex> include_regex;  // Compiled regex patterns
  std::vector<std::regex> exclude_regex;  // Compiled regex patterns
};

}  // namespace lyra::test
