#ifndef TESTS_FRAMEWORK_TEST_DISCOVERY_HPP
#define TESTS_FRAMEWORK_TEST_DISCOVERY_HPP

#include <string>
#include <vector>

#include "tests/framework/suite.hpp"
#include "tests/framework/test_case.hpp"

namespace lyra::test {

// Parsed command-line arguments
struct CommandLineArgs {
  std::string suite;
  std::string backend;
  std::string test_file;
  std::string shard_count;  // Framework-level sharding (0 = disabled)
  std::string shard_index;  // 0-based shard index
  // Timeout in seconds (0 = no timeout). Semantics are backend-dependent:
  //   JIT: whole-case timeout (fork-isolated, covers frontend+execution)
  //   AOT/LLI: simulation subprocess timeout only (frontend/lowering/link
  //     are not covered; runs direct for performance)
  int timeout_seconds = 0;
  bool two_state = false;  // Force two-state mode (--test_file only)
};

// Explicit sharding specification. Callers resolve from CLI/env and pass in.
struct ShardSpec {
  bool enabled = false;
  int shard_count = 1;
  int shard_index = 0;
};

// Fully resolved, ordered manifest of runnable test cases.
struct Manifest {
  BackendKind backend;
  bool force_two_state = false;
  std::vector<TestCase> cases;
};

// Resolve sharding from CLI args and Bazel environment variables.
// CLI args take precedence over env vars. Returns disabled spec if neither set.
auto ResolveShardSpec(const CommandLineArgs& args) -> ShardSpec;

// Single canonical discovery entrypoint. Resolves suite or test_file,
// loads all cases, applies sharding if specified. Does not read environment.
auto BuildManifest(const CommandLineArgs& args, const ShardSpec& shard = {})
    -> Manifest;

}  // namespace lyra::test

#endif  // TESTS_FRAMEWORK_TEST_DISCOVERY_HPP
