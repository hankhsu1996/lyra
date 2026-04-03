#pragma once

#include <map>
#include <optional>
#include <ostream>
#include <string>
#include <vector>

#include "tests/framework/test_value.hpp"

namespace lyra::test {

struct SourceFile {
  std::string name;
  std::string content;
};

struct ExpectedOutput {
  std::optional<std::string> exact;
  std::vector<std::string> contains;
  std::vector<std::string> not_contains;

  [[nodiscard]] auto IsExact() const -> bool {
    return exact.has_value();
  }
};

// Expected runtime fatal (abort/crash during simulation).
// The test is run in a subprocess; non-zero exit is expected.
struct FatalRunExpectation {
  std::vector<std::string> stderr_contains;
};

struct MutationExpectation {
  std::optional<size_t> min_count;
  std::vector<std::string> contains_kind;  // "value_write", "structural", etc.
};

struct TestCase {
  std::string name;
  std::string feature;
  std::string source_yaml;  // Path to YAML file for error reporting
  std::string sv_code;
  std::vector<SourceFile> files;
  std::vector<std::string> plusargs;
  std::vector<std::string> param_overrides;  // Compile-time param overrides
  std::map<std::string, ExpectedValue> expected_values;
  std::optional<uint64_t> expected_time;
  std::optional<ExpectedOutput> expected_stdout;
  std::map<std::string, ExpectedOutput> expected_files;
  std::optional<ExpectedOutput> expected_compiler_output;  // Compiler artifacts
  std::optional<ExpectedOutput> expected_error;  // Expected compilation error
  std::optional<MutationExpectation> expected_mutations;
  std::optional<FatalRunExpectation> expected_runtime_fatal;
  bool pedantic = false;        // Strict LRM compliance mode for this test
  bool trace_summary = false;   // Enable trace summary output for this test
  bool signal_trace = false;    // Enable text signal trace for this test
  bool dump_slot_meta = false;  // Dump slot metadata registry (test-only)
  bool dump_specialization_map = false;  // Dump specialization grouping
  bool dump_repertoire = false;          // Dump generate repertoire observation
  bool dump_repertoire_desc = false;     // Dump repertoire descriptor
  bool dump_dpi_header = false;          // Dump generated DPI-C header
  bool dump_llvm_ir = false;             // Dump LLVM IR module
  bool disable_assertions = false;
  bool single_unit = false;
  std::vector<std::string> defines;
  std::vector<std::string> dpi_sources;  // Companion C source paths (DPI)
  [[nodiscard]] auto IsMultiFile() const -> bool {
    return !files.empty();
  }
};

// GTest printer for readable test names
inline void PrintTo(const TestCase& test_case, std::ostream* os) {
  *os << test_case.feature << "/" << test_case.name;
}

}  // namespace lyra::test
