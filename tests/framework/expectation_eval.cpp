#include "tests/framework/expectation_eval.hpp"

#include <cstdint>
#include <format>
#include <string>
#include <variant>

#include "tests/framework/test_case.hpp"
#include "tests/framework/test_result.hpp"
#include "tests/framework/test_value.hpp"

namespace lyra::test {
namespace {

auto CheckOutputContains(
    const std::string& actual, const ExpectedOutput& expected,
    std::string_view context) -> std::string {
  if (expected.IsExact()) {
    if (actual != expected.exact.value()) {
      return std::format(
          "{}: expected exact match but got different output", context);
    }
  } else {
    for (const auto& substring : expected.contains) {
      if (actual.find(substring) == std::string::npos) {
        return std::format(
            "{}: expected to contain '{}' but did not", context, substring);
      }
    }
  }
  for (const auto& substring : expected.not_contains) {
    if (actual.find(substring) != std::string::npos) {
      return std::format(
          "{}: expected NOT to contain '{}' but did", context, substring);
    }
  }
  return {};
}

auto IntegralEqual(const IntegralValue& a, const IntegralValue& b) -> bool {
  if (a.width != b.width) return false;
  size_t num_words = (a.width + 63) / 64;
  for (size_t i = 0; i < num_words; ++i) {
    uint64_t a_val = i < a.value.size() ? a.value[i] : 0;
    uint64_t b_val = i < b.value.size() ? b.value[i] : 0;
    uint64_t a_unk = i < a.unknown.size() ? a.unknown[i] : 0;
    uint64_t b_unk = i < b.unknown.size() ? b.unknown[i] : 0;
    if (a_val != b_val || a_unk != b_unk) return false;
  }
  return true;
}

auto Uint64ToIntegral(uint64_t val, uint32_t width) -> IntegralValue {
  IntegralValue result;
  result.width = width;
  size_t num_words = (width + 63) / 64;
  result.value.resize(num_words, 0);
  result.unknown.resize(num_words, 0);
  if (!result.value.empty()) {
    result.value[0] = val;
  }
  if (width > 0 && width % 64 != 0) {
    uint64_t mask = (uint64_t{1} << (width % 64)) - 1;
    result.value.back() &= mask;
  }
  return result;
}

auto CheckVariables(
    const std::map<std::string, TestValue>& actual,
    const std::map<std::string, ExpectedValue>& expected) -> std::string {
  for (const auto& [name, expected_val] : expected) {
    auto it = actual.find(name);
    if (it == actual.end()) {
      return std::format("missing variable: {}", name);
    }
    const TestValue& actual_val = it->second;

    if (std::holds_alternative<double>(expected_val)) {
      const auto* actual_ptr = std::get_if<double>(&actual_val);
      if (actual_ptr == nullptr) {
        return std::format(
            "type mismatch for variable {} (expected double)", name);
      }
      if (*actual_ptr != std::get<double>(expected_val)) {
        return std::format("variable {} value mismatch", name);
      }
      continue;
    }

    const auto* actual_iv = std::get_if<IntegralValue>(&actual_val);
    if (actual_iv == nullptr) {
      return std::format(
          "type mismatch for variable {} (expected integral)", name);
    }

    IntegralValue expected_iv;
    if (std::holds_alternative<IntegralValue>(expected_val)) {
      expected_iv = std::get<IntegralValue>(expected_val);
    } else {
      expected_iv =
          Uint64ToIntegral(std::get<uint64_t>(expected_val), actual_iv->width);
    }

    if (!IntegralEqual(*actual_iv, expected_iv)) {
      return std::format("variable {} value mismatch", name);
    }
  }
  return {};
}

}  // namespace

auto ValidateTestContract(const TestCase& test_case, BackendKind backend)
    -> std::string {
  if (test_case.expected_runtime_fatal.has_value() &&
      backend != BackendKind::kJit) {
    return "expected_runtime_fatal is only supported on the JIT backend";
  }
  return {};
}

auto EvaluateExpectations(
    const TestCase& test_case, const CaseExecutionResult& result)
    -> ExpectationVerdict {
  // Expected runtime-fatal: assert crash with expected stderr
  if (test_case.expected_runtime_fatal.has_value()) {
    if (result.execution.outcome != ExecutionOutcome::kCrashed) {
      return {
          .passed = false,
          .failure_message = std::format(
              "expected signal-terminated (abort), got outcome {}",
              static_cast<int>(result.execution.outcome)),
      };
    }
    for (const auto& expected :
         test_case.expected_runtime_fatal->stderr_contains) {
      if (result.execution.stderr_text.find(expected) == std::string::npos) {
        return {
            .passed = false,
            .failure_message = std::format(
                "expected stderr to contain '{}', got: {}", expected,
                result.execution.stderr_text),
        };
      }
    }
    return {.passed = true, .failure_message = {}};
  }

  // Expected compilation/setup error
  if (test_case.expected_error.has_value()) {
    bool is_expected_error =
        result.execution.outcome == ExecutionOutcome::kFrontendError ||
        result.execution.outcome == ExecutionOutcome::kBackendSetupError ||
        result.execution.outcome == ExecutionOutcome::kExecutionFailed;
    if (!is_expected_error) {
      return {
          .passed = false,
          .failure_message = std::format(
              "expected classified error outcome, got {}",
              static_cast<int>(result.execution.outcome)),
      };
    }
    auto msg = CheckOutputContains(
        result.execution.error_message, *test_case.expected_error,
        "error output");
    if (!msg.empty()) {
      return {.passed = false, .failure_message = msg};
    }
    return {.passed = true, .failure_message = {}};
  }

  // Normal case: assert success
  if (result.execution.outcome != ExecutionOutcome::kSuccess) {
    return {
        .passed = false,
        .failure_message = result.execution.error_message,
    };
  }

  if (!test_case.expected_values.empty()) {
    auto msg =
        CheckVariables(result.artifacts.variables, test_case.expected_values);
    if (!msg.empty()) {
      return {.passed = false, .failure_message = msg};
    }
  }

  if (test_case.expected_time.has_value()) {
    if (result.artifacts.final_time != *test_case.expected_time) {
      return {
          .passed = false,
          .failure_message = std::format(
              "time mismatch: expected {}, got {}", *test_case.expected_time,
              result.artifacts.final_time),
      };
    }
  }

  if (test_case.expected_stdout.has_value()) {
    auto msg = CheckOutputContains(
        result.artifacts.captured_output, *test_case.expected_stdout, "stdout");
    if (!msg.empty()) {
      return {.passed = false, .failure_message = msg};
    }
  }

  if (test_case.expected_compiler_output.has_value()) {
    auto msg = CheckOutputContains(
        result.artifacts.compiler_output, *test_case.expected_compiler_output,
        "compiler output");
    if (!msg.empty()) {
      return {.passed = false, .failure_message = msg};
    }
  }

  if (test_case.expected_cover_hits.has_value()) {
    const auto& expected = *test_case.expected_cover_hits;
    if (result.artifacts.cover_hits.size() != expected.size()) {
      return {
          .passed = false,
          .failure_message = std::format(
              "cover site count mismatch: expected {}, got {}", expected.size(),
              result.artifacts.cover_hits.size()),
      };
    }
    for (size_t i = 0; i < expected.size(); ++i) {
      if (result.artifacts.cover_hits[i] != expected[i]) {
        return {
            .passed = false,
            .failure_message = std::format(
                "cover site {} hit count mismatch: expected {}, got {}", i,
                expected[i], result.artifacts.cover_hits[i]),
        };
      }
    }
  }

  if (!test_case.expected_files.empty()) {
    for (const auto& [filename, expected] : test_case.expected_files) {
      auto it = result.artifacts.produced_files.find(filename);
      if (it == result.artifacts.produced_files.end()) {
        return {
            .passed = false,
            .failure_message =
                std::format("expected file '{}' was not produced", filename),
        };
      }
      auto msg = CheckOutputContains(
          it->second, expected, std::string("file ") + filename);
      if (!msg.empty()) {
        return {.passed = false, .failure_message = msg};
      }
    }
  }

  return {.passed = true, .failure_message = {}};
}

}  // namespace lyra::test
