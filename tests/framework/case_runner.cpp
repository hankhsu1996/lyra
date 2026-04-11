#include "tests/framework/case_runner.hpp"

#include <chrono>
#include <cstdint>
#include <cstring>
#include <exception>
#include <format>
#include <map>
#include <string>
#include <string_view>
#include <unistd.h>
#include <variant>
#include <vector>
#include <yaml-cpp/yaml.h>

#include "tests/framework/process_runner.hpp"
#include "tests/framework/test_executor.hpp"
#include "tests/framework/test_result.hpp"
#include "tests/framework/test_value.hpp"

namespace lyra::test {
namespace {

// Serialize CaseExecutionResult to YAML and write to fd.
void WriteCaseResult(int fd, const CaseExecutionResult& result) {
  YAML::Emitter out;
  out << YAML::BeginMap;

  out << YAML::Key << "outcome" << YAML::Value
      << static_cast<int>(result.execution.outcome);
  out << YAML::Key << "error_message" << YAML::Value
      << result.execution.error_message;
  out << YAML::Key << "stderr_text" << YAML::Value
      << result.execution.stderr_text;
  out << YAML::Key << "exit_code" << YAML::Value << result.execution.exit_code;
  out << YAML::Key << "signal_number" << YAML::Value
      << result.execution.signal_number;

  out << YAML::Key << "captured_output" << YAML::Value
      << result.artifacts.captured_output;
  out << YAML::Key << "compiler_output" << YAML::Value
      << result.artifacts.compiler_output;
  out << YAML::Key << "final_time" << YAML::Value
      << result.artifacts.final_time;

  out << YAML::Key << "variables" << YAML::Value << YAML::BeginMap;
  for (const auto& [name, val] : result.artifacts.variables) {
    out << YAML::Key << name << YAML::Value;
    if (const auto* d = std::get_if<double>(&val)) {
      out << YAML::BeginMap;
      out << YAML::Key << "t" << YAML::Value << "d";
      out << YAML::Key << "v" << YAML::Value << *d;
      out << YAML::EndMap;
    } else if (const auto* iv = std::get_if<IntegralValue>(&val)) {
      out << YAML::BeginMap;
      out << YAML::Key << "t" << YAML::Value << "i";
      out << YAML::Key << "w" << YAML::Value << iv->width;
      out << YAML::Key << "v" << YAML::Value << YAML::Flow << YAML::BeginSeq;
      for (auto w : iv->value) out << w;
      out << YAML::EndSeq;
      out << YAML::Key << "u" << YAML::Value << YAML::Flow << YAML::BeginSeq;
      for (auto w : iv->unknown) out << w;
      out << YAML::EndSeq;
      out << YAML::EndMap;
    }
  }
  out << YAML::EndMap;

  out << YAML::Key << "cover_hits" << YAML::Value << YAML::Flow
      << YAML::BeginSeq;
  for (auto hit : result.artifacts.cover_hits) out << hit;
  out << YAML::EndSeq;

  out << YAML::Key << "nba_stats" << YAML::Value << YAML::BeginMap;
  out << YAML::Key << "captured" << YAML::Value
      << result.artifacts.nba_stats.captured;
  out << YAML::Key << "generic_queue" << YAML::Value
      << result.artifacts.nba_stats.generic_queue;
  out << YAML::Key << "deferred_local" << YAML::Value
      << result.artifacts.nba_stats.deferred_local;
  out << YAML::EndMap;

  out << YAML::Key << "produced_files" << YAML::Value << YAML::BeginMap;
  for (const auto& [name, content] : result.artifacts.produced_files) {
    out << YAML::Key << name << YAML::Value << content;
  }
  out << YAML::EndMap;

  out << YAML::Key << "timings" << YAML::Value << YAML::BeginMap;
  out << YAML::Key << "p" << YAML::Value << result.artifacts.timings.parse;
  out << YAML::Key << "h" << YAML::Value << result.artifacts.timings.hir_lower;
  out << YAML::Key << "m" << YAML::Value << result.artifacts.timings.mir_lower;
  out << YAML::Key << "l" << YAML::Value << result.artifacts.timings.llvm_lower;
  out << YAML::Key << "b" << YAML::Value << result.artifacts.timings.backend;
  out << YAML::Key << "e" << YAML::Value << result.artifacts.timings.execute;
  out << YAML::Key << "t" << YAML::Value << result.artifacts.timings.total;
  out << YAML::EndMap;

  out << YAML::EndMap;

  std::string data = out.c_str();
  size_t offset = 0;
  while (offset < data.size()) {
    // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
    auto n = write(fd, data.data() + offset, data.size() - offset);
    if (n <= 0) break;
    offset += static_cast<size_t>(n);
  }
}

// Deserialize CaseExecutionResult from YAML string.
auto ParseCaseResult(std::string_view yaml) -> CaseExecutionResult {
  auto node = YAML::Load(std::string(yaml));
  CaseExecutionResult result;

  result.execution.outcome =
      static_cast<ExecutionOutcome>(node["outcome"].as<int>());
  result.execution.error_message = node["error_message"].as<std::string>("");
  result.execution.stderr_text = node["stderr_text"].as<std::string>("");
  result.execution.exit_code = node["exit_code"].as<int>(0);
  result.execution.signal_number = node["signal_number"].as<int>(0);

  result.artifacts.captured_output =
      node["captured_output"].as<std::string>("");
  result.artifacts.compiler_output =
      node["compiler_output"].as<std::string>("");
  result.artifacts.final_time = node["final_time"].as<uint64_t>(0);

  if (node["variables"]) {
    for (const auto& kv : node["variables"]) {
      auto name = kv.first.as<std::string>();
      auto type = kv.second["t"].as<std::string>();
      if (type == "d") {
        result.artifacts.variables[name] = kv.second["v"].as<double>();
      } else if (type == "i") {
        IntegralValue iv;
        iv.width = kv.second["w"].as<uint32_t>();
        for (const auto& w : kv.second["v"])
          iv.value.push_back(w.as<uint64_t>());
        for (const auto& w : kv.second["u"])
          iv.unknown.push_back(w.as<uint64_t>());
        result.artifacts.variables[name] = iv;
      }
    }
  }

  if (node["cover_hits"]) {
    for (const auto& hit : node["cover_hits"])
      result.artifacts.cover_hits.push_back(hit.as<uint64_t>());
  }

  if (node["nba_stats"]) {
    const auto& ns = node["nba_stats"];
    result.artifacts.nba_stats.captured = ns["captured"].as<bool>(false);
    result.artifacts.nba_stats.generic_queue =
        ns["generic_queue"].as<uint64_t>(0);
    result.artifacts.nba_stats.deferred_local =
        ns["deferred_local"].as<uint64_t>(0);
  }

  if (node["produced_files"]) {
    for (const auto& kv : node["produced_files"]) {
      result.artifacts.produced_files[kv.first.as<std::string>()] =
          kv.second.as<std::string>("");
    }
  }

  if (node["timings"]) {
    const auto& t = node["timings"];
    result.artifacts.timings.parse = t["p"].as<double>(0.0);
    result.artifacts.timings.hir_lower = t["h"].as<double>(0.0);
    result.artifacts.timings.mir_lower = t["m"].as<double>(0.0);
    result.artifacts.timings.llvm_lower = t["l"].as<double>(0.0);
    result.artifacts.timings.backend = t["b"].as<double>(0.0);
    result.artifacts.timings.execute = t["e"].as<double>(0.0);
    result.artifacts.timings.total = t["t"].as<double>(0.0);
  }

  return result;
}

// Interpret a ProcessOutcome from RunInFork into a CaseExecutionResult.
auto InterpretForkOutcome(const ProcessOutcome& proc) -> CaseExecutionResult {
  switch (proc.termination) {
    case TerminationKind::kExitedNormally:
      try {
        return ParseCaseResult(proc.stdout_text);
      } catch (const std::exception& e) {
        CaseExecutionResult result;
        result.execution.outcome = ExecutionOutcome::kInfraError;
        result.execution.error_message =
            std::format("failed to parse child result: {}", e.what());
        result.execution.stderr_text = proc.stderr_text;
        return result;
      }
    case TerminationKind::kExitedNonZero: {
      try {
        return ParseCaseResult(proc.stdout_text);
      } catch (...) {
        CaseExecutionResult result;
        result.execution.outcome = ExecutionOutcome::kInfraError;
        result.execution.error_message =
            std::format("child exited {} without valid result", proc.exit_code);
        result.execution.stderr_text = proc.stderr_text;
        result.execution.exit_code = proc.exit_code;
        return result;
      }
    }
    case TerminationKind::kSignaled: {
      CaseExecutionResult result;
      result.execution.outcome = ExecutionOutcome::kCrashed;
      result.execution.error_message =
          std::format("child killed by signal {}", proc.signal_number);
      result.execution.stderr_text = proc.stderr_text;
      result.execution.signal_number = proc.signal_number;
      return result;
    }
    case TerminationKind::kTimedOut: {
      CaseExecutionResult result;
      result.execution.outcome = ExecutionOutcome::kTimedOut;
      result.execution.error_message = "child timed out";
      result.execution.stderr_text = proc.stderr_text;
      return result;
    }
    case TerminationKind::kSpawnFailed:
    case TerminationKind::kWaitFailed: {
      CaseExecutionResult result;
      result.execution.outcome = ExecutionOutcome::kInfraError;
      result.execution.error_message =
          std::format("fork infrastructure failure: {}", proc.stderr_text);
      result.execution.stderr_text = proc.stderr_text;
      return result;
    }
  }
  CaseExecutionResult result;
  result.execution.outcome = ExecutionOutcome::kInfraError;
  return result;
}

}  // namespace

// Fork-isolated execution for JIT backend.
auto RunIsolatedCase(
    const TestCase& test_case, BackendKind backend, bool force_two_state,
    std::chrono::seconds timeout) -> CaseExecutionResult {
  auto proc = RunInFork(
      [&](int result_fd) {
        auto case_result = ExecuteTestCase(test_case, backend, force_two_state);
        WriteCaseResult(result_fd, case_result);
      },
      timeout);
  return InterpretForkOutcome(proc);
}

auto RunCase(
    const TestCase& test_case, BackendKind backend, bool force_two_state,
    std::chrono::seconds timeout) -> CaseExecutionResult {
  switch (backend) {
    case BackendKind::kJit:
      // Fork for crash isolation. Timeout covers the entire case.
      return RunIsolatedCase(test_case, backend, force_two_state, timeout);
    case BackendKind::kAot:
    case BackendKind::kLli:
      // Direct execution (no fork). Timeout applies to the simulation
      // subprocess only; frontend/lowering/link are not covered.
      return ExecuteTestCase(test_case, backend, force_two_state, timeout);
  }
  CaseExecutionResult result;
  result.execution.outcome = ExecutionOutcome::kInfraError;
  result.execution.error_message = "unknown backend";
  return result;
}

}  // namespace lyra::test
