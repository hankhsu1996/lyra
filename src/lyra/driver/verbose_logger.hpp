#pragma once

#include <array>
#include <chrono>
#include <condition_variable>
#include <cstdio>
#include <mutex>
#include <string>
#include <string_view>
#include <thread>
#include <unordered_map>

namespace lyra::driver {

// Central logger for verbose output during compilation.
// All output goes to stderr to preserve stdout for program results.
class VerboseLogger {
 public:
  explicit VerboseLogger(int level, FILE* sink = stderr)
      : level_(level), sink_(sink) {
  }

  // Check if logging at the given level is enabled.
  auto Enabled(int required_level) const -> bool {
    return level_ >= required_level;
  }

  // Log a phase begin event (level 1).
  void PhaseBegin(std::string_view phase_name);

  // Log a phase done event with duration (level 1).
  void PhaseDone(std::string_view phase_name, double seconds);

  // Log a progress heartbeat (level 1).
  void Progress(std::string_view phase_name, double elapsed_seconds);

  // Record phase duration (always, regardless of verbosity level).
  // Called by PhaseTimer destructor.
  void RecordPhaseDuration(std::string_view name, double seconds);

  // Print phase summary line for --stats output.
  void PrintPhaseSummary(FILE* sink = stderr) const;

  auto level() const -> int {
    return level_;
  }

 private:
  // Fixed phase order for deterministic output.
  static constexpr std::array<std::string_view, 6> kPhaseOrder = {
      "parse",     "elaborate",  "lower_hir",
      "lower_mir", "lower_llvm", "jit_compile"};

  int level_;
  FILE* sink_;
  std::unordered_map<std::string, double> phase_durations_;
};

// RAII helper for timing phases. Logs begin on construction, done on
// destruction.
class PhaseTimer {
 public:
  PhaseTimer(
      VerboseLogger& logger, std::string phase_name,
      bool enable_heartbeat = false);
  ~PhaseTimer();

  // Non-copyable, non-movable (RAII resource)
  PhaseTimer(const PhaseTimer&) = delete;
  PhaseTimer& operator=(const PhaseTimer&) = delete;
  PhaseTimer(PhaseTimer&&) = delete;
  PhaseTimer& operator=(PhaseTimer&&) = delete;

 private:
  void HeartbeatLoop(std::stop_token stop_token);

  VerboseLogger& logger_;
  std::string phase_name_;
  std::chrono::steady_clock::time_point start_;
  bool enabled_;

  // Heartbeat thread state
  bool heartbeat_enabled_;
  std::jthread heartbeat_thread_;
  std::mutex heartbeat_mutex_;
  std::condition_variable_any heartbeat_cv_;
};

}  // namespace lyra::driver
