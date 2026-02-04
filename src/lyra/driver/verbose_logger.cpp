#include "verbose_logger.hpp"

#include <chrono>
#include <ctime>
#include <iomanip>
#include <sstream>

#include <fmt/core.h>

namespace lyra::driver {

namespace {

// Format current time as HH:MM:SS
auto FormatTime() -> std::string {
  auto now = std::chrono::system_clock::now();
  auto time_t_now = std::chrono::system_clock::to_time_t(now);
  std::tm tm_buf{};
  localtime_r(&time_t_now, &tm_buf);
  std::ostringstream oss;
  oss << std::put_time(&tm_buf, "%H:%M:%S");
  return oss.str();
}

}  // namespace

void VerboseLogger::PhaseBegin(std::string_view phase_name) {
  if (!Enabled(1)) return;
  fmt::print(sink_, "[lyra][{}][phase] {}: begin\n", FormatTime(), phase_name);
  std::fflush(sink_);
}

void VerboseLogger::PhaseDone(std::string_view phase_name, double seconds) {
  if (!Enabled(1)) return;
  fmt::print(
      sink_, "[lyra][{}][phase] {}: done ({:.2f}s)\n", FormatTime(), phase_name,
      seconds);
  std::fflush(sink_);
}

void VerboseLogger::Progress(
    std::string_view phase_name, double elapsed_seconds) {
  if (!Enabled(1)) return;
  fmt::print(
      sink_, "[lyra][{}][progress] {}: still running ({:.0f}s)...\n",
      FormatTime(), phase_name, elapsed_seconds);
  std::fflush(sink_);
}

PhaseTimer::PhaseTimer(
    VerboseLogger& logger, std::string phase_name, bool enable_heartbeat)
    : logger_(logger),
      phase_name_(std::move(phase_name)),
      start_(std::chrono::steady_clock::now()),
      enabled_(logger.Enabled(1)),
      heartbeat_enabled_(enable_heartbeat && enabled_) {
  if (enabled_) {
    logger_.PhaseBegin(phase_name_);
  }
  if (heartbeat_enabled_) {
    heartbeat_thread_ = std::jthread(
        [this](std::stop_token st) { HeartbeatLoop(std::move(st)); });
  }
}

PhaseTimer::~PhaseTimer() {
  // Stop heartbeat thread first (jthread destructor will request_stop and
  // join, but we need to signal the condition variable)
  if (heartbeat_enabled_) {
    heartbeat_thread_.request_stop();
    heartbeat_cv_.notify_all();
    // jthread destructor will join
  }

  auto end = std::chrono::steady_clock::now();
  auto duration =
      std::chrono::duration_cast<std::chrono::milliseconds>(end - start_);
  double seconds = duration.count() / 1000.0;

  // ALWAYS record duration (for --stats), regardless of verbosity
  logger_.RecordPhaseDuration(phase_name_, seconds);

  // Only print human-readable logs if verbose enabled
  if (enabled_) {
    logger_.PhaseDone(phase_name_, seconds);
  }
}

void VerboseLogger::RecordPhaseDuration(std::string_view name, double seconds) {
  phase_durations_[std::string(name)] = seconds;
}

void VerboseLogger::PrintPhaseSummary(FILE* sink) const {
  std::string line = "[lyra][stats][phase]";
  for (std::string_view phase : kPhaseOrder) {
    auto it = phase_durations_.find(std::string(phase));
    if (it != phase_durations_.end()) {
      line += fmt::format(" {}={:.2f}s", phase, it->second);
    }
  }
  fmt::print(sink, "{}\n", line);
  std::fflush(sink);
}

void PhaseTimer::HeartbeatLoop(std::stop_token stop_token) {
  constexpr auto kThreshold = std::chrono::seconds(10);
  constexpr auto kInterval = std::chrono::seconds(10);

  // Wait for initial threshold
  {
    std::unique_lock lock(heartbeat_mutex_);
    if (heartbeat_cv_.wait_for(lock, stop_token, kThreshold, [&] {
          return stop_token.stop_requested();
        })) {
      return;  // Stopped before threshold
    }
  }

  // Emit heartbeats at regular intervals
  while (!stop_token.stop_requested()) {
    auto elapsed = std::chrono::steady_clock::now() - start_;
    auto elapsed_secs =
        std::chrono::duration_cast<std::chrono::seconds>(elapsed).count();
    logger_.Progress(phase_name_, static_cast<double>(elapsed_secs));

    std::unique_lock lock(heartbeat_mutex_);
    if (heartbeat_cv_.wait_for(lock, stop_token, kInterval, [&] {
          return stop_token.stop_requested();
        })) {
      return;  // Stopped during interval wait
    }
  }
}

}  // namespace lyra::driver
