#include "lyra/profiling/time_trace.hpp"

#include <atomic>
#include <expected>
#include <filesystem>
#include <format>
#include <string>
#include <string_view>
#include <system_error>

#include <llvm/Support/FileSystem.h>
#include <llvm/Support/TimeProfiler.h>
#include <llvm/Support/raw_ostream.h>

namespace lyra::profiling {

namespace {

constexpr std::string_view kProcessName = "lyra";

struct Trace {
  std::atomic<bool> started = false;
  std::atomic<unsigned> granularity = 0;
};

auto TheTrace() -> Trace& {
  static Trace trace;
  return trace;
}

// LLVM's profiler is one per thread, and a thread's records reach the trace
// only once the thread hands them over. A thread the run enrolled hands them
// over as it exits, which is before whoever waited for it goes on to write.
struct EnrolledThread {
  bool enrolled = false;

  EnrolledThread() = default;
  EnrolledThread(const EnrolledThread&) = delete;
  auto operator=(const EnrolledThread&) -> EnrolledThread& = delete;
  EnrolledThread(EnrolledThread&&) = delete;
  auto operator=(EnrolledThread&&) -> EnrolledThread& = delete;

  ~EnrolledThread() {
    if (enrolled) {
      llvm::timeTraceProfilerFinishThread();
    }
  }
};

void EnrollThisThread() {
  if (llvm::getTimeTraceProfilerInstance() != nullptr) {
    return;
  }
  thread_local EnrolledThread enrolled;
  llvm::timeTraceProfilerInitialize(TheTrace().granularity, kProcessName);
  enrolled.enrolled = true;
}

}  // namespace

void TimeTraceStart(unsigned granularity_us) {
  TheTrace().granularity = granularity_us;
  llvm::timeTraceProfilerInitialize(granularity_us, kProcessName);
  TheTrace().started = true;
}

auto TimeTraceStarted() -> bool {
  return TheTrace().started;
}

auto TimeTraceWrite(const std::filesystem::path& path)
    -> std::expected<void, std::string> {
  std::error_code ec;
  llvm::raw_fd_ostream out(path.string(), ec, llvm::sys::fs::OF_Text);
  if (ec) {
    return std::unexpected(
        std::format(
            "failed to write the time trace to '{}': {}", path.string(),
            ec.message()));
  }
  llvm::timeTraceProfilerWrite(out);
  llvm::timeTraceProfilerCleanup();
  return {};
}

TimeTraceScope::TimeTraceScope(std::string_view name) {
  if (TimeTraceStarted()) {
    Begin(name, std::string{});
  }
}

TimeTraceScope::~TimeTraceScope() {
  if (open_) {
    llvm::timeTraceProfilerEnd();
  }
}

void TimeTraceScope::Begin(std::string_view name, std::string detail) {
  EnrollThisThread();
  llvm::timeTraceProfilerBegin(name, detail);
  open_ = true;
}

}  // namespace lyra::profiling
