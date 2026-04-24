#pragma once

#include <functional>
#include <string>
#include <string_view>
#include <utility>

namespace lyra::runtime {

// Output sink callback type: receives text to be output.
using OutputSink = std::function<void(std::string_view)>;

// Owns simulation output state: sink callback and pending fragment buffer.
// One instance per run session, borrowed by Engine during simulation.
class OutputDispatcher {
 public:
  // Append one fragment of a user-facing simulation record.
  void AppendSimOutputFragment(std::string_view text);

  // Complete one user-facing simulation record (append newline, write, clear).
  void FinishSimOutputRecord();

  // Drain pending $write fragments without newline. Call before protocol
  // emission or termination to preserve stream ordering.
  void DrainSimOutputBuffer();

  // Emit one complete machine-readable protocol/data record.
  void WriteProtocolRecord(std::string_view text);

  // Install or clear the output sink.
  void SetSink(OutputSink sink) {
    sink_ = std::move(sink);
  }

  // Get the current output sink (may be empty).
  [[nodiscard]] auto GetSink() const -> const OutputSink& {
    return sink_;
  }

 private:
  OutputSink sink_;
  std::string pending_sim_output_;

  // Write text to the current sink, or stdout if no sink is set.
  void WriteTo(std::string_view text);
};

// Run-lifetime session. Owns services that must survive through
// simulation + epilogue. Currently: output dispatch only.
struct RunSession {
  OutputDispatcher output;
};

// RAII guard for output sink on a specific OutputDispatcher.
// Installs the sink on construction, restores the previous sink on destruction.
class OutputSinkScope {
 public:
  OutputSinkScope(OutputDispatcher& target, OutputSink sink)
      : target_(target), prev_(target.GetSink()) {
    target_.SetSink(std::move(sink));
  }
  ~OutputSinkScope() {
    target_.SetSink(std::move(prev_));
  }

  OutputSinkScope(const OutputSinkScope&) = delete;
  OutputSinkScope(OutputSinkScope&&) = delete;
  auto operator=(const OutputSinkScope&) -> OutputSinkScope& = delete;
  auto operator=(OutputSinkScope&&) -> OutputSinkScope& = delete;

 private:
  OutputDispatcher& target_;
  OutputSink prev_;
};

}  // namespace lyra::runtime
