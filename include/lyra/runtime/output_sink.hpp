#pragma once

#include <functional>
#include <string_view>
#include <utility>

namespace lyra::runtime {

// Output sink callback type: receives text to be output.
using OutputSink = std::function<void(std::string_view)>;

// Set global output sink. Pass nullptr to restore default (stdout).
// Not thread-safe: set before starting simulation, restore after.
void SetOutputSink(OutputSink sink);

// Get current output sink (may be empty if default stdout is active).
auto GetOutputSink() -> OutputSink;

// Write to current output sink (stdout if no sink set).
void WriteOutput(std::string_view text);

// RAII guard for output sink. Restores previous sink on destruction.
// Use this instead of raw SetOutputSink() to ensure cleanup on early returns.
class OutputSinkScope {
 public:
  explicit OutputSinkScope(OutputSink sink) : prev_(GetOutputSink()) {
    SetOutputSink(std::move(sink));
  }
  ~OutputSinkScope() {
    SetOutputSink(std::move(prev_));
  }

  OutputSinkScope(const OutputSinkScope&) = delete;
  OutputSinkScope(OutputSinkScope&&) = delete;
  auto operator=(const OutputSinkScope&) -> OutputSinkScope& = delete;
  auto operator=(OutputSinkScope&&) -> OutputSinkScope& = delete;

 private:
  OutputSink prev_;
};

}  // namespace lyra::runtime
