#include "lyra/runtime/output_sink.hpp"

#include <functional>
#include <string_view>

#include <fmt/core.h>

namespace lyra::runtime {

namespace {

// NOLINTBEGIN(cppcoreguidelines-avoid-non-const-global-variables)
OutputSink g_output_sink;
// NOLINTEND(cppcoreguidelines-avoid-non-const-global-variables)

}  // namespace

void SetOutputSink(OutputSink sink) {
  g_output_sink = std::move(sink);
}

auto GetOutputSink() -> OutputSink {
  return g_output_sink;
}

void WriteOutput(std::string_view text) {
  if (g_output_sink) {
    g_output_sink(text);
  } else {
    fmt::print("{}", text);
  }
}

}  // namespace lyra::runtime
