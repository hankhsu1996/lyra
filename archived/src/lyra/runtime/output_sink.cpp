#include "lyra/runtime/output_sink.hpp"

#include <cstdio>
#include <memory>
#include <string_view>

#include <fmt/core.h>

namespace lyra::runtime {

void OutputDispatcher::WriteTo(std::string_view text) {
  if (sink_) {
    sink_(text);
    return;
  }
  fmt::print("{}", text);
}

void OutputDispatcher::AppendSimOutputFragment(std::string_view text) {
  if (sink_) {
    sink_(text);
    return;
  }
  pending_sim_output_.append(text);
}

void OutputDispatcher::FinishSimOutputRecord() {
  if (sink_) {
    sink_("\n");
    return;
  }
  pending_sim_output_.push_back('\n');
  WriteTo(pending_sim_output_);
  pending_sim_output_.clear();
}

void OutputDispatcher::DrainSimOutputBuffer() {
  if (sink_) {
    return;
  }
  if (pending_sim_output_.empty()) {
    return;
  }
  WriteTo(pending_sim_output_);
  pending_sim_output_.clear();
}

void OutputDispatcher::WriteProtocolRecord(std::string_view text) {
  WriteTo(text);
}

extern "C" auto LyraCreateRunSession() -> void* {
  return std::make_unique<RunSession>().release();
}

extern "C" void LyraDestroyRunSession(void* ptr) {
  std::unique_ptr<RunSession>(static_cast<RunSession*>(ptr));
}

}  // namespace lyra::runtime
