#include "lyra/runtime/output_sink.hpp"

#include <string_view>
#include <utility>

namespace lyra::runtime {

OutputDispatcher::OutputDispatcher(OutputSink sink) : sink_(std::move(sink)) {
}

void OutputDispatcher::Append(std::string_view text) {
  pending_.append(text);
}

void OutputDispatcher::FinishRecord(bool append_newline) {
  // FinishRecord(false) is a no-op: $write keeps content buffered.
  if (append_newline) {
    pending_.push_back('\n');
    if (sink_) {
      sink_(pending_);
    }
    pending_.clear();
  }
}

void OutputDispatcher::Drain() {
  if (!pending_.empty()) {
    if (sink_) {
      sink_(pending_);
    }
    pending_.clear();
  }
}

}  // namespace lyra::runtime
