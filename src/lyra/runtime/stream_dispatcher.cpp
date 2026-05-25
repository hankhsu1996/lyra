#include "lyra/runtime/stream_dispatcher.hpp"

#include <string_view>
#include <utility>

namespace lyra::runtime {

StreamDispatcher::StreamDispatcher(StreamSink sink) : sink_(std::move(sink)) {
}

void StreamDispatcher::Append(std::string_view text) {
  pending_.append(text);
}

void StreamDispatcher::FinishRecord(bool append_newline) {
  // FinishRecord(false) is a no-op: $write keeps content buffered.
  if (append_newline) {
    pending_.push_back('\n');
    if (sink_) {
      sink_(pending_);
    }
    pending_.clear();
  }
}

void StreamDispatcher::Drain() {
  if (!pending_.empty()) {
    if (sink_) {
      sink_(pending_);
    }
    pending_.clear();
  }
}

}  // namespace lyra::runtime
