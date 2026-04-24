#pragma once

#include "lyra/trace/trace_event.hpp"

namespace lyra::trace {

// Streaming consumer of trace events.
//
// Ownership contract: OnEvent receives an ephemeral event reference that is
// only valid for the duration of the call. Sinks must copy any data they
// need to retain (e.g. TraceValue contents). The caller may move or destroy
// the event immediately after OnEvent returns.
class TraceSink {
 public:
  virtual ~TraceSink() = default;
  virtual void OnEvent(const TraceEvent& event) = 0;
};

}  // namespace lyra::trace
