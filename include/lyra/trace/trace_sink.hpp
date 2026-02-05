#pragma once

#include "lyra/trace/trace_event.hpp"

namespace lyra::trace {

class TraceSink {
 public:
  virtual ~TraceSink() = default;
  virtual void OnEvent(const TraceEvent& event) = 0;
};

}  // namespace lyra::trace
