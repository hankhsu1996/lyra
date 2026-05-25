#pragma once

#include "lyra/base/internal_error.hpp"

namespace lyra::runtime {

class StreamDispatcher;
class DiagnosticDispatcher;

class RuntimeServices {
 public:
  RuntimeServices(StreamDispatcher& stream, DiagnosticDispatcher& diagnostic)
      : stream_(&stream), diagnostic_(&diagnostic) {
  }

  auto Stream() -> StreamDispatcher& {
    if (stream_ == nullptr) {
      throw InternalError("RuntimeServices has no StreamDispatcher");
    }
    return *stream_;
  }

  auto Diagnostic() -> DiagnosticDispatcher& {
    if (diagnostic_ == nullptr) {
      throw InternalError("RuntimeServices has no DiagnosticDispatcher");
    }
    return *diagnostic_;
  }

 private:
  StreamDispatcher* stream_ = nullptr;
  DiagnosticDispatcher* diagnostic_ = nullptr;
};

}  // namespace lyra::runtime
