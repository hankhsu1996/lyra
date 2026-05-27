#pragma once

#include <cstdint>
#include <functional>

#include "lyra/base/internal_error.hpp"

namespace lyra::runtime {

class StreamDispatcher;
class DiagnosticDispatcher;
class Engine;
class Observable;
enum class EdgeTransition : std::uint8_t;

class RuntimeServices {
 public:
  RuntimeServices(
      StreamDispatcher& stream, DiagnosticDispatcher& diagnostic,
      Engine& engine)
      : stream_(&stream), diagnostic_(&diagnostic), engine_(&engine) {
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

  void SubmitNba(std::function<void()> closure);

  void TriggerValueChange(Observable& observable, EdgeTransition transition);

 private:
  StreamDispatcher* stream_ = nullptr;
  DiagnosticDispatcher* diagnostic_ = nullptr;
  Engine* engine_ = nullptr;
};

}  // namespace lyra::runtime
