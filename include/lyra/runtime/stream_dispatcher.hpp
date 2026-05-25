#pragma once

#include <functional>
#include <string>
#include <string_view>

namespace lyra::runtime {

class StreamDispatcher {
 public:
  using StreamSink = std::function<void(std::string_view)>;

  explicit StreamDispatcher(StreamSink sink);

  void Append(std::string_view text);
  void FinishRecord(bool append_newline);
  void Drain();

 private:
  std::string pending_;
  StreamSink sink_;
};

}  // namespace lyra::runtime
