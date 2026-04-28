#pragma once

#include <functional>
#include <string>
#include <string_view>

namespace lyra::runtime {

class OutputDispatcher {
 public:
  using OutputSink = std::function<void(std::string_view)>;

  explicit OutputDispatcher(OutputSink sink);

  void Append(std::string_view text);
  void FinishRecord(bool append_newline);
  void Drain();

 private:
  std::string pending_;
  OutputSink sink_;
};

}  // namespace lyra::runtime
