#pragma once

#include <cstdio>
#include <memory>
#include <string_view>

namespace lyra::driver {

class TextSink {
 public:
  virtual ~TextSink() = default;
  virtual void Write(std::string_view text) = 0;
  virtual void Flush() = 0;

  TextSink() = default;
  TextSink(const TextSink&) = delete;
  auto operator=(const TextSink&) -> TextSink& = delete;
  TextSink(TextSink&&) = delete;
  auto operator=(TextSink&&) -> TextSink& = delete;
};

auto CreateStderrSink() -> std::unique_ptr<TextSink>;

}  // namespace lyra::driver
