#include "text_sink.hpp"

#include <cstdio>
#include <string_view>

namespace lyra::driver {

namespace {

class FileTextSink final : public TextSink {
 public:
  explicit FileTextSink(FILE* file) : file_(file) {
  }

  void Write(std::string_view text) override {
    std::fwrite(text.data(), 1, text.size(), file_);
  }

  void Flush() override {
    std::fflush(file_);
  }

 private:
  FILE* file_;
};

}  // namespace

auto CreateStderrSink() -> std::unique_ptr<TextSink> {
  return std::make_unique<FileTextSink>(stderr);
}

}  // namespace lyra::driver
