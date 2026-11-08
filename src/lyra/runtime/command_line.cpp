#include "lyra/runtime/command_line.hpp"

#include <string>
#include <utility>
#include <vector>

#include "lyra/runtime/plusargs.hpp"

namespace lyra::runtime {

CommandLine::CommandLine(std::vector<std::string> words)
    : words_(std::move(words)), plusargs_(PlusargsFrom(words_)) {
  pointers_.reserve(words_.size() + 1);
  for (std::string& word : words_) {
    pointers_.push_back(word.data());
  }
  pointers_.push_back(nullptr);
}

}  // namespace lyra::runtime
