#pragma once

#include <span>
#include <string>
#include <vector>

namespace lyra::runtime {

// The command line the program was started with: its own name, then each
// argument as given. The simulation reads its plusargs from the arguments (LRM
// 21.6), and foreign code is handed the whole of it (LRM 38.17) as pointers it
// may keep, so the words stay where they are for the run and the invocation is
// neither copied nor moved.
class CommandLine {
 public:
  explicit CommandLine(std::vector<std::string> words);
  CommandLine(const CommandLine&) = delete;
  auto operator=(const CommandLine&) -> CommandLine& = delete;
  CommandLine(CommandLine&&) = delete;
  auto operator=(CommandLine&&) -> CommandLine& = delete;
  ~CommandLine() = default;

  // The arguments that are plusargs, the `+` that marks one removed.
  [[nodiscard]] auto Plusargs() const -> std::span<const std::string> {
    return plusargs_;
  }

  // The words as C strings, the program's name first, as `argv` is: a null
  // follows the last, as one follows `main`'s.
  [[nodiscard]] auto Words() -> std::span<char*> {
    return std::span<char*>(pointers_).first(pointers_.size() - 1);
  }

 private:
  std::vector<std::string> words_;
  std::vector<std::string> plusargs_;
  std::vector<char*> pointers_;
};

}  // namespace lyra::runtime
