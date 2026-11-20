// The work of main.sv, as a C++ programmer would write it over a queue that
// keeps the language's guarantees.
#include <array>
#include <cstddef>
#include <cstdint>
#include <format>
#include <span>
#include <string>

#include "../../hand_written_containers.hpp"

auto main(int argc, char** argv) -> int {
  constexpr std::int32_t kQueueSize = 1024;
  const std::int32_t num_passes =
      hand_written::WorkFrom(std::span(argv, static_cast<std::size_t>(argc)));

  std::array<std::string, kQueueSize> words;
  for (std::size_t i = 0; i < words.size(); ++i) {
    words.at(i) = std::format("element number {} of the queue", i);
  }

  std::int32_t checksum = 0;
  hand_written::Queue<std::string> q;
  hand_written::Queue<std::string> copy;
  for (std::int32_t pass = 0; pass < num_passes; ++pass) {
    for (std::int32_t i = 0; i < kQueueSize; ++i) {
      q.PushBack(words.at(static_cast<std::size_t>((i + pass) % kQueueSize)));
    }
    copy = q;
    while (copy.Size() > 0) {
      const std::string word = copy.PopFront();
      // `word[15]` is a byte, zero where the string is shorter (LRM 6.16).
      const std::int32_t byte =
          word.size() > 15 ? static_cast<unsigned char>(word[15]) : 0;
      checksum = hand_written::Add(
          checksum,
          hand_written::Add(static_cast<std::int32_t>(word.size()), byte));
    }
    q.Clear();
  }

  hand_written::Print(
      std::format("queue-push-pop-string done: checksum={}", checksum));
  return 0;
}
