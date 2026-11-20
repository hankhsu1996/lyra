// The work of main.sv, as a C++ programmer would write it over a queue that
// keeps the language's guarantees.
#include <cstddef>
#include <cstdint>
#include <format>
#include <span>

#include "../../hand_written_containers.hpp"

auto main(int argc, char** argv) -> int {
  constexpr std::int32_t kQueueSize = 1024;
  const std::int32_t num_passes =
      hand_written::WorkFrom(std::span(argv, static_cast<std::size_t>(argc)));

  hand_written::Queue<std::int32_t> q;
  for (std::int32_t i = 0; i < kQueueSize; ++i) q.PushBack(i);

  std::int32_t checksum = 0;
  for (std::int32_t pass = 0; pass < num_passes; ++pass) {
    q.Insert(kQueueSize / 2, pass);
    checksum = hand_written::Add(
        checksum, q.Read((pass + (kQueueSize / 4)) % kQueueSize));
    q.Delete(pass % kQueueSize);
  }

  hand_written::Print(
      std::format("queue-insert-delete done: checksum={}", checksum));
  return 0;
}
