// The work of main.sv, as a C++ programmer would write it over a dynamic
// array that keeps the language's guarantees.
#include <cstddef>
#include <cstdint>
#include <format>
#include <span>

#include "../../hand_written_containers.hpp"

auto main(int argc, char** argv) -> int {
  constexpr std::int32_t kArraySize = 1024;
  constexpr std::int32_t kKept = kArraySize / 4;
  const std::int32_t num_passes =
      hand_written::WorkFrom(std::span(argv, static_cast<std::size_t>(argc)));

  std::int32_t checksum = 0;
  hand_written::DynamicArray<std::int32_t> a;
  for (std::int32_t pass = 0; pass < num_passes; ++pass) {
    a.New(kKept);
    a.Write(pass % kKept, pass);
    a.New(kArraySize, a);
    checksum = hand_written::Add(
        hand_written::Add(checksum, a.Read(pass % kKept)), a.Size());
  }

  hand_written::Print(
      std::format("dynamic-array-resize done: checksum={}", checksum));
  return 0;
}
