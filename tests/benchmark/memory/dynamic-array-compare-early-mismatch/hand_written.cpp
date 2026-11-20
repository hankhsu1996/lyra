// The work of main.sv, as a C++ programmer would write it over a dynamic
// array that keeps the language's guarantees.
#include <cstddef>
#include <cstdint>
#include <format>
#include <span>

#include "../../hand_written_containers.hpp"

auto main(int argc, char** argv) -> int {
  constexpr std::int32_t kArraySize = 4096;
  const std::int32_t num_passes =
      hand_written::WorkFrom(std::span(argv, static_cast<std::size_t>(argc)));

  hand_written::DynamicArray<std::int32_t> left;
  hand_written::DynamicArray<std::int32_t> right;
  left.New(kArraySize);
  right.New(kArraySize);
  for (std::int32_t i = 0; i < kArraySize; ++i) {
    left.Write(i, i);
    right.Write(i, i);
  }

  std::int32_t checksum = 0;
  for (std::int32_t pass = 0; pass < num_passes; ++pass) {
    left.Write(0, hand_written::Add(pass, 1));
    if (!(left == right)) checksum = hand_written::Add(checksum, 1);
  }

  hand_written::Print(
      std::format(
          "dynamic-array-compare-early-mismatch done: checksum={}", checksum));
  return 0;
}
