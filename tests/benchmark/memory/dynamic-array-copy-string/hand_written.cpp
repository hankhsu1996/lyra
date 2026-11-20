// The work of main.sv, as a C++ programmer would write it over a dynamic
// array that keeps the language's guarantees.
#include <cstddef>
#include <cstdint>
#include <format>
#include <span>
#include <string>

#include "../../hand_written_containers.hpp"

auto main(int argc, char** argv) -> int {
  constexpr std::int32_t kArraySize = 4096;
  const std::int32_t num_passes =
      hand_written::WorkFrom(std::span(argv, static_cast<std::size_t>(argc)));

  hand_written::DynamicArray<std::string> source;
  hand_written::DynamicArray<std::string> copy;
  source.New(kArraySize);
  copy.New(kArraySize);
  for (std::int32_t i = 0; i < kArraySize; ++i) {
    source.Write(i, std::format("element number {} of the array", i));
  }

  std::int32_t checksum = 0;
  for (std::int32_t pass = 0; pass < num_passes; ++pass) {
    source.Write(pass % kArraySize, source.Read((pass + 1) % kArraySize));
    copy = source;
    if (copy == source) {
      checksum = hand_written::Add(
          checksum,
          static_cast<std::int32_t>(copy.Read(pass % kArraySize).size()));
    }
  }

  hand_written::Print(
      std::format("dynamic-array-copy-string done: checksum={}", checksum));
  return 0;
}
