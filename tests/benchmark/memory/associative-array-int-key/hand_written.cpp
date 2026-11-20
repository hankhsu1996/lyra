// The work of main.sv, as a C++ programmer would write it over an associative
// array that keeps the language's guarantees.
#include <cstddef>
#include <cstdint>
#include <format>
#include <span>

#include "../../hand_written_containers.hpp"

auto main(int argc, char** argv) -> int {
  constexpr std::int32_t kEntries = 1024;
  constexpr std::int32_t kStride = 7919;
  const std::int32_t num_passes =
      hand_written::WorkFrom(std::span(argv, static_cast<std::size_t>(argc)));

  const auto index_of = [](std::int32_t i) {
    return (i * kStride) % (kEntries * 8);
  };

  std::int32_t checksum = 0;
  hand_written::AssociativeArray<std::int32_t, std::int32_t> a;
  for (std::int32_t pass = 0; pass < num_passes; ++pass) {
    for (std::int32_t i = 0; i < kEntries; ++i) {
      a.Write(index_of(i), hand_written::Add(i, pass));
    }
    for (std::int32_t i = 0; i < kEntries; ++i) {
      if (a.Exists(index_of(i))) {
        checksum = hand_written::Add(checksum, a.Read(index_of(i)));
      }
    }
    std::int32_t index = 0;
    for (bool more = a.First(index); more; more = a.Next(index)) {
      checksum = hand_written::Add(checksum, index);
    }
    for (std::int32_t i = 0; i < kEntries; ++i) a.Delete(index_of(i));
  }

  hand_written::Print(
      std::format("associative-array-int-key done: checksum={}", checksum));
  return 0;
}
