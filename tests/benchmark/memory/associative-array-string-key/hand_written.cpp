// The work of main.sv, as a C++ programmer would write it over an associative
// array that keeps the language's guarantees.
#include <array>
#include <cstddef>
#include <cstdint>
#include <format>
#include <span>
#include <string>

#include "../../hand_written_containers.hpp"

auto main(int argc, char** argv) -> int {
  constexpr std::int32_t kEntries = 1024;
  constexpr std::int32_t kStride = 7919;
  const std::int32_t num_passes =
      hand_written::WorkFrom(std::span(argv, static_cast<std::size_t>(argc)));

  std::array<std::string, kEntries> keys;
  for (std::int32_t i = 0; i < kEntries; ++i) {
    keys.at(static_cast<std::size_t>(i)) =
        std::format("key {}", (i * kStride) % (kEntries * 8));
  }

  std::int32_t checksum = 0;
  hand_written::AssociativeArray<std::string, std::int32_t> a;
  for (std::int32_t pass = 0; pass < num_passes; ++pass) {
    for (std::int32_t i = 0; i < kEntries; ++i) {
      a.Write(keys.at(static_cast<std::size_t>(i)), hand_written::Add(i, pass));
    }
    for (const std::string& key : keys) {
      if (a.Exists(key)) checksum = hand_written::Add(checksum, a.Read(key));
    }
    std::string index;
    for (bool more = a.First(index); more; more = a.Next(index)) {
      checksum =
          hand_written::Add(checksum, static_cast<std::int32_t>(index.size()));
    }
    for (const std::string& key : keys) a.Delete(key);
  }

  hand_written::Print(
      std::format("associative-array-string-key done: checksum={}", checksum));
  return 0;
}
