#include "lyra/runtime/timescale.hpp"

#include <array>
#include <format>
#include <string>

#include "lyra/runtime/runtime_services.hpp"
#include "lyra/runtime/stream_dispatcher.hpp"

namespace lyra::runtime {

namespace {

// LRM Table 20-2: a power value maps to one of {1, 10, 100} times a unit that
// cycles s / ms / us / ns / ps / fs every three decades below 1 s.
auto TimeUnitText(std::int8_t power) -> std::string {
  constexpr std::array<std::string_view, 6> kUnits = {"s",  "ms", "us",
                                                      "ns", "ps", "fs"};
  constexpr std::array<std::string_view, 3> kMantissa = {"1", "10", "100"};
  const int p = static_cast<int>(power);
  const int index = (-p + 2) / 3;
  const int mantissa_exp = p + (3 * index);
  const std::string_view unit = (index >= 0 && index < 6)
                                    ? kUnits.at(static_cast<std::size_t>(index))
                                    : "?";
  const std::string_view mantissa =
      (mantissa_exp >= 0 && mantissa_exp < 3)
          ? kMantissa.at(static_cast<std::size_t>(mantissa_exp))
          : "?";
  return std::string(mantissa) + std::string(unit);
}

}  // namespace

void LyraPrintTimescale(
    RuntimeServices& services, std::string_view scope_name,
    std::int8_t unit_power, std::int8_t precision_power) {
  auto& stream = services.Stream();
  stream.Append(
      std::format(
          "Time scale of ({}) is {} / {}", scope_name, TimeUnitText(unit_power),
          TimeUnitText(precision_power)));
  stream.FinishRecord(true);
}

}  // namespace lyra::runtime
