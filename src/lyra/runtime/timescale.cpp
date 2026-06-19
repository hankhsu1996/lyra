#include "lyra/runtime/timescale.hpp"

#include <array>
#include <cstdint>
#include <format>
#include <string>

#include "lyra/runtime/runtime_services.hpp"
#include "lyra/runtime/stream_dispatcher.hpp"
#include "lyra/value/format.hpp"

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
    RuntimeServices& services, const value::String& scope_name,
    const value::PackedArray& unit_power,
    const value::PackedArray& precision_power) {
  auto& stream = services.Stream();
  stream.Append(
      std::format(
          "Time scale of ({}) is {} / {}", scope_name.View(),
          TimeUnitText(static_cast<std::int8_t>(unit_power.ToInt64())),
          TimeUnitText(static_cast<std::int8_t>(precision_power.ToInt64()))));
  stream.FinishRecord(true);
}

void LyraTimeFormat(
    RuntimeServices& services, const value::PackedArray& units_power,
    const value::PackedArray& precision, const value::String& suffix,
    const value::PackedArray& min_width) {
  services.SetTimeFormat(
      value::TimeFormat{
          .units_power = static_cast<std::int8_t>(units_power.ToInt64()),
          .precision = static_cast<std::int32_t>(precision.ToInt64()),
          .suffix = std::string(suffix.View()),
          .min_width = static_cast<std::int32_t>(min_width.ToInt64())});
}

void LyraTimeFormat(RuntimeServices& services) {
  value::TimeFormat time_format;
  time_format.units_power = services.GlobalPrecisionPower();
  services.SetTimeFormat(time_format);
}

}  // namespace lyra::runtime
