#pragma once

#include <cstdint>
#include <expected>
#include <string>

namespace lyra::lowering::ast_to_hir {

/// Scales delay values from module timeunit to global precision ticks.
///
/// The ratio = 10^(unit_power - prec_power) is always an integer because
/// LRM requires precision <= unit (i.e., prec_power <= unit_power).
class DelayScaler {
 public:
  /// Construct a scaler from unit and precision powers.
  /// unit_power: scope's timeunit as power of 10 (e.g., -8 for 10ns)
  /// prec_power: global precision as power of 10 (e.g., -12 for 1ps)
  /// Throws InternalError if ratio computation fails (precision > unit).
  DelayScaler(int unit_power, int prec_power);

  /// Scale an integer literal (exact path, no rounding).
  /// Returns ticks or error message on overflow.
  [[nodiscard]] auto ScaleInteger(uint64_t module_units) const
      -> std::expected<uint64_t, std::string>;

  /// Scale a time literal value (floating path, with rounding).
  /// The value is already in module timeunits (converted by slang).
  /// Returns ticks or error message on overflow/invalid value.
  [[nodiscard]] auto ScaleReal(double module_units) const
      -> std::expected<uint64_t, std::string>;

  /// Get the scaling ratio (for diagnostics).
  [[nodiscard]] auto Ratio() const -> uint64_t {
    return ratio_;
  }

 private:
  uint64_t ratio_;  // 10^(unit_power - prec_power)
};

}  // namespace lyra::lowering::ast_to_hir
