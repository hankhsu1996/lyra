#include "lyra/lowering/ast_to_hir/delay_scaler.hpp"

#include <cmath>
#include <cstdint>
#include <expected>
#include <limits>
#include <string>

#include "lyra/common/internal_error.hpp"
#include "lyra/lowering/ast_to_hir/timescale.hpp"

namespace lyra::lowering::ast_to_hir {

DelayScaler::DelayScaler(int unit_power, int prec_power) {
  int exponent = unit_power - prec_power;

  if (exponent < 0) {
    throw common::InternalError(
        "delay scaler construction",
        "negative exponent - global precision coarser than timeunit");
  }

  auto ratio = IntegerPow10(exponent);
  if (!ratio) {
    throw common::InternalError("delay scaler construction", "ratio overflow");
  }

  ratio_ = *ratio;
}

auto DelayScaler::ScaleInteger(uint64_t module_units) const
    -> std::expected<uint64_t, std::string> {
  if (ratio_ == 1) {
    return module_units;
  }

  if (module_units > std::numeric_limits<uint64_t>::max() / ratio_) {
    return std::unexpected("delay value overflow");
  }

  return module_units * ratio_;
}

auto DelayScaler::ScaleReal(double module_units) const
    -> std::expected<uint64_t, std::string> {
  if (!std::isfinite(module_units)) {
    return std::unexpected("non-finite delay value");
  }

  if (module_units < 0.0) {
    return std::unexpected("negative delay value");
  }

  // Use long double for slightly better precision in the multiply
  auto ticks_f =
      static_cast<long double>(module_units) * static_cast<long double>(ratio_);

  if (!std::isfinite(ticks_f)) {
    return std::unexpected("delay value overflow");
  }

  // Round to nearest integer (ties away from zero)
  // llround returns long long, but we need int64_t for the comparison
  auto ticks_i = static_cast<int64_t>(std::llround(ticks_f));

  if (ticks_i < 0) {
    return std::unexpected("delay value overflow");
  }

  return static_cast<uint64_t>(ticks_i);
}

}  // namespace lyra::lowering::ast_to_hir
