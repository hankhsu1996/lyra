#pragma once

#include <slang/ast/Compilation.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/numeric/Time.h>

#include "lyra/common/timescale_format.hpp"

namespace lyra::lowering::ast_to_hir {

/// Convert a slang TimeScaleValue to a power-of-10 integer.
inline auto TimeScaleValueToPower(const slang::TimeScaleValue& tsv) -> int {
  int base = 0;
  switch (tsv.unit) {
    case slang::TimeUnit::Seconds:
      base = 0;
      break;
    case slang::TimeUnit::Milliseconds:
      base = -3;
      break;
    case slang::TimeUnit::Microseconds:
      base = -6;
      break;
    case slang::TimeUnit::Nanoseconds:
      base = -9;
      break;
    case slang::TimeUnit::Picoseconds:
      base = -12;
      break;
    case slang::TimeUnit::Femtoseconds:
      base = -15;
      break;
  }

  int magnitude_offset = 0;
  switch (tsv.magnitude) {
    case slang::TimeScaleMagnitude::One:
      magnitude_offset = 0;
      break;
    case slang::TimeScaleMagnitude::Ten:
      magnitude_offset = 1;
      break;
    case slang::TimeScaleMagnitude::Hundred:
      magnitude_offset = 2;
      break;
  }

  return base + magnitude_offset;
}

namespace detail {

inline void CollectMinPrecision(
    const slang::ast::InstanceSymbol& inst, int& min_power, bool& found) {
  auto ts = inst.body.getTimeScale();
  if (ts) {
    int prec = TimeScaleValueToPower(ts->precision);
    if (!found || prec < min_power) {
      min_power = prec;
      found = true;
    }
  }
  for (const auto& child :
       inst.body.membersOfType<slang::ast::InstanceSymbol>()) {
    CollectMinPrecision(child, min_power, found);
  }
}

}  // namespace detail

/// Compute the global precision (finest timeprecision across all instances).
inline auto ComputeGlobalPrecision(slang::ast::Compilation& comp) -> int {
  int min_power = 0;
  bool found = false;
  for (const auto* inst : comp.getRoot().topInstances) {
    detail::CollectMinPrecision(*inst, min_power, found);
  }
  return found ? min_power : kDefaultTimeScalePower;
}

}  // namespace lyra::lowering::ast_to_hir
