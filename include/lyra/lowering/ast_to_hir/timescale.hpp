#pragma once

#include <cstdint>
#include <limits>
#include <optional>

#include <slang/ast/Compilation.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/numeric/Time.h>

#include "lyra/common/timescale_format.hpp"

namespace lyra::lowering::ast_to_hir {

/// Compute 10^exponent with overflow checking.
/// Returns std::nullopt on overflow or negative exponent.
inline auto IntegerPow10(int exponent) -> std::optional<uint64_t> {
  if (exponent < 0) {
    return std::nullopt;
  }
  uint64_t result = 1;
  for (int i = 0; i < exponent; ++i) {
    if (result > std::numeric_limits<uint64_t>::max() / 10) {
      return std::nullopt;
    }
    result *= 10;
  }
  return result;
}

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

// Forward declaration for mutual recursion
inline void CollectMinPrecisionFromScope(
    const slang::ast::Scope& scope, int& min_power, bool& found);

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
  CollectMinPrecisionFromScope(inst.body, min_power, found);
}

// Recursively collect min precision from a scope, walking into generate blocks.
// Keep traversal structure identical to CollectInstancesFromScope in
// design.cpp.
inline void CollectMinPrecisionFromScope(
    const slang::ast::Scope& scope, int& min_power, bool& found) {
  for (const auto& member : scope.members()) {
    if (member.kind == slang::ast::SymbolKind::Instance) {
      const auto& child = member.as<slang::ast::InstanceSymbol>();
      CollectMinPrecision(child, min_power, found);
    } else if (member.kind == slang::ast::SymbolKind::GenerateBlock) {
      const auto& block = member.as<slang::ast::GenerateBlockSymbol>();
      if (!block.isUninstantiated) {
        CollectMinPrecisionFromScope(block, min_power, found);
      }
    } else if (member.kind == slang::ast::SymbolKind::GenerateBlockArray) {
      const auto& array = member.as<slang::ast::GenerateBlockArraySymbol>();
      for (const auto* entry : array.entries) {
        if (!entry->isUninstantiated) {
          CollectMinPrecisionFromScope(*entry, min_power, found);
        }
      }
    }
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
