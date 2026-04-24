#pragma once

#include <algorithm>
#include <cstdint>
#include <limits>
#include <optional>

#include <slang/ast/Compilation.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/numeric/Time.h>

namespace lyra::lowering::ast_to_hir {

/// Resolved timescale for a scope: unit and precision as powers of 10.
/// Uses int for computation ergonomics. Downstream storage and ABI carriers
/// narrow to int8_t (valid range [-15, 0]) at explicit storage boundaries.
struct ResolvedTimeScale {
  int unit_power;
  int precision_power;
};

/// Canonical resolver for an optional slang TimeScale.
/// Accepts the value returned by Scope::getTimeScale() or similar APIs.
/// If the optional has a value, converts its base/precision to powers.
/// If nullopt (no timescale in scope), maps to Lyra's canonical
/// missing-timescale contract: 1ns/1ns, matching the IEEE 1800 default
/// and slang's TimeLiteral fallback (TimeScale default constructor).
/// All missing-timescale resolution in Lyra must go through this helper.
inline auto ResolveScopeTimeScale(std::optional<slang::TimeScale> ts)
    -> ResolvedTimeScale;

/// Convenience projections -- delegate to ResolveScopeTimeScale.
inline auto ResolveScopeUnitPower(std::optional<slang::TimeScale> ts) -> int;
inline auto ResolveScopePrecisionPower(std::optional<slang::TimeScale> ts)
    -> int;

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

// IEEE 1800 default timescale when no `timescale directive is present.
// Matches slang's TimeScaleValue default constructor (1ns) and the
// TimeScale default constructor (1ns/1ns) used by TimeLiteral::fromSyntax.
constexpr int kDefaultTimeUnitPower = -9;       // 1ns
constexpr int kDefaultTimePrecisionPower = -9;  // 1ns

inline auto ResolveScopeTimeScale(std::optional<slang::TimeScale> ts)
    -> ResolvedTimeScale {
  if (!ts) {
    return {
        .unit_power = kDefaultTimeUnitPower,
        .precision_power = kDefaultTimePrecisionPower,
    };
  }
  return {
      .unit_power = TimeScaleValueToPower(ts->base),
      .precision_power = TimeScaleValueToPower(ts->precision),
  };
}

inline auto ResolveScopeUnitPower(std::optional<slang::TimeScale> ts) -> int {
  return ResolveScopeTimeScale(ts).unit_power;
}

inline auto ResolveScopePrecisionPower(std::optional<slang::TimeScale> ts)
    -> int {
  return ResolveScopeTimeScale(ts).precision_power;
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
    min_power = found ? std::min(min_power, prec) : prec;
    found = true;
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
/// Scans all instances for explicit timescales and returns the finest
/// precision found. If no instance has an explicit timescale, returns
/// the canonical default (1ns) via ResolveScopeTimeScale(nullopt).
inline auto ComputeGlobalPrecision(slang::ast::Compilation& comp) -> int {
  int min_power = 0;
  bool found = false;
  for (const auto* inst : comp.getRoot().topInstances) {
    detail::CollectMinPrecision(*inst, min_power, found);
  }
  return found ? min_power
               : ResolveScopeTimeScale(std::nullopt).precision_power;
}

}  // namespace lyra::lowering::ast_to_hir
