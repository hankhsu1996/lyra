#pragma once

#include <array>
#include <cstddef>
#include <cstdint>
#include <string_view>
#include <utility>

namespace lyra::driver {

enum class Phase : uint8_t {
  kParse,
  kElaborate,
  kLowerHir,
  kLowerMir,
  kLowerLlvm,
  kOptimizeIr,
  kEmitObj,
  kLink,
  kJitCompile,
  kSim,
  kCount,
};

inline constexpr size_t kPhaseCount = static_cast<size_t>(Phase::kCount);
using PhaseDurationArray = std::array<double, kPhaseCount>;
using PhaseRecordedArray = std::array<bool, kPhaseCount>;

using PhaseEntry = std::pair<Phase, std::string_view>;
inline constexpr std::array<PhaseEntry, kPhaseCount> kOrderedPhases = {{
    {Phase::kParse, "parse"},
    {Phase::kElaborate, "elaborate"},
    {Phase::kLowerHir, "lower_hir"},
    {Phase::kLowerMir, "lower_mir"},
    {Phase::kLowerLlvm, "lower_llvm"},
    {Phase::kOptimizeIr, "optimize_ir"},
    {Phase::kEmitObj, "emit_obj"},
    {Phase::kLink, "link"},
    {Phase::kJitCompile, "jit_compile"},
    {Phase::kSim, "sim"},
}};

struct PhaseSummaryData {
  PhaseDurationArray durations{};
  PhaseRecordedArray recorded{};
};

enum class HeartbeatPolicy : uint8_t {
  kDisabled,
  kEnabled,
};

}  // namespace lyra::driver
