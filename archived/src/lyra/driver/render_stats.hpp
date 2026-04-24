#pragma once

#include "output_phase.hpp"

namespace lyra::lowering::hir_to_mir {
struct LoweringStats;
}

namespace lyra::lowering::mir_to_llvm {
struct JitCompileTimings;
struct JitOrcStats;
}  // namespace lyra::lowering::mir_to_llvm

namespace lyra::driver {

class TextSink;
struct LlvmStats;
struct ProcessStatsData;

void RenderPhaseSummary(
    TextSink& sink, const PhaseDurationArray& durations,
    const PhaseRecordedArray& recorded);
void RenderMirStats(
    TextSink& sink, const lowering::hir_to_mir::LoweringStats& stats);
void RenderLlvmStats(TextSink& sink, const LlvmStats& stats, int top_n);
void RenderProcessStats(TextSink& sink, const ProcessStatsData& data);
void RenderJitTimings(
    TextSink& sink, const lowering::mir_to_llvm::JitCompileTimings& timings);
void RenderOrcStats(
    TextSink& sink, const lowering::mir_to_llvm::JitOrcStats& stats);

}  // namespace lyra::driver
