#pragma once

namespace lyra::lowering::mir_to_llvm {
class ForwardingAnalysisReport;
}

namespace lyra::driver {

class TextSink;

void RenderForwardingAnalysisReport(
    TextSink& sink,
    const lowering::mir_to_llvm::ForwardingAnalysisReport& report);

}  // namespace lyra::driver
