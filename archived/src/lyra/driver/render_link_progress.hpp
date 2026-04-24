#pragma once

namespace lyra::lowering::mir_to_llvm {
struct LinkProgressSnapshot;
}

namespace lyra::driver {

class TextSink;

void RenderLinkProgress(
    TextSink& sink,
    const lowering::mir_to_llvm::LinkProgressSnapshot& snapshot);

}  // namespace lyra::driver
