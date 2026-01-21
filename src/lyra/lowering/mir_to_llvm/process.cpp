#include "lyra/lowering/mir_to_llvm/process.hpp"

#include "lyra/lowering/mir_to_llvm/instruction.hpp"

namespace lyra::lowering::mir_to_llvm {

void LowerProcess(Context& context, const mir::Process& process) {
  // For now, iterate through blocks sequentially
  // (assumes linear control flow for simple cases like hello world)
  for (const auto& block : process.blocks) {
    for (const auto& instruction : block.instructions) {
      LowerInstruction(context, instruction);
    }
  }
}

}  // namespace lyra::lowering::mir_to_llvm
