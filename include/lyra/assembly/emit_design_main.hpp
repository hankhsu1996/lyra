#pragma once

#include <cstdint>
#include <string>
#include <vector>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/source_manager.hpp"
#include "lyra/llvm_backend/lower.hpp"
#include "lyra/lowering/diagnostic_context.hpp"

namespace lyra::lowering::mir_to_llvm {
struct CodegenSession;
}  // namespace lyra::lowering::mir_to_llvm

namespace lyra::assembly {

// Narrow assembly contract for design-wide main() emission.
// Contains only what main() generation needs beyond what is already
// accessible through CodegenSession (Context carries design, type_arena,
// layout, force_two_state, etc.).
struct EmitDesignMainInput {
  const lowering::DiagnosticContext* diag_ctx = nullptr;
  const SourceManager* source_manager = nullptr;
  lowering::mir_to_llvm::SimulationHooks* hooks = nullptr;
  lowering::mir_to_llvm::MainAbi main_abi =
      lowering::mir_to_llvm::MainAbi::kEmbeddedPlusargs;
  std::string fs_base_dir;
  std::vector<std::string> plusargs;
  uint32_t feature_flags = 0;
};

// Emit the design-wide main() function into the LLVM module owned by session.
// This is assembly work: allocate DesignState, initialize runtime, run init
// processes, call LyraRunSimulation, release resources.
auto EmitDesignMain(
    lowering::mir_to_llvm::CodegenSession& session,
    const EmitDesignMainInput& input) -> Result<void>;

// Build the narrow assembly input from the backend's LoweringInput.
auto BuildEmitDesignMainInput(const lowering::mir_to_llvm::LoweringInput& input)
    -> EmitDesignMainInput;

}  // namespace lyra::assembly
