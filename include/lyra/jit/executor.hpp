#pragma once

namespace lyra::lir {
struct CompilationUnit;
}  // namespace lyra::lir

namespace lyra::compiler {
struct ElaboratedUnitMetadata;
}  // namespace lyra::compiler

namespace lyra::jit {

// Emits one compilation unit's executable body to an LLVM module, JIT-compiles
// it in this process, constructs the design against the runtime -- filling the
// unit definition's constant metadata from `metadata`, the source-level facts
// LIR does not carry -- and runs the simulation to completion, returning its
// exit code. The JIT owning the generated code outlives the design, so the
// runtime's pointers into generated code stay valid for the whole run.
auto Execute(
    const lir::CompilationUnit& unit,
    const compiler::ElaboratedUnitMetadata& metadata) -> int;

}  // namespace lyra::jit
