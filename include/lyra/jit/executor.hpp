#pragma once

#include <span>

namespace lyra::lir {
struct CompilationUnit;
}  // namespace lyra::lir

namespace lyra::compiler {
struct ElaboratedUnitMetadata;
}  // namespace lyra::compiler

namespace lyra::jit {

// JIT-compiles every unit of a design in this process and runs it to
// completion, returning the simulation's exit code. Each unit's executable body
// becomes one module; its definition metadata (`metadata`, co-indexed with
// `units`) fills the constant facts LIR does not carry. The design-root unit's
// construct elaborates the design by building the top-level units as its owned
// children -- the same path the C++ backend takes through the root's
// constructor. The JIT owning the generated code outlives the design, so the
// runtime's pointers into generated code stay valid for the whole run.
auto Execute(
    std::span<const lir::CompilationUnit> units,
    std::span<const compiler::ElaboratedUnitMetadata> metadata,
    const lir::CompilationUnit& root_unit,
    const compiler::ElaboratedUnitMetadata& root_metadata) -> int;

}  // namespace lyra::jit
