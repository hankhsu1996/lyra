#pragma once

#include <filesystem>
#include <span>
#include <string>

#include "lyra/compiler/unit_metadata.hpp"
#include "lyra/diag/diagnostic.hpp"

namespace lyra::jit {

// JIT-compiles every unit of a design in this process and runs it to
// completion, returning the simulation's exit code. Each unit's executable body
// becomes one module, and its definition metadata fills the constant facts LIR
// does not carry. The design-root unit's construct elaborates the design by
// building the top-level units as its owned children -- the same path the C++
// backend takes through the root's constructor. The JIT owning the generated
// code outlives the design, so the runtime's pointers into generated code stay
// valid for the whole run.
//
// `dpi_objects` are the compiled DPI-C link inputs (LRM 35). The session is
// this design's linker, so they are linked into it rather than loaded beside
// it: that is what resolves a generated foreign call against a symbol this
// process does not define, and equally an exported subroutine the foreign side
// calls back under a name only the session defines. A design with no foreign
// sources passes none.
//
// `simulation_arguments` are the design's own arguments, the ones a caller
// keeps apart from the compiler's; the run reads its LRM 21.6 plusargs out of
// them, as a built program reads them out of its argv.
//
// A design naming a construct the execution backend has no entry for is refused
// before anything runs, so nothing of the simulation is observed.
auto Execute(
    std::span<const compiler::ExecutableUnit> units,
    const compiler::ExecutableUnit& root_unit,
    std::span<const std::filesystem::path> dpi_objects,
    std::span<const std::string> simulation_arguments) -> diag::Result<int>;

}  // namespace lyra::jit
