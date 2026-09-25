#pragma once

namespace lyra::mir {

struct CompilationUnit;

// Checks what this layer states about the bodies of a unit, where the unit is
// produced, and throws InternalError naming the body that breaks it. A body's
// result type is its call protocol, so a body that is not a coroutine returns
// to its caller before anything else runs and cannot suspend: nothing would
// resume it; and a cleanup runs on every way out of its body, a departure
// included, so it cannot depart itself. Every layer below consumes those facts,
// and a lowering that breaks one is refused here, where the body is still named
// after what the source wrote, rather than as a machine-level fault one
// lowering later or as another compiler's error against generated text.
void Verify(const CompilationUnit& unit);

}  // namespace lyra::mir
