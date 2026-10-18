#pragma once

namespace lyra::value {

// Reports that a write left a bounded queue longer than its declared bound, so
// the elements past it were dropped (LRM 7.10.5). The trimming is what the
// standard requires and not an error, but it loses data the design wrote, which
// is worth saying. Both realizations of a queue report it through here, so the
// two cannot come to say different things about the same event.
void ReportBoundOverflow();

// Who hears that report. A value knows nothing of where in the design or when
// it is written, so the run listens for as long as it lasts and says it as one
// of its own reports; with nobody listening, nothing is said.
using BoundOverflowListener = void (*)();

// Installs `listener` and answers the one it replaces.
auto ListenForBoundOverflow(BoundOverflowListener listener)
    -> BoundOverflowListener;

}  // namespace lyra::value
