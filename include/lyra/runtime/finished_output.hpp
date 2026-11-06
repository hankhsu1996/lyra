#pragma once

namespace lyra::runtime {

// What the run had finished reaches where it was going, which is what an
// ending the process can still act on does before it ends: each file's
// finished bytes, and each dump's recorded time steps, the file ended where
// the run was. Called from a signal handler, from the fault handler and from
// the checker's report, so nothing here is beyond what a signal handler may
// call; work that is, a dump's thread does.
void DeliverFinishedOutput() noexcept;

}  // namespace lyra::runtime
