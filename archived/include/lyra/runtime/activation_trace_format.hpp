#pragma once

#include <string>

#include "lyra/runtime/activation_trace.hpp"
#include "lyra/runtime/process_meta.hpp"

namespace lyra::runtime {

// Format an activation event as a single text line for live stderr output.
// Output is stable and machine-parsable (one line per event):
//   [act] t=100 d=0 wake always_ff@top.u_reg cause=edge resume=5 slot=3
//   [act] t=100 d=0 run  always_ff@top.u_reg dirtied=2 resume=5
auto FormatActivationEvent(
    const ActivationEvent& event, const ProcessMetaRegistry& proc_meta)
    -> std::string;

}  // namespace lyra::runtime
