#pragma once

#include "lyra/support/system_subroutine.hpp"

namespace lyra::mir {

// LRM 20.3 simulation-time read ($time / $stime / $realtime). The node carries
// only which function it is; the scaling target (the calling scope's time unit)
// is read at render time from the enclosing scope's emitted `kTimeUnitPower`
// constant, and the design-global tick comes from the engine at run time. So a
// time read inside a subroutine scales by its declaring scope's unit (LRM
// 3.14.2 / 20.3.1) with nothing threaded onto this node.
struct RuntimeTimeCall {
  support::TimeKind kind;
};

}  // namespace lyra::mir
