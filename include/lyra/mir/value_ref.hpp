#pragma once

#include "lyra/mir/block_hops.hpp"
#include "lyra/mir/enclosing_hops.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/member.hpp"

namespace lyra::mir {

struct MemberRef {
  EnclosingHops hops;
  MemberId var{};
};

struct LocalRef {
  BlockHops hops;
  LocalId var{};
};

// A sensitivity leaf observes a member that resolves to a stored
// `Observable` the scheduler subscribes to: a plain signal, an upward
// ExternalRef member, or a downward borrowed-pointer slot. All three are
// MemberRefs; the var's type tells the renderer how to reach the cell.
using SensitivityRef = MemberRef;

}  // namespace lyra::mir
