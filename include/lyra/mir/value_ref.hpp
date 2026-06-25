#pragma once

#include "lyra/mir/block_hops.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/member.hpp"

namespace lyra::mir {

// A member of a class, named by its class-local id. The class a member access
// reaches is determined by the access's receiver (the member belongs to the
// receiver's class), not carried here -- a reference names the member, not its
// owner.
struct MemberRef {
  MemberId var{};
};

struct LocalRef {
  BlockHops hops;
  LocalId var{};
};

}  // namespace lyra::mir
