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

}  // namespace lyra::mir
