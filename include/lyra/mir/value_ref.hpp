#pragma once

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

// An activation local or parameter of the enclosing callable, named by its
// id in that callable's `locals` arena. It carries no navigation distance:
// every local of a callable lives in one arena, so a reference within the
// callable names the binding directly. A binding in an enclosing callable is
// never named this way -- it crosses the boundary as a captured field, read by
// a field access over the closure receiver (the invoke body's `locals[0]`).
struct LocalRef {
  LocalId var{};
};

}  // namespace lyra::mir
