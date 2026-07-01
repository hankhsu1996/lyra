#pragma once

#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/member.hpp"

namespace lyra::mir {

// A closure record declaration: the compiler-generated nominal value type one
// closure site constructs. It carries the captured bindings as named fields
// plus exactly one invoke body, and nothing else -- no base, no methods, no
// dispatch, no lifecycle. A closure value copies / moves it like any value.
//
// Three identities stay distinct: the binding origin that keys a capture, the
// field id `MemberId` the invoke body reads (assigned when the capture is
// discovered, so `fields` is in discovery order), and the physical layout
// position. `layout` is the field ids in canonical order (by binding origin);
// it fixes only the physical order a backend emits -- capture-clause order,
// dump order, destruction order -- and never the body, which reads by the
// stable `MemberId`.
//
// `invoke.locals[0]` is the closure receiver: a read-only borrow of this
// record. A captured read is `MemberAccess(receiver, MemberId)` -- the same
// field-access primitive an object member read uses, over a field-bearing
// nominal receiver that happens to be a value record rather than an object.
struct ClosureRecord {
  base::Arena<MemberDecl, MemberId> fields;
  std::vector<MemberId> layout;
  CallableCode invoke;
};

}  // namespace lyra::mir
