#pragma once

#include <cstdint>

namespace lyra::base {

// Hands out identities of one kind, in order, for entities whose pool does not
// exist yet.
//
// Identity normally comes from the pool that will answer to it: a pool mints an
// id when it takes the value, so the id and the thing it names are settled
// together. That fails only where something must be named before anything can
// hold it -- a declaration that another declaration refers to while both are
// still being settled. There the pool cannot be the source, because it is built
// later, out of facts that are not known yet.
//
// This is the source for that case, and it is a type rather than a counter for
// one reason: an untyped counter can be confused with any other number, and
// what it hands out is an identity. Everything the pool later builds is sized
// and indexed from what was taken here, so this is the authority for the id
// space and nothing downstream re-derives it.
template <typename Id>
class IdAllocator {
 public:
  auto Take() -> Id {
    return Id{next_++};
  }

 private:
  std::uint32_t next_ = 0;
};

}  // namespace lyra::base
