#pragma once

#include <memory>
#include <utility>

#include "lyra/value/integral_fwd.hpp"
#include "lyra/value/object_ref.hpp"

namespace lyra::runtime {

class ReadReport;

// A reference to an object that does not keep it (LRM 8.29, 8.30): the
// object's ownership held weakly. It answers no object once the object has
// expired -- once nothing refers to it -- and none where it was made from null.
class WeakObjectRef {
 public:
  WeakObjectRef() = default;

  explicit WeakObjectRef(std::weak_ptr<void> share) : share_(std::move(share)) {
  }

  [[nodiscard]] auto Share() const -> const std::weak_ptr<void>& {
    return share_;
  }

 private:
  std::weak_ptr<void> share_;
};

// The weak reference to the object `strong` refers to, or to none.
[[nodiscard]] auto Downgrade(const value::ObjectRef& strong) -> WeakObjectRef;

// The object `weak` refers to while it has not expired, and null after. An
// evaluation a wait makes (LRM 8.30.3) is handed `report`, and is told to watch
// the object's expiry, which changes this answer; null where no wait asks.
[[nodiscard]] auto Upgrade(const WeakObjectRef& weak, ReadReport* report)
    -> value::ObjectRef;

// Whether `weak` refers to the object `strong` names: both to none, or both to
// one object. One that has expired refers to no object a handle can name, so it
// differs from every handle, null included, and an object made at the address
// of one that has expired is not taken for it.
[[nodiscard]] auto WeakRefersTo(
    const WeakObjectRef& weak, const value::ObjectRef& strong) -> bool;

// A number for the object `object` refers to (LRM 8.30.5): 0 for none, and one
// nonzero number for one object for as long as it exists, whichever class of
// its lineage the handle sees it as, which no other object existing at the
// same time has. An object that has expired may leave its number to another.
[[nodiscard]] auto ObjectIdentity(const value::ObjectRef& object)
    -> value::LongInt;

}  // namespace lyra::runtime
