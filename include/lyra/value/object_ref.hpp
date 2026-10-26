#pragma once

#include <cstddef>
#include <utility>

#include "lyra/base/simulation_error.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/managed_ref.hpp"

namespace lyra::value {

// LRM 8.4: reaching a non-static member or a virtual method through a null
// object handle is illegal, and the result is indeterminate. Which handle a
// variable holds is a value the design computed, so the access is the design's
// own failure and answers to the run rather than to the host.
[[noreturn]] inline void RaiseNullObjectHandleAccess() {
  throw SimulationError("a member reached through a null object handle");
}

// A reference to an object the simulator owns (LRM 8.3): which object it
// names, and a share of its ownership. Every operator LRM Table 11-1 gives a
// handle is the handle's own answer.
//
// One shape serves every class a program point sees the object as. A point's
// class says what may be done through a reference, never what the reference
// is, and every class of an object's lineage starts where the object does,
// since a class extends one class (LRM 8.13) and an interface class adds no
// part of its own (LRM 8.26). So the object's address is what a point reaches
// a member through, whichever class it names.
class ObjectRef {
 public:
  ObjectRef() = default;

  ObjectRef(std::nullptr_t) {  // NOLINT(google-explicit-constructor)
  }

  explicit ObjectRef(ManagedRef handle) : handle_(std::move(handle)) {
  }

  // The object, as the class this program point assumes. A type argument
  // stands where a reference to a cell needs none, because a cell is the one
  // thing it holds while this states which object it names and leaves what may
  // be read through it to the point holding it. Reaching through a reference
  // that names no object is the design's own failure (LRM 8.4).
  template <typename T>
  [[nodiscard]] auto Deref() const -> T& {
    T* object = Get<T>();
    if (object == nullptr) {
      RaiseNullObjectHandleAccess();
    }
    return *object;
  }

  // The same address where nothing is reached through it, which is defined on
  // a reference naming no object (LRM 8.4 admits null as a value and forbids
  // only reaching through it).
  template <typename T>
  [[nodiscard]] auto Get() const -> T* {
    return static_cast<T*>(handle_.Share().get());
  }

  [[nodiscard]] auto Handle() const -> const ManagedRef& {
    return handle_;
  }

  [[nodiscard]] auto operator==(const ObjectRef& o) const -> Bit {
    return handle_ == o.handle_;
  }
  [[nodiscard]] auto operator!=(const ObjectRef& o) const -> Bit {
    return handle_ != o.handle_;
  }

  [[nodiscard]] auto CaseEqual(const ObjectRef& o) const -> Bit {
    return handle_.CaseEqual(o.handle_);
  }

  [[nodiscard]] auto IsBitIdentical(const ObjectRef& o) const -> bool {
    return handle_.IsBitIdentical(o.handle_);
  }

  [[nodiscard]] static auto HasUnknown() -> bool {
    return ManagedRef::HasUnknown();
  }

  [[nodiscard]] static auto IsUnknown() -> Bit {
    return ManagedRef::IsUnknown();
  }

  // LRM 6.24.3 streams a class object as its members, which is not carried out
  // yet; the execution backend answers a handle the same way. A structure with
  // a handle member asks these only where the program measures it.
  [[noreturn]] static auto BitstreamWidth() -> Int {
    throw SimulationError("'$bits' of a class object is not yet supported");
  }
  // LRM 20.9 counts over the bit stream.
  template <IntegralValue Control>
  [[nodiscard]] static auto CountBits(const Control& control_bits) -> Int {
    return Stream().CountBits(control_bits);
  }

  explicit operator bool() const {
    return static_cast<bool>(handle_);
  }

 private:
  [[noreturn]] static auto Stream() -> Bit {
    throw SimulationError(
        "reading a class object as a stream of bits is not yet supported");
  }

  ManagedRef handle_;
};

static_assert(LyraValue<ObjectRef>);
static_assert(CaseEqualComparable<ObjectRef>);
static_assert(!Ordered<ObjectRef>);

}  // namespace lyra::value
