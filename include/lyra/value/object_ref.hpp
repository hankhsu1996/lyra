#pragma once

#include <cstddef>
#include <utility>

#include "lyra/base/simulation_error.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/managed_ref.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::value {

// LRM 8.4: reaching a non-static member or a virtual method through a null
// object handle is illegal, and the result is indeterminate. Which handle a
// variable holds is a value the design computed, so the access is the design's
// own failure and answers to the run rather than to the host.
[[noreturn]] inline void RaiseNullObjectHandleAccess() {
  throw SimulationError(
      "a member reached through a null object handle "
      "(LRM 8.4)");
}

// A reference to an object the simulator owns (LRM 8.3), on a target that
// reaches a member through a pointer of the class the program point assumes.
// Its value is the handle -- which object it names -- and the pointer is only
// how this target gets from that to a member, so every operator LRM Table 11-1
// gives a handle is the handle's own answer. Where members are reached by a
// coordinate the object answers for, the handle alone is the whole reference.
//
// One shape serves every static view. A program point's static view says what
// may be done through a reference, never what the reference is: two units hold
// one cell under different views wherever a name resolves at elaboration, so a
// representation that followed the view would put two of them on one storage
// and could be read only by arranging for the two to agree.
//
// So the view travels as the pointer the reference was formed with, and a point
// that names a class reads its own pointer back out exactly -- not a
// conversion, since that is the pointer that was stored. A point with no class
// to name reads the handle and leaves the view alone. Which subobject sits at
// an object's own address depends on whether the design declared a virtual
// method, so nothing derived from a view answers which object this is.
class ObjectRef {
 public:
  ObjectRef() = default;

  ObjectRef(std::nullptr_t) {  // NOLINT(google-explicit-constructor)
  }

  ObjectRef(ManagedRef handle, void* view)
      : handle_(std::move(handle)), view_(view) {
  }

  // The object, as the class this program point assumes. A type argument
  // stands where a reference to a cell needs none, because a cell is the one
  // thing it holds while this states which object it names and leaves what may
  // be read through it to the point holding it. Reaching through a reference
  // that names no object is the design's own failure (LRM 8.4).
  template <typename T>
  [[nodiscard]] auto Deref() const -> T& {
    if (view_ == nullptr) {
      RaiseNullObjectHandleAccess();
    }
    return *static_cast<T*>(view_);
  }

  // The same pointer where nothing is reached through it -- forming another
  // view of the object, which is defined on a reference naming none (LRM 8.4
  // admits null as a value and forbids only reaching through it).
  template <typename T>
  [[nodiscard]] auto View() const -> T* {
    return static_cast<T*>(view_);
  }

  [[nodiscard]] auto Handle() const -> const ManagedRef& {
    return handle_;
  }

  [[nodiscard]] auto operator==(const ObjectRef& o) const -> PackedArray {
    return handle_ == o.handle_;
  }
  [[nodiscard]] auto operator!=(const ObjectRef& o) const -> PackedArray {
    return handle_ != o.handle_;
  }

  [[nodiscard]] auto CaseEqual(const ObjectRef& o) const -> PackedArray {
    return handle_.CaseEqual(o.handle_);
  }

  [[nodiscard]] auto IsBitIdentical(const ObjectRef& o) const -> bool {
    return handle_.IsBitIdentical(o.handle_);
  }

  [[nodiscard]] static auto HasUnknown() -> bool {
    return ManagedRef::HasUnknown();
  }

  [[nodiscard]] static auto IsUnknown() -> PackedArray {
    return ManagedRef::IsUnknown();
  }

  // LRM 8.4 / Table 6-7: an uninitialized reference is null, which for this one
  // means naming no object and assuming no class.
  auto ResetToDefault() -> void {
    handle_.ResetToDefault();
    view_ = nullptr;
  }

  explicit operator bool() const {
    return static_cast<bool>(handle_);
  }

 private:
  ManagedRef handle_;
  void* view_ = nullptr;
};

static_assert(LyraValue<ObjectRef>);
static_assert(CaseEqualComparable<ObjectRef>);
static_assert(Defaultable<ObjectRef>);
static_assert(!Ordered<ObjectRef>);
static_assert(!WildcardComparable<ObjectRef>);

}  // namespace lyra::value
