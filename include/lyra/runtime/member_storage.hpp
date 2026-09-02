#pragma once

#include <variant>

#include "lyra/runtime/activation_value_cell.hpp"
#include "lyra/runtime/cancellation.hpp"
#include "lyra/runtime/file_table.hpp"
#include "lyra/runtime/gc_ref.hpp"
#include "lyra/runtime/scope_program.hpp"
#include "lyra/runtime/var.hpp"
#include "lyra/value/chandle.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/real.hpp"
#include "lyra/value/runtime_associative_array.hpp"
#include "lyra/value/runtime_dynamic_array.hpp"
#include "lyra/value/runtime_queue.hpp"
#include "lyra/value/runtime_tagged_union.hpp"
#include "lyra/value/runtime_tuple.hpp"
#include "lyra/value/runtime_union.hpp"
#include "lyra/value/runtime_unpacked_array.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

// An object built by `new`, whose storage this file's storage is one slot of.
// Named rather than included, because the object owns a block of these slots
// and so is defined in terms of them.
class ManagedObject;

// The box a borrowed handle is: a pointer the instance does not own, held so
// that reading the member loads the pointer out rather than the target. It is a
// type of its own rather than a bare pointer so that every alternative of the
// storage below is an object, which keeps a member's address the address of an
// object in every case and never a pointer to a pointer.
struct BorrowedHandle {
  void* target = nullptr;
};

// One member's runtime-owned storage, realized from the descriptor its
// declaration carries. The owner owns this object and a member place resolves
// to its address; what the address means follows the member's storage kind. A
// borrowed handle is a box holding a pointer the owner does not own, so reading
// the member reads the box; an observable cell is the storage itself, which
// library calls reach through its address and never read out as a value; an
// inline value is a value the owner owns, whose address is the handle it
// crosses as.
//
// The same storage serves a closure value's captures, which are members of the
// declaration whose invoke reads them: a captured pointer or reference is a
// borrowed handle, and a captured value is an inline one the closure owns for
// its whole life, so nothing a deferred body reads points into the stretch that
// built it.
class MemberStorage {
 public:
  explicit MemberStorage(MemberStorageDescriptor descriptor);
  MemberStorage(const MemberStorage&) = delete;
  auto operator=(const MemberStorage&) -> MemberStorage& = delete;
  MemberStorage(MemberStorage&&) = delete;
  auto operator=(MemberStorage&&) -> MemberStorage& = delete;
  ~MemberStorage() = default;

  // Where this storage lives, which is what a member place resolves to.
  [[nodiscard]] auto Address() -> void*;

  // What this storage holds, as the handle it crosses to generated code as.
  // Nothing is copied out: the storage outlives every read of it, and a value
  // handle is never written through.
  [[nodiscard]] auto HeldValue() -> void*;

  // Takes a copy of what `handle` names, which is how a value reaches storage
  // that outlives the stretch the value was made in. Only storage its owner
  // fills at construction takes this; a cell is written through its own access,
  // where the write is an update event.
  void AdoptFrom(void* handle);

 private:
  std::variant<
      BorrowedHandle, CancellationTarget, ChannelCancellation,
      Var<value::PackedArray>, Var<value::String>, Var<value::Real>,
      Var<value::ShortReal>, Var<value::RuntimeTuple>, Var<value::RuntimeUnion>,
      Var<value::RuntimeTaggedUnion>, Var<value::RuntimeDynamicArray>,
      Var<value::RuntimeUnpackedArray>, Var<value::RuntimeQueue>,
      Var<value::RuntimeAssociativeArray>, value::Chandle, value::PackedArray,
      value::String, value::Real, value::ShortReal, value::RuntimeTuple,
      value::RuntimeUnion, value::RuntimeTaggedUnion,
      value::RuntimeDynamicArray, value::RuntimeUnpackedArray,
      value::RuntimeQueue, value::RuntimeAssociativeArray, GcRef<ManagedObject>,
      ActivationValueCell<value::PackedArray>,
      ActivationValueCell<value::String>, ActivationValueCell<value::Real>,
      ActivationValueCell<value::ShortReal>,
      ActivationValueCell<value::RuntimeTuple>,
      ActivationValueCell<value::RuntimeUnion>,
      ActivationValueCell<value::RuntimeTaggedUnion>,
      ActivationValueCell<value::RuntimeDynamicArray>,
      ActivationValueCell<value::RuntimeUnpackedArray>,
      ActivationValueCell<value::RuntimeQueue>,
      ActivationValueCell<value::RuntimeAssociativeArray>>
      object_;
};

}  // namespace lyra::runtime
