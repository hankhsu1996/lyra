#pragma once

#include <variant>

#include "lyra/runtime/cancellation.hpp"
#include "lyra/runtime/scope_program.hpp"
#include "lyra/runtime/var.hpp"
#include "lyra/value/chandle.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/real.hpp"
#include "lyra/value/runtime_dynamic_array.hpp"
#include "lyra/value/runtime_tuple.hpp"
#include "lyra/value/runtime_unpacked_array.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

// The box a borrowed handle is: a pointer the instance does not own, held so
// that reading the member loads the pointer out rather than the target. It is a
// type of its own rather than a bare pointer so that every alternative of the
// storage below is an object, which keeps a member's address the address of an
// object in every case and never a pointer to a pointer.
struct BorrowedHandle {
  void* target = nullptr;
};

// One member's runtime-owned storage, realized from the descriptor its class
// definition carries. The instance owns this object and a member place resolves
// to its address; what the address means follows the member's storage kind. A
// borrowed handle is a box holding a pointer the instance does not own, so
// reading the member reads the box; an observable cell is the storage itself,
// which library calls reach through its address and never read out as a value;
// an inline value is a pointer-sized value the instance owns, read and written
// directly at its address.
class MemberStorage {
 public:
  explicit MemberStorage(MemberStorageDescriptor descriptor);
  MemberStorage(const MemberStorage&) = delete;
  auto operator=(const MemberStorage&) -> MemberStorage& = delete;
  MemberStorage(MemberStorage&&) = delete;
  auto operator=(MemberStorage&&) -> MemberStorage& = delete;
  ~MemberStorage() = default;

  [[nodiscard]] auto Address() -> void*;

 private:
  std::variant<
      BorrowedHandle, Var<value::PackedArray>, Var<value::String>,
      Var<value::Real>, Var<value::ShortReal>, value::Chandle,
      Var<value::RuntimeTuple>, Var<value::RuntimeDynamicArray>,
      Var<value::RuntimeUnpackedArray>, CancellationSource>
      object_;
};

}  // namespace lyra::runtime
