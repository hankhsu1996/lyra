#pragma once

#include <variant>

#include "lyra/runtime/scope_program.hpp"
#include "lyra/runtime/var.hpp"
#include "lyra/value/chandle.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/real.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

// One member's runtime-owned storage, realized from the descriptor its unit
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
      void*, Var<value::PackedArray>, Var<value::String>, Var<value::Real>,
      Var<value::ShortReal>, value::Chandle>
      object_;
};

}  // namespace lyra::runtime
