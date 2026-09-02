#pragma once

#include <cstdint>
#include <memory>
#include <vector>

#include "lyra/runtime/member_storage.hpp"
#include "lyra/runtime/scope_program.hpp"

namespace lyra::runtime {

// The storage one declaration's schema asks a value of it to own: one storage
// object per descriptor, in schema order, reached by the index the declaration
// gave that slot. A scope's members, a closure's captures, and an object's
// properties are the same relation between a described shape and a value that
// realizes it, so they are one block rather than several vectors that happen to
// agree.
class StorageBlock {
 public:
  explicit StorageBlock(MemberStorageSchema schema);

  // Where slot `index` lives, which is what a place naming it resolves to.
  [[nodiscard]] auto Address(std::uint32_t index) -> void*;

  // What slot `index` holds, as the handle it crosses to generated code as.
  [[nodiscard]] auto Held(std::uint32_t index) -> void*;

  // Takes a copy of what `handle` names into slot `index`, which is how a value
  // reaches storage outliving the stretch that made it.
  void Adopt(std::uint32_t index, void* handle);

  [[nodiscard]] auto Size() const -> std::uint32_t;

 private:
  std::vector<std::unique_ptr<MemberStorage>> slots_;
};

}  // namespace lyra::runtime
