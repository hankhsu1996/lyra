#pragma once

#include <cstdint>
#include <span>

#include "lyra/runtime/scope_program.hpp"
#include "lyra/runtime/storage_block.hpp"

namespace lyra::runtime {

// One entry of a class's dispatch table: a code address with its prototype
// erased, so entries of every signature share one table. A dispatch position
// carries one signature in every class that fills it (LRM 8.20), so the call
// site restores the exact type the body was generated with and the two cannot
// disagree -- the same erasure, for the same reason, that a scope's exports
// use.
using ErasedMethodEntry = void (*)();

// The bodies a class fills its dispatch positions with (LRM 8.20), in position
// order and shared by every object of the class. It crosses as a pointer plus a
// length rather than a C++ container, like the storage schema beside it, so a
// definition can name a table that outlives whatever built it. An entry is null
// where nothing in the class's lineage supplied a body (LRM 8.21 pure virtual);
// no object carries one, because such a class is never constructed.
struct MethodDispatchTable {
  const ErasedMethodEntry* data = nullptr;
  std::uint32_t size = 0;

  [[nodiscard]] constexpr auto Entries() const
      -> std::span<const ErasedMethodEntry> {
    return {data, size};
  }
};

// The immutable definition of one class: the storage its properties need and
// the bodies its dispatch positions hold, held once and shared by every object
// built from it. A class joins no lifecycle and holds no place in the object
// tree, and which body initializes an object is settled where the object is
// asked for rather than by the class it is of (LRM 8.7), so what a definition
// carries is what every object of the class shares and nothing about any one of
// them.
struct ObjectDefinition {
  MemberStorageSchema members;
  MethodDispatchTable methods;
};

// An object the program built with `new` (LRM 8.3), whose lifetime the
// simulator owns rather than any scope. It owns one storage object per
// property, so a property place resolves to that storage's address exactly as a
// scope member's does, and it keeps the definition it was built from, which is
// what makes what class it is a question about the object itself.
class ManagedObject {
 public:
  explicit ManagedObject(const ObjectDefinition* definition);

  // Where property `index` lives, which is what a place naming it resolves to.
  [[nodiscard]] auto MemberAddress(std::uint32_t index) -> void*;

  // The body this object's class holds at dispatch position `position` (LRM
  // 8.20). What class an object is, is this side's to answer; entering the body
  // is the asking code's own, which is why this answers with the address.
  [[nodiscard]] auto Method(std::uint32_t position) const -> ErasedMethodEntry;

 private:
  const ObjectDefinition* definition_;
  StorageBlock members_;
};

}  // namespace lyra::runtime
