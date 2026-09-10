#pragma once

#include <cstdint>

#include "lyra/runtime/scope_program.hpp"
#include "lyra/runtime/storage_block.hpp"

namespace lyra::runtime {

// The immutable definition of one class: the storage its properties need,
// held once and shared by every object built from it. A class joins no
// lifecycle and holds no place in the object tree, and which body initializes
// an object is settled where the object is asked for rather than by the class
// it is of (LRM 8.7), so a definition of one carries the storage and nothing
// else.
struct ObjectDefinition {
  MemberStorageSchema members;
};

// An object the program built with `new` (LRM 8.3), whose lifetime the
// simulator owns rather than any scope. It owns one storage object per
// property, so a property place resolves to that storage's address exactly as a
// scope member's does.
class ManagedObject {
 public:
  explicit ManagedObject(const ObjectDefinition* definition);

  // Where property `index` lives, which is what a place naming it resolves to.
  [[nodiscard]] auto MemberAddress(std::uint32_t index) -> void*;

 private:
  StorageBlock members_;
};

}  // namespace lyra::runtime
