#pragma once

#include <cstdint>

#include "lyra/runtime/scope_program.hpp"
#include "lyra/runtime/storage_block.hpp"

namespace lyra::runtime {

// The body that brings one object's properties to their initial values,
// entered on the object it is building (LRM 8.7).
using ObjectEntry = void (*)(void* self);

// The immutable definition of one class: the storage schema its properties
// need, and the body that initializes them. Held once and shared by every
// object built from it, the way a scope class's definition is shared by its
// instances. A class joins no lifecycle and holds no place in the object tree,
// so a definition of one carries these two and nothing else.
struct ObjectDefinition {
  MemberStorageSchema members;
  ObjectEntry construct = nullptr;
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

  // Runs the body that initializes the properties, on an object whose storage
  // already exists, so a body reaching its own property finds storage rather
  // than building it.
  void Construct();

 private:
  const ObjectDefinition* definition_;
  StorageBlock members_;
};

}  // namespace lyra::runtime
