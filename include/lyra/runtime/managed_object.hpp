#pragma once

#include <cstdint>
#include <span>
#include <vector>

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

// The definition of one class: the storage its properties need, the bodies its
// dispatch positions hold, and where its own properties and behaviors begin in
// a value of any class extending it. A class joins no lifecycle and holds no
// place in the object tree, and which body initializes an object is settled
// where the object is asked for rather than by the class it is of (LRM 8.7), so
// what a definition carries is what every object of the class shares and
// nothing about any one of them.
//
// The first two are read of the class a value is; the last two are read of the
// class an access names, which is what lets an access name a property or a
// behavior of an ancestor without knowing what the value it runs on turns out
// to be. All four are settled when the class is realized, which is only once
// the generated code is brought up: that code takes the definition's address,
// so the record has to exist before anything it holds does.
struct ObjectDefinition {
  MemberStorageSchema members;
  MethodDispatchTable methods;
  std::uint32_t first_member = 0;
  std::uint32_t first_behavior = 0;
};

// One behavior a class takes over from its lineage (LRM 8.20): the behavior,
// named the way every reader of one names it, and the body this class answers
// it with.
struct DispatchTakeover {
  const ObjectDefinition* introduced_by = nullptr;
  std::uint32_t ordinal = 0;
  ErasedMethodEntry body = nullptr;
};

// What one class adds to its lineage: the class it extends, the storage its own
// properties need, the behaviors it introduces in the order it introduces them,
// and the ones it takes over. A class states what it adds and nothing about the
// lineage, which is what keeps one declaration's meaning independent of what
// extends it and what lets a class be stated by a unit that cannot see past its
// own boundary.
struct ClassContribution {
  const ObjectDefinition* base = nullptr;
  std::span<const MemberStorageDescriptor> members;
  std::span<const ErasedMethodEntry> introductions;
  std::span<const DispatchTakeover> takeovers;
};

// The flat forms every value of one class shares. Held apart from the
// definition because a definition names storage rather than owning it, the way
// a scope's and a closure's do, and it must outlive every value built from it.
struct RealizedClass {
  std::vector<MemberStorageDescriptor> members;
  std::vector<ErasedMethodEntry> methods;
};

// Completes `definition` by extending what `adds.base` was realized with, into
// storage `realization` holds. A class is realized after the one it extends,
// which is what makes this one step rather than a walk, and a class extending
// nothing starts from empty by taking that step over no base at all.
void RealizeClass(
    const ClassContribution& adds, RealizedClass& realization,
    ObjectDefinition& definition);

// An object the program built with `new` (LRM 8.3), whose lifetime the
// simulator owns rather than any scope. It owns one storage object per
// property, so a property place resolves to that storage's address exactly as a
// scope member's does, and it keeps the definition it was built from, which is
// what makes what class it is a question about the object itself.
class ManagedObject {
 public:
  explicit ManagedObject(const ObjectDefinition* definition);

  // Where the property `declared_by` gave `slot` to lives on this object, which
  // is what a place naming it resolves to. The pair is the whole coordinate: a
  // class extending another carries its base's properties as well as its own
  // and may declare one of the same name, so which storage is meant is fixed by
  // the class the access names (LRM 8.14) rather than by what this object is.
  [[nodiscard]] auto MemberAddress(
      const ObjectDefinition* declared_by, std::uint32_t slot) -> void*;

  // The body this object's class answers `introduced_by`'s `ordinal`-th
  // behavior with (LRM 8.20). What class an object is, is this side's to
  // answer; entering the body is the asking code's own, which is why this
  // answers with the address.
  [[nodiscard]] auto Method(
      const ObjectDefinition* introduced_by, std::uint32_t ordinal) const
      -> ErasedMethodEntry;

 private:
  const ObjectDefinition* definition_;
  StorageBlock members_;
};

}  // namespace lyra::runtime
