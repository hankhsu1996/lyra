#pragma once

#include <compare>
#include <cstdint>
#include <string>
#include <variant>

#include "lyra/base/overloaded.hpp"
#include "lyra/base/pool_id.hpp"
#include "lyra/hir/structural_data_object.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/support/def_path.hpp"

namespace lyra::hir {

// Where a member sits in the list its scope published. The unit that publishes
// and the unit that reads both lay the scope's published class out from this
// list, so neither states a field's position to the other.
struct PublishedMemberId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const PublishedMemberId&) const
      -> std::strong_ordering = default;
};

// A member holding its own mutable cell (LRM 6.5).
struct VariableStorage {
  auto operator==(const VariableStorage&) const -> bool = default;
};

// A member whose value is the resolution of its drivers rather than anything
// written to it (LRM 6.6), so a referrer reads and waits on it and never
// stores through it. The fold it resolves under is the declaring unit's to
// install, never a referrer's to know.
struct NetStorage {
  auto operator==(const NetStorage&) const -> bool = default;
};

// A member holding no cell of its own: a `ref` / `const ref` port aliases
// whatever the connection binds it to, and the binding decides whether writing
// through it is permitted (LRM 23.3.3.2).
struct ReferenceStorage {
  ReferenceBinding binding{};

  auto operator==(const ReferenceStorage&) const -> bool = default;
};

// A member holding no object of its own: an interface port stands for an
// instance of another unit that some enclosing scope owns, and the parent binds
// it during elaboration (LRM 25.3).
struct BorrowedObjectStorage {
  auto operator==(const BorrowedObjectStorage&) const -> bool = default;
};

// Which storage a published member is, and so what the member holds: a cell of
// its own, a cell another declaration owns, or an object another scope owns.
// The publishing unit's own declaration is the only source of this, so the
// signature states it and a referrer never reads that declaration to learn it
// -- which is what the external name exists to prevent.
using PublishedStorage = std::variant<
    VariableStorage, NetStorage, ReferenceStorage, BorrowedObjectStorage>;

// One declaration a scope exposes to another unit by name.
//
// `holder` is the scope of the unit the source declares it in, which together
// with its name tells it from every other the unit declares (LRM 23.6): the
// publishing scope itself, a subroutine or labelled block inside it for a
// static variable a hierarchical name reaches through those (LRM 23.9), or a
// class for a static property (LRM 8.9) of one each object of the scope has as
// a type of its own (LRM 6.22). The holder need not lie inside the publishing
// scope: the scope keeping a class's cell is the one replicating the class,
// and a specialization bound to a type of an inner scope is replicated below
// where its generic was declared (LRM 8.25).
struct PublishedMember {
  std::string name;
  support::DefPath holder;
  TypeId type;
  PublishedStorage storage;

  auto operator==(const PublishedMember&) const -> bool = default;
};

// A unit states a declaration's storage on its signature and builds its own
// object from it, so the two cannot describe different storage.
[[nodiscard]] inline auto StorageOf(const StructuralDataObjectDecl& decl)
    -> PublishedStorage {
  return std::visit(
      Overloaded{
          [](const StructuralVariableDecl&) -> PublishedStorage {
            return VariableStorage{};
          },
          [](const StructuralNetDecl&) -> PublishedStorage {
            return NetStorage{};
          },
          [](const StructuralReferenceDecl& reference) -> PublishedStorage {
            return ReferenceStorage{.binding = reference.binding};
          },
          // The three below hold a value settled before the simulation runs,
          // which nothing it does changes: not a driver's resolution and not
          // storage another declaration owns. So each takes a cell of its own,
          // the same storage a declared variable takes, over-provisioned only
          // by what nothing ever waits on.
          //
          // What differs among them is who may name one, which is a separate
          // question from what the storage is. A loop's index does not exist at
          // simulation time so nothing answers its name at all; the other two
          // answer the names the declaring scope and the scopes inside it read,
          // while a hierarchical name reaching one from elsewhere folds to the
          // value one elaboration gave it.
          [](const StructuralGenvarDecl&) -> PublishedStorage {
            return VariableStorage{};
          },
          [](const StructuralConstructionValueDecl&) -> PublishedStorage {
            return VariableStorage{};
          },
          [](const StructuralParameterDecl&) -> PublishedStorage {
            return VariableStorage{};
          }},
      decl.kind);
}

}  // namespace lyra::hir
