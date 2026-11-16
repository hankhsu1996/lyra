#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <vector>

#include "lyra/mir/type_id.hpp"
#include "lyra/support/def_path.hpp"

namespace lyra::mir {

// Which aggregate a record declares: a structure (LRM 7.2), whose members each
// hold storage of their own, or a union (LRM 7.3), untagged or tagged, whose
// members overlay one storage.
enum class RecordKind : std::uint8_t {
  kStruct,
  kUnion,
  kTaggedUnion,
};

// A structure or a union a unit declares, as clang's record declaration is
// either: which declaration of its unit it is, which is what another unit
// reaches it by, which aggregate it is, and its members' types in declaration
// order, which an access reaches by position. Its methods are declarations of
// the unit's table that name it as their owner, as rustc's derived
// implementation is an item beside the definition it derives from, since a
// method reaches the record's own members.
struct RecordDecl {
  // The unit that defines the record, where it is another unit's. Such a
  // record is declared here as that unit published it, and its methods are
  // that unit's: this unit declares each of them and defines none.
  std::optional<std::string> defining_unit;
  support::DefPath path;
  RecordKind kind;
  std::vector<TypeId> elements;
};

}  // namespace lyra::mir
