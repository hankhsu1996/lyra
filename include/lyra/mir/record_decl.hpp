#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <vector>

#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/def_path.hpp"
#include "lyra/support/value_operation.hpp"

namespace lyra::mir {

// One body a record answers an operation on its whole value with, defined over
// its members -- two structs are equal when every pair of members is, two
// unions when their first members are (LRM 11.4.5). It takes the parameters the
// operation takes of any value, the value it is asked of first where it is
// asked of one.
struct RecordMethod {
  support::ValueOperation answers;
  CallableCode code;
};

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
// order, which an access reaches by position. Its methods are stated beside it
// (`RecordMethods`), as rustc's derived implementation is an item beside the
// definition it derives from, since a method reaches the record's own members.
struct RecordDecl {
  // The unit that defines the record, where it is another unit's. Such a
  // record is declared here as that unit published it, and this unit states
  // none of its methods, which are that unit's.
  std::optional<std::string> defining_unit;
  support::DefPath path;
  RecordKind kind;
  std::vector<TypeId> elements;
};

// A method for every operation on a whole value a record's type has -- a real
// or a chandle member leaves no bit stream (LRM 6.24.3), a tagged union no
// stream at all (LRM 11.4.14.1), and a member not valid for a net nothing to
// resolve (LRM 6.7.1).
using RecordMethods = std::vector<RecordMethod>;

}  // namespace lyra::mir
