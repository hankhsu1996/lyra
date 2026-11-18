#pragma once

#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <variant>
#include <vector>

#include "lyra/base/overloaded.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/def_path.hpp"

namespace lyra::mir {

// Which aggregate a record declares: a structure (LRM 7.2), whose members each
// hold storage of their own, or a union (LRM 7.3), untagged or tagged, whose
// members are read and written through one storage.
enum class RecordKind : std::uint8_t {
  kStruct,
  kUnion,
  kTaggedUnion,
};

// A structure's members, in declaration order, each storage of its own.
struct StructRecord {
  std::vector<TypeId> members;
};

// An untagged union: one storage, an integral value of as many positions as
// its widest member has, and its members, in declaration order, each a typed
// view of that storage's first positions (LRM 7.3). The members are what the
// union is read and written through; the storage is the one component a field
// step reaches.
struct UnionRecord {
  TypeId storage;
  std::vector<TypeId> members;
};

// A tagged union's members, in declaration order, overlaid at its start, then
// its tag (LRM 7.3.2).
struct TaggedUnionRecord {
  std::vector<TypeId> members;
};

// A structure or a union a unit declares, as clang's record declaration is
// either: which declaration of its unit it is, which is what another unit
// reaches it by, and what the aggregate holds. Its methods are declarations of
// the unit's table that name it as their owner, as rustc's derived
// implementation is an item beside the definition it derives from, since a
// method reaches the record's own members.
struct RecordDecl {
  // The unit that defines the record, where it is another unit's. Such a
  // record is declared here as that unit published it, and its methods are
  // that unit's: this unit declares each of them and defines none.
  std::optional<std::string> defining_unit;
  support::DefPath path;
  std::variant<StructRecord, UnionRecord, TaggedUnionRecord> shape;
};

// Which aggregate `record` declares.
[[nodiscard]] inline auto KindOf(const RecordDecl& record) -> RecordKind {
  return std::visit(
      Overloaded{
          [](const StructRecord&) { return RecordKind::kStruct; },
          [](const UnionRecord&) { return RecordKind::kUnion; },
          [](const TaggedUnionRecord&) { return RecordKind::kTaggedUnion; }},
      record.shape);
}

// The components a field step on a value of `record` reaches, by position: a
// structure's members, an untagged union's one storage, a tagged union's
// members.
[[nodiscard]] inline auto ComponentsOf(const RecordDecl& record)
    -> std::span<const TypeId> {
  return std::visit(
      Overloaded{
          [](const StructRecord& s) {
            return std::span<const TypeId>(s.members);
          },
          [](const UnionRecord& u) {
            return std::span<const TypeId>(&u.storage, 1);
          },
          [](const TaggedUnionRecord& t) {
            return std::span<const TypeId>(t.members);
          }},
      record.shape);
}

// The members of the aggregate `record` declares, in declaration order: a
// structure's components, and the members a union is read through.
[[nodiscard]] inline auto MembersOf(const RecordDecl& record)
    -> std::span<const TypeId> {
  return std::visit(
      Overloaded{
          [](const StructRecord& s) {
            return std::span<const TypeId>(s.members);
          },
          [](const UnionRecord& u) {
            return std::span<const TypeId>(u.members);
          },
          [](const TaggedUnionRecord& t) {
            return std::span<const TypeId>(t.members);
          }},
      record.shape);
}

}  // namespace lyra::mir
