#pragma once

#include <optional>
#include <span>
#include <utility>
#include <vector>

#include "lyra/abi/library_records.hpp"
#include "lyra/abi/records.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/constant_id.hpp"
#include "lyra/mir/constant_value.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

// The types the library declares the members of `Record` at, in its order.
template <typename Record>
auto LibraryRecordMembersOf(const CompilationUnit& unit)
    -> std::span<const TypeId> {
  const std::optional<std::span<const TypeId>> members =
      unit.LibraryRecordMembers(abi::LibraryDeclOf<Record>());
  if (!members.has_value()) {
    throw InternalError(
        "mir: a unit declares no structure of the library a constant is "
        "stated as -- please report this as a bug");
  }
  return *members;
}

// The type the library declares `member` at.
template <typename Record, typename Member>
auto LibraryRecordMemberType(
    const CompilationUnit& unit, Member Record::* member) -> TypeId {
  return LibraryRecordMembersOf<Record>(unit)[abi::PositionOf(member)];
}

// An array constant of `elements`, each a value of `element`, and a view of
// all of it, as a member of a structure of the library holds a table.
[[nodiscard]] inline auto ViewOf(
    const CompilationUnit& unit, TypeId element,
    std::vector<ConstantValue> elements) -> ConstantValue {
  const TypeId array = MachineArrayOf(unit.types, element, elements.size());
  return ConstantValue{SpanValue{
      .array = unit.constants.Intern(
          ConstantDecl{
              .type = array,
              .value = ConstantValue{
                  ArrayValue{.elements = std::move(elements)}}})}};
}

// A structure of the library composed from a part for each of its members,
// each part named by the member it fills, so it stands where the library
// declares that member and is a value of the type the library declares it at.
//
//   LibraryRecordBuilder<abi::ScopeCallable> entry(unit);
//   entry.Set(&abi::ScopeCallable::name, Name("bump"));
//   entry.Set(&abi::ScopeCallable::entry, body);
//   std::move(entry).Build()  // {"bump", body}
template <typename Record>
class LibraryRecordBuilder {
 public:
  explicit LibraryRecordBuilder(const CompilationUnit& unit) : unit_(&unit) {
    parts_.resize(LibraryRecordMembersOf<Record>(unit).size());
  }

  // The type the library declares `member` at.
  template <typename Member>
  [[nodiscard]] auto TypeOf(Member Record::* member) const -> TypeId {
    return LibraryRecordMemberType(*unit_, member);
  }

  template <typename Member>
  void Set(Member Record::* member, ConstantValue part) {
    std::optional<ConstantValue>& at = parts_[abi::PositionOf(member)];
    if (at.has_value()) {
      throw InternalError(
          "mir: a member of a structure of the library is given twice -- "
          "please report this as a bug");
    }
    at = std::move(part);
  }

  // The structure, once every member is given.
  [[nodiscard]] auto Build() && -> ConstantValue {
    RecordValue record;
    record.parts.reserve(parts_.size());
    for (std::optional<ConstantValue>& part : parts_) {
      if (!part.has_value()) {
        throw InternalError(
            "mir: a structure of the library is composed with a member left "
            "out -- please report this as a bug");
      }
      record.parts.push_back(*std::move(part));
    }
    return ConstantValue{std::move(record)};
  }

  // The structure, once every member is given, as a constant of the unit.
  [[nodiscard]] auto Intern() && -> ConstantId {
    return unit_->constants.Intern(
        ConstantDecl{
            .type = LibraryTypeOf(unit_->types, abi::LibraryDeclOf<Record>()),
            .value = std::move(*this).Build()});
  }

 private:
  const CompilationUnit* unit_;
  std::vector<std::optional<ConstantValue>> parts_;
};

}  // namespace lyra::mir
