#include "lyra/lir/class_query.hpp"

#include <cstdint>
#include <optional>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/type.hpp"

namespace lyra::lir {

namespace {

// The class this one extends, when that base is compiled here. A base another
// unit or the runtime library defines ends the walk: what it declares is not
// visible in this unit's classes.
auto IntraUnitBaseOf(const CompilationUnit& unit, ClassId cls)
    -> std::optional<ClassId> {
  const std::optional<Base>& base = unit.classes.Get(cls).base;
  if (!base) {
    return std::nullopt;
  }
  const auto* intra = std::get_if<IntraUnitBase>(&*base);
  if (intra == nullptr) {
    return std::nullopt;
  }
  return intra->class_id;
}

// The class a type names, when it names one at all. Only a class extends
// another, so only a class carries members some other declaration declares.
auto ClassOf(const CompilationUnit& unit, TypeId type)
    -> std::optional<ClassId> {
  const auto* object = unit.types.Get(type).As<ObjectType>();
  if (object == nullptr) {
    return std::nullopt;
  }
  return object->class_id;
}

// How many members a declaration's bases contribute, which is where its own
// first member sits. A declaration with no base -- a closure, another unit's
// object, a class extending nothing this unit compiles -- contributes none, so
// its own members start at zero.
auto InheritedMemberCount(const CompilationUnit& unit, TypeId declaration)
    -> std::uint32_t {
  const std::optional<ClassId> cls = ClassOf(unit, declaration);
  if (!cls) {
    return 0;
  }
  std::uint32_t count = 0;
  for (std::optional<ClassId> base = IntraUnitBaseOf(unit, *cls); base;
       base = IntraUnitBaseOf(unit, *base)) {
    count += static_cast<std::uint32_t>(unit.classes.Get(*base).members.size());
  }
  return count;
}

// How many behaviors a class's bases introduce, which is where its own first
// one sits. Absent where the lineage leaves this unit: extending nothing and
// extending a class the runtime library defines both start at zero, since
// neither carries a behavior the source language dispatches through, but
// extending another unit's class starts after behaviors this unit cannot count.
//
// The storage walk above answers the same lineage question and answers a
// departing lineage with zero rather than with nothing, which is not an
// inconsistency: a unit publishes the members of its objects at the positions
// they sit in, so a member of another unit's class is reached through what that
// unit promised. Nothing is published about which behaviors a class carries, so
// there is no second route to the count and the only honest answer is that
// there is none.
auto InheritedDispatchCount(const CompilationUnit& unit, ClassId cls)
    -> std::optional<std::uint32_t> {
  const std::optional<Base>& base = unit.classes.Get(cls).base;
  if (!base.has_value()) {
    return 0;
  }
  return std::visit(
      Overloaded{
          [&](const IntraUnitBase& intra) -> std::optional<std::uint32_t> {
            const std::optional<std::uint32_t> inherited =
                InheritedDispatchCount(unit, intra.class_id);
            if (!inherited.has_value()) {
              return std::nullopt;
            }
            return *inherited +
                   static_cast<std::uint32_t>(
                       unit.classes.Get(intra.class_id).introduces.size());
          },
          [](const CrossUnitBase&) -> std::optional<std::uint32_t> {
            return std::nullopt;
          },
          [](const RuntimeBase&) -> std::optional<std::uint32_t> { return 0; }},
      *base);
}

}  // namespace

auto IsObjectTreeNode(const Class& cls) -> bool {
  if (!cls.base.has_value()) {
    return false;
  }
  return std::visit(
      Overloaded{
          [](const RuntimeBase&) { return true; },
          [](const IntraUnitBase&) { return false; },
          [](const CrossUnitBase&) { return false; }},
      *cls.base);
}

auto StorageMembers(const CompilationUnit& unit, ClassId cls)
    -> std::vector<Member> {
  std::vector<Member> members;
  if (const std::optional<ClassId> base = IntraUnitBaseOf(unit, cls)) {
    members = StorageMembers(unit, *base);
  }
  const std::vector<Member>& own = unit.classes.Get(cls).members;
  members.insert(members.end(), own.begin(), own.end());
  return members;
}

auto MemberPosition(const CompilationUnit& unit, MemberRef member)
    -> std::uint32_t {
  return InheritedMemberCount(unit, member.declared_by) + member.slot.value;
}

auto DispatchTable(const CompilationUnit& unit, ClassId cls)
    -> std::optional<std::vector<std::optional<FunctionId>>> {
  const std::optional<std::uint32_t> inherited =
      InheritedDispatchCount(unit, cls);
  if (!inherited.has_value()) {
    return std::nullopt;
  }
  std::vector<std::optional<FunctionId>> table;
  if (const std::optional<ClassId> base = IntraUnitBaseOf(unit, cls)) {
    table = *DispatchTable(unit, *base);
  }
  const Class& own = unit.classes.Get(cls);
  table.insert(table.end(), own.introduces.begin(), own.introduces.end());
  for (const DispatchOverride& taken : own.overrides) {
    const std::optional<std::uint32_t> position =
        DispatchPosition(unit, taken.method);
    if (!position.has_value() || *position >= table.size()) {
      throw InternalError(
          "lir: a class takes over a behavior its lineage does not carry");
    }
    table[*position] = taken.body;
  }
  return table;
}

auto DispatchPosition(const CompilationUnit& unit, DispatchRef method)
    -> std::optional<std::uint32_t> {
  const std::optional<std::uint32_t> inherited =
      InheritedDispatchCount(unit, method.introduced_by);
  if (!inherited.has_value()) {
    return std::nullopt;
  }
  return *inherited + method.ordinal.value;
}

auto CarriesMembersOf(
    const CompilationUnit& unit, TypeId type, TypeId declaration) -> bool {
  // A declaration that extends nothing this unit compiles -- a closure,
  // another unit's object -- carries only what it declares itself, and the walk
  // below has no lineage to follow for it, so equality is the whole answer.
  if (type == declaration) {
    return true;
  }
  const std::optional<ClassId> declared = ClassOf(unit, declaration);
  if (!declared) {
    return false;
  }
  for (std::optional<ClassId> current = ClassOf(unit, type); current;
       current = IntraUnitBaseOf(unit, *current)) {
    if (*current == *declared) {
      return true;
    }
  }
  return false;
}

}  // namespace lyra::lir
