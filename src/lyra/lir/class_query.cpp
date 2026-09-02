#include "lyra/lir/class_query.hpp"

#include <cstdint>
#include <optional>
#include <variant>
#include <vector>

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
