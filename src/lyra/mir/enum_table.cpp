#include "lyra/mir/enum_table.hpp"

#include <bit>
#include <cstdint>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/mir/constant_id.hpp"
#include "lyra/mir/constant_value.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/library_record_builder.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/value/enumeration.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::mir {

namespace {

// The element type of the view the library declares at `member`.
auto ViewedElement(const CompilationUnit& unit, TypeId member) -> TypeId {
  return unit.types.Get(member).Get<MachineSpanType>().element;
}

}  // namespace

auto BuildEnumMemberTable(
    const CompilationUnit& unit, Builder block, TypeId enumeration) -> ExprId {
  using lyra::value::Enumeration;
  const auto* declared = unit.types.Get(enumeration).As<EnumType>();
  if (declared == nullptr) {
    throw InternalError("mir: only an enumeration declares members");
  }
  const bool four_state =
      declared->base.state_kind == IntegralStateKind::kFourState;
  std::vector<lyra::value::ConstPlanes> member_planes;
  std::vector<ConstantValue> names;
  member_planes.reserve(declared->members.size());
  names.reserve(declared->members.size());
  for (const EnumMember& member : declared->members) {
    member_planes.push_back(
        lyra::value::ConstPlanes{
            .value = member.value.value_words,
            .unknown = member.value.state_words});
    names.push_back(ConstantValue{CStringValue{.text = member.name}});
  }
  std::vector<ConstantValue> words;
  for (const std::uint64_t word : lyra::value::EnumerationPlanes(
           declared->base.bit_width, four_state, member_planes)) {
    words.push_back(
        ConstantValue{
            MachineIntValue{.value = std::bit_cast<std::int64_t>(word)}});
  }
  LibraryRecordBuilder<Enumeration> table(unit);
  table.Set(
      &Enumeration::planes,
      ViewOf(
          unit, ViewedElement(unit, table.TypeOf(&Enumeration::planes)),
          std::move(words)));
  table.Set(
      &Enumeration::names,
      ViewOf(
          unit, ViewedElement(unit, table.TypeOf(&Enumeration::names)),
          std::move(names)));
  table.Set(
      &Enumeration::width,
      ConstantValue{MachineIntValue{
          .value = static_cast<std::int64_t>(declared->base.bit_width)}});
  table.Set(
      &Enumeration::four_state,
      ConstantValue{MachineIntValue{.value = four_state ? 1 : 0}});
  const ConstantId held = std::move(table).Intern();
  return block.Add(
      Expr{
          .data = ReferenceExpr{.target = ConstantRef{.constant = held}},
          .type = unit.builtins.enumeration});
}

}  // namespace lyra::mir
