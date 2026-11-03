#include "lyra/mir/constant_value.hpp"

#include <cstddef>
#include <optional>
#include <unordered_set>
#include <variant>
#include <vector>

#include "lyra/base/hash.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::mir {

// A part holds a value of its own and is compared as one, which is defined
// only once the value it holds is complete.
auto RecordValue::operator==(const RecordValue&) const -> bool = default;
auto ArrayValue::operator==(const ArrayValue&) const -> bool = default;

namespace {

void HashTarget(std::size_t& seed, const ReferenceTarget& target) {
  base::HashField(seed, target.index());
  std::visit(
      Overloaded{
          [&](const LocalRef& r) { base::HashField(seed, r.var.value); },
          [&](const DefinitionRef& r) { base::HashField(seed, r.of.value); },
          [&](const ConstantRef& r) {
            base::HashField(seed, r.constant.value);
          },
          [&](const StaticPropertyRef& r) {
            base::HashField(seed, r.owner.value);
            base::HashField(seed, r.prop.value);
          },
          [&](const StaticVariableRef& r) {
            base::HashField(seed, r.variable.value);
          },
          [&](const FunctionRef& r) {
            base::HashField(seed, r.body.owner.value);
            base::HashField(seed, r.body.slot.value);
          },
          [&](const DispatchEntryRef& r) {
            base::HashField(seed, r.body.owner.value);
            base::HashField(seed, r.body.slot.value);
          },
          [&](const DestructorRef& r) { base::HashField(seed, r.of.value); },
          [&](const DispatchTableRef& r) { base::HashField(seed, r.of.value); },
          [&](const InterfaceTableRef& r) {
            base::HashField(seed, r.of.value);
            base::HashField(seed, r.iface.value);
          },
          [&](const ExternalObjectEntryRef& r) {
            base::HashField(seed, r.unit_name);
            base::HashField(seed, r.body_name);
            base::HashField(seed, r.block.transform([](BlockBodyOrdinal b) {
              return b.value;
            }));
          }},
      target);
}

}  // namespace

void HashConstantValue(std::size_t& seed, const ConstantValue& value) {
  base::HashField(seed, value.value.index());
  std::visit(
      Overloaded{
          [&](const IntegralValue& v) { HashIntegralConstant(seed, v.planes); },
          [&](const MachineIntValue& v) { base::HashField(seed, v.value); },
          [](const NullValue&) {},
          [&](const CStringValue& v) { base::HashField(seed, v.text); },
          [&](const PointerValue& v) {
            HashTarget(seed, v.to);
            base::HashField(seed, v.type.value);
          },
          [&](const RecordValue& v) {
            base::HashField(seed, v.parts.size());
            for (const ConstantValue& part : v.parts) {
              HashConstantValue(seed, part);
            }
          },
          [&](const ArrayValue& v) {
            base::HashField(seed, v.elements.size());
            for (const ConstantValue& element : v.elements) {
              HashConstantValue(seed, element);
            }
          },
          [&](const SpanValue& v) { base::HashField(seed, v.array.value); }},
      value.value);
}

auto ConstantDecl::Hash::operator()(const ConstantDecl& decl) const
    -> std::size_t {
  std::size_t seed = 0;
  base::HashField(seed, decl.type.value);
  HashConstantValue(seed, decl.value);
  return seed;
}

auto PointsAtData(const ReferenceTarget& to) -> bool {
  return std::visit(
      Overloaded{
          [](const LocalRef&) -> bool {
            throw InternalError(
                "mir: a constant holds the address of a local -- please report "
                "this as a bug");
          },
          [](const ConstantRef&) { return true; },
          [](const DefinitionRef&) { return true; },
          [](const StaticPropertyRef&) { return true; },
          [](const StaticVariableRef&) { return true; },
          [](const FunctionRef&) { return false; },
          [](const DispatchEntryRef&) { return false; },
          [](const DestructorRef&) { return false; },
          [](const DispatchTableRef&) { return false; },
          [](const InterfaceTableRef&) { return false; },
          [](const ExternalObjectEntryRef&) { return false; }},
      to);
}

auto OwnPointerType(const TypePool& types, const PointerValue& value)
    -> TypeId {
  if (!PointsAtData(value.to)) {
    return value.type;
  }
  return types.Intern(
      Type{PointerType{
          .pointee = value.type,
          .ownership = PointerOwnership::kBorrowed,
          .mutability = Mutability::kReadOnly}});
}

void ForEachNameReached(
    const ConstantPool& constants, const ConstantValue& value,
    const std::function<void(const ReferenceTarget&)>& visit) {
  std::unordered_set<std::uint32_t> reached;
  std::vector<const ConstantValue*> pending{&value};
  const auto reach = [&](ConstantId entry) {
    if (reached.insert(entry.value).second) {
      pending.push_back(&constants.Get(entry).value);
    }
  };
  while (!pending.empty()) {
    const ConstantValue* at = pending.back();
    pending.pop_back();
    std::visit(
        Overloaded{
            [&](const PointerValue& v) {
              visit(v.to);
              if (const auto* named = std::get_if<ConstantRef>(&v.to)) {
                reach(named->constant);
              }
            },
            [&](const RecordValue& v) {
              for (const ConstantValue& part : v.parts) {
                pending.push_back(&part);
              }
            },
            [&](const ArrayValue& v) {
              for (const ConstantValue& element : v.elements) {
                pending.push_back(&element);
              }
            },
            [&](const SpanValue& v) { reach(v.array); },
            [](const IntegralValue&) {}, [](const MachineIntValue&) {},
            [](const NullValue&) {}, [](const CStringValue&) {}},
        at->value);
  }
}

auto IntegralPlanesNamedBy(const ConstantPool& constants, const ExprData& data)
    -> const IntegralConstant* {
  const auto* reference = std::get_if<ReferenceExpr>(&data);
  const auto* named = reference == nullptr
                          ? nullptr
                          : std::get_if<ConstantRef>(&reference->target);
  if (named == nullptr) {
    return nullptr;
  }
  const auto* integral =
      std::get_if<IntegralValue>(&constants.Get(named->constant).value.value);
  return integral == nullptr ? nullptr : &integral->planes;
}

}  // namespace lyra::mir
