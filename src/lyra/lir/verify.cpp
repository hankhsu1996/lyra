#include "lyra/lir/verify.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <optional>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lir/class_id.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/type_id.hpp"

namespace lyra::lir {

namespace {

// The class an object-or-pointer-to-object type names, peeling the borrowed
// pointer a receiver arrives through. A type that is neither an object nor a
// pointer to one is not projectable by a member step.
auto AsClassId(const CompilationUnit& unit, TypeId type)
    -> std::optional<ClassId> {
  return std::visit(
      Overloaded{
          [](const ObjectType& o) -> std::optional<ClassId> {
            return o.class_id;
          },
          [&](const PointerType& p) -> std::optional<ClassId> {
            return AsClassId(unit, p.pointee);
          },
          [](const auto&) -> std::optional<ClassId> { return std::nullopt; }},
      unit.types.Get(type).data);
}

// A reference-like type crosses as one opaque handle: distinct reference types
// (a borrowed scope pointer, a borrowed unit pointer) share a representation,
// so a store between them is a reinterpretation the MIR pointer cast already
// justified, not a type error.
auto IsReference(const CompilationUnit& unit, TypeId type) -> bool {
  return std::visit(
      Overloaded{
          [](const PointerType&) { return true; },
          [](const RefType&) { return true; },
          [](const ManagedRefType&) { return true; },
          [](const auto&) { return false; }},
      unit.types.Get(type).data);
}

auto IsVoid(const CompilationUnit& unit, TypeId type) -> bool {
  return std::holds_alternative<VoidType>(unit.types.Get(type).data);
}

auto OperandType(const Function& fn, const Operand& op)
    -> std::optional<TypeId> {
  return std::visit(
      Overloaded{
          [&](const Use& u) -> std::optional<TypeId> {
            return fn.values.Get(u.value).type;
          },
          [](const IntConst& c) -> std::optional<TypeId> { return c.type; },
          [](const StrConst& c) -> std::optional<TypeId> { return c.type; },
          [](const FuncRef&) -> std::optional<TypeId> { return std::nullopt; }},
      op);
}

// The type a place's projection chain arrives at: the base's storage type,
// projected one step at a time. Each member step resolves the base to a class
// and selects the member's declared type.
auto PlaceType(
    const CompilationUnit& unit, const Function& fn, const Place& place)
    -> TypeId {
  const std::optional<TypeId> base = OperandType(fn, place.base);
  if (!base) {
    throw InternalError("lir verify: place base has no storage type");
  }
  TypeId current = *base;
  for (const Projection& step : place.chain) {
    std::visit(
        Overloaded{[&](const MemberProjection& m) {
          const std::optional<ClassId> class_id = AsClassId(unit, current);
          if (!class_id) {
            throw InternalError(
                "lir verify: member projection on a non-object base");
          }
          const Class& cls = unit.classes.Get(*class_id);
          if (m.member.value >= cls.members.size()) {
            throw InternalError(
                std::format(
                    "lir verify: member index {} out of range on class '{}'",
                    m.member.value, cls.name));
          }
          current = cls.members[m.member.value].type;
        }},
        step);
  }
  return current;
}

void VerifyFunction(const CompilationUnit& unit, const Function& fn) {
  for (const BasicBlock& block : fn.blocks) {
    for (const Instr& instr : block.instrs) {
      const TypeId result_type = fn.values.Get(instr.result).type;
      std::visit(
          Overloaded{
              [&](const LoadInstr& load) {
                const TypeId place_type = PlaceType(unit, fn, load.place);
                if (result_type != place_type) {
                  throw InternalError(
                      "lir verify: load result type does not match its place "
                      "type");
                }
              },
              [&](const StoreInstr& store) {
                const TypeId place_type = PlaceType(unit, fn, store.place);
                const std::optional<TypeId> value_type =
                    OperandType(fn, store.value);
                if (!value_type) {
                  throw InternalError("lir verify: store value has no type");
                }
                const bool compatible = *value_type == place_type ||
                                        (IsReference(unit, *value_type) &&
                                         IsReference(unit, place_type));
                if (!compatible) {
                  throw InternalError(
                      "lir verify: store value type is not compatible with its "
                      "place type");
                }
                if (!IsVoid(unit, result_type)) {
                  throw InternalError("lir verify: store must yield void");
                }
              },
              [](const CallInstr&) {}, [](const AggregateInstr&) {}},
          instr.data);
    }
  }
}

}  // namespace

void Verify(const CompilationUnit& unit) {
  for (std::size_t i = 0; i < unit.classes.size(); ++i) {
    const Class& cls = unit.classes.Get(ClassId{static_cast<std::uint32_t>(i)});
    VerifyFunction(unit, cls.constructor);
    for (const Function& method : cls.methods) {
      VerifyFunction(unit, method);
    }
  }
}

}  // namespace lyra::lir
