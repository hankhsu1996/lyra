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
#include "lyra/lir/type_query.hpp"

namespace lyra::lir {

namespace {

auto AsClassId(const CompilationUnit& unit, TypeId type)
    -> std::optional<ClassId> {
  const auto* object = std::get_if<ObjectType>(&unit.types.Get(type).data);
  return object != nullptr ? std::optional{object->class_id} : std::nullopt;
}

auto IsVoid(const CompilationUnit& unit, TypeId type) -> bool {
  return std::holds_alternative<VoidType>(unit.types.Get(type).data);
}

auto IsPlaceLocal(const Function& fn, const Operand& op) -> bool {
  const auto* use = std::get_if<Use>(&op);
  return use != nullptr && fn.values.Get(use->value).kind == LocalKind::kPlace;
}

// The type of the storage a place names. The base contributes the storage the
// chain starts from: a place local names its own storage, and any other base is
// a value, which names storage only once dereferenced. Each dereference reads
// the reference the chain has reached and names its referent; each member step
// selects a member of the class the chain has reached.
auto PlaceType(
    const CompilationUnit& unit, const Function& fn, const Place& place)
    -> TypeId {
  const std::optional<TypeId> base = OperandType(fn, place.base);
  if (!base) {
    throw InternalError("lir verify: place base has no type");
  }
  const bool opens_with_deref =
      !place.chain.empty() &&
      std::holds_alternative<DerefProjection>(place.chain.front());
  if (!IsPlaceLocal(fn, place.base) && !opens_with_deref) {
    throw InternalError(
        "lir verify: a place over a value base must open with a dereference");
  }

  TypeId current = *base;
  for (const Projection& step : place.chain) {
    std::visit(
        Overloaded{
            [&](const DerefProjection&) {
              const std::optional<TypeId> pointee =
                  Pointee(unit.types, current);
              if (!pointee) {
                throw InternalError(
                    "lir verify: dereference of a non-reference type");
              }
              current = *pointee;
            },
            [&](const MemberProjection& m) {
              const std::optional<ClassId> class_id = AsClassId(unit, current);
              if (!class_id) {
                throw InternalError(
                    "lir verify: member projection on a non-object base");
              }
              const Class& cls = unit.classes.Get(*class_id);
              if (m.member.value >= cls.members.size()) {
                throw InternalError(
                    std::format(
                        "lir verify: member index {} out of range on class "
                        "'{}'",
                        m.member.value, cls.name));
              }
              current = cls.members[m.member.value].type;
            }},
        step);
  }
  return current;
}

void VerifyInstr(
    const CompilationUnit& unit, const Function& fn, const Instr& instr) {
  const TypeId result_type = fn.values.Get(instr.result).type;
  std::visit(
      Overloaded{
          [&](const LoadInstr& load) {
            const TypeId place_type = PlaceType(unit, fn, load.place);
            if (IsAddressOnly(unit.types, place_type)) {
              throw InternalError(
                  "lir verify: load of a place whose storage is only "
                  "addressable");
            }
            if (result_type != place_type) {
              throw InternalError(
                  "lir verify: load result type does not match its place type");
            }
          },
          [&](const StoreInstr& store) {
            const TypeId place_type = PlaceType(unit, fn, store.place);
            if (IsAddressOnly(unit.types, place_type)) {
              throw InternalError(
                  "lir verify: store into a place whose storage is only "
                  "addressable");
            }
            const std::optional<TypeId> value_type =
                OperandType(fn, store.value);
            if (!value_type) {
              throw InternalError("lir verify: store value has no type");
            }
            if (*value_type != place_type) {
              throw InternalError(
                  "lir verify: store value type does not match its place type");
            }
            if (!IsVoid(unit, result_type)) {
              throw InternalError("lir verify: store must yield void");
            }
          },
          [&](const AddrOfInstr& addr) {
            const TypeId place_type = PlaceType(unit, fn, addr.place);
            const std::optional<TypeId> pointee =
                Pointee(unit.types, result_type);
            if (!pointee || *pointee != place_type) {
              throw InternalError(
                  "lir verify: address-of result is not a reference to its "
                  "place type");
            }
          },
          [&](const PointerCastInstr& cast) {
            const std::optional<TypeId> operand_type =
                OperandType(fn, cast.operand);
            if (!operand_type || !Pointee(unit.types, *operand_type)) {
              throw InternalError(
                  "lir verify: pointer cast of a non-reference operand");
            }
            if (!Pointee(unit.types, result_type)) {
              throw InternalError(
                  "lir verify: pointer cast result is not a reference type");
            }
          },
          [](const CallInstr&) {}, [](const AggregateInstr&) {},
          [](const BinaryInstr&) {}, [](const UnaryInstr&) {},
          [](const BoolCastInstr&) {}},
      instr.data);
}

void VerifyFunction(const CompilationUnit& unit, const Function& fn) {
  const bool is_coroutine = IsCoroutine(unit.types, fn.result_type);
  for (const BasicBlock& block : fn.blocks) {
    for (const Instr& instr : block.instrs) {
      VerifyInstr(unit, fn, instr);
    }
    // Only a body whose call protocol is the coroutine one can hand control
    // back to the scheduler; a suspension anywhere else has no one to resume
    // it.
    if (std::holds_alternative<SuspendTerm>(block.terminator.data) &&
        !is_coroutine) {
      throw InternalError(
          "lir verify: a suspension appears in a body whose result type is not "
          "a coroutine");
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
