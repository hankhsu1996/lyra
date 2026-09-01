#include "lyra/lir/verify.hpp"

#include <optional>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/place_query.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/type_id.hpp"

namespace lyra::lir {

namespace {

auto IsVoid(const CompilationUnit& unit, TypeId type) -> bool {
  return unit.types.Get(type).Is<VoidType>();
}

void VerifyInstr(
    const CompilationUnit& unit, const Function& fn, const Instr& instr) {
  const TypeId result_type = fn.values.Get(instr.result).type;
  std::visit(
      Overloaded{
          [&](const LoadInstr& load) {
            const TypeId place_type = PlaceType(unit, fn, load.place);
            if (unit.types.Get(place_type).IsAddressOnly()) {
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
            if (unit.types.Get(place_type).IsAddressOnly()) {
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
                unit.types.Get(result_type).Pointee();
            if (!pointee || *pointee != place_type) {
              throw InternalError(
                  "lir verify: address-of result is not a reference to its "
                  "place type");
            }
          },
          [&](const PointerCastInstr& cast) {
            const std::optional<TypeId> operand_type =
                OperandType(fn, cast.operand);
            if (!operand_type || !unit.types.Get(*operand_type).Pointee()) {
              throw InternalError(
                  "lir verify: pointer cast of a non-reference operand");
            }
            if (!unit.types.Get(result_type).Pointee()) {
              throw InternalError(
                  "lir verify: pointer cast result is not a reference type");
            }
          },
          [&](const IntCastInstr& cast) {
            const std::optional<TypeId> operand_type =
                OperandType(fn, cast.operand);
            const auto is_machine_int = [&](TypeId type) {
              return unit.types.Get(type).Is<MachineIntType>();
            };
            if (!operand_type || !is_machine_int(*operand_type)) {
              throw InternalError(
                  "lir verify: integer cast of a non-machine-integer operand");
            }
            if (!is_machine_int(result_type)) {
              throw InternalError(
                  "lir verify: integer cast result is not a machine integer");
            }
          },
          // A value cast names a type; it never reshapes. Both sides must
          // structure their bits identically, or the reshape that was meant to
          // precede it is missing and the value silently changes width.
          [&](const ValueCastInstr& cast) {
            const std::optional<TypeId> operand_type =
                OperandType(fn, cast.operand);
            if (!operand_type ||
                !unit.types.Get(*operand_type).IsIntegralPacked() ||
                !unit.types.Get(result_type).IsIntegralPacked()) {
              throw InternalError(
                  "lir verify: value cast between types that are not both "
                  "integral");
            }
            if (unit.types.Get(*operand_type).PackedShape() !=
                unit.types.Get(result_type).PackedShape()) {
              throw InternalError(
                  "lir verify: value cast changes its value's representation");
            }
          },
          [](const CallInstr&) {}, [](const ProductInstr&) {},
          [](const ArrayInstr&) {}, [](const AggregateExtractInstr&) {},
          [](const AggregateUpdateInstr&) {}, [](const BinaryInstr&) {},
          [](const UnaryInstr&) {}, [](const BoolCastInstr&) {}},
      instr.data);
}

void VerifyFunction(const CompilationUnit& unit, const Function& fn) {
  const bool is_coroutine = unit.types.Get(fn.result_type).Is<CoroutineType>();
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
  for (const Function& fn : unit.functions) {
    VerifyFunction(unit, fn);
  }
}

}  // namespace lyra::lir
