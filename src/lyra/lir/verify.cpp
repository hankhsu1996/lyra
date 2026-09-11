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
          // Between two packed values a cast only renames what the program
          // holds the bits to be, so the two must structure their bits alike:
          // where they do not, the reshape meant to precede this is missing
          // and the value silently changes width.
          [&](const CastInstr& cast) {
            const std::optional<TypeId> operand_type =
                OperandType(fn, cast.operand);
            if (!operand_type) {
              throw InternalError("lir verify: cast operand has no type");
            }
            const Type& from = unit.types.Get(*operand_type);
            const Type& to = unit.types.Get(result_type);
            if (from.IsIntegralPacked() && to.IsIntegralPacked() &&
                from.PackedShape() != to.PackedShape()) {
              throw InternalError(
                  "lir verify: cast changes its value's representation");
            }
          },
          [](const CallInstr&) {}, [](const ProductInstr&) {},
          [](const ArrayInstr&) {}, [](const UnionInstr&) {},
          [](const AggregateExtractInstr&) {},
          [](const AggregateUpdateInstr&) {}, [](const TagTestInstr&) {},
          [](const BinaryInstr&) {}, [](const UnaryInstr&) {}},
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
