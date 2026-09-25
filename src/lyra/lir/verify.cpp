#include "lyra/lir/verify.hpp"

#include <cstddef>
#include <format>
#include <optional>
#include <variant>
#include <vector>

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

// What the storage at a place holds, for a place a reference is taken over.
// LRM 13.5.2 admits a variable, a class property, a member of an unpacked
// structure and an element of an unpacked array, and bars a net -- so such a
// place reaches either a subscribable variable, which holds the value it
// represents, or storage nothing wraps, which holds itself. No other wrapper
// can stand there, which is why none is named.
auto LentValues(const CompilationUnit& unit, TypeId storage) -> TypeId {
  const Type& type = unit.types.Get(storage);
  if (const auto* observable = type.As<ObservableType>()) {
    return observable->value;
  }
  return storage;
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
                  std::format(
                      "lir verify: load result type does not match its place "
                      "type (result Type[{}], place Type[{}])",
                      result_type.value, place_type.value));
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
                  std::format(
                      "lir verify: store value type does not match its place "
                      "type (value Type[{}], place Type[{}])",
                      value_type->value, place_type.value));
            }
            if (!IsVoid(unit, result_type)) {
              throw InternalError("lir verify: store must yield void");
            }
          },
          [&](const AddrOfInstr& addr) {
            const TypeId place_type = PlaceType(unit, fn, addr.place);
            const Type& result = unit.types.Get(result_type);
            const std::optional<TypeId> pointee = result.Pointee();
            // A reference states the values the storage it names holds rather
            // than that storage's own type: what it is lent may be a
            // subscribable variable or storage nothing subscribes to, and the
            // body holding it is lowered once for both (LRM 13.5.2). Every
            // other address names the storage it points at.
            const TypeId named = result.Is<RefType>()
                                     ? LentValues(unit, place_type)
                                     : place_type;
            if (!pointee || *pointee != named) {
              throw InternalError(
                  "lir verify: address-of result does not name what its place "
                  "holds");
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
          [](const BinaryInstr&) {}, [](const UnaryInstr&) {},
          // Where it may stand is a property of the block rather than of the
          // instruction, so it is held where the blocks are walked.
          [](const ReceiveDepartureInstr&) {}},
      instr.data);
}

// A landing is a block one departing call names and that opens by receiving
// what arrived. Neither half stands without the other: a body reaches a landing
// no other way, and a departure has nowhere else to be read -- so a block that
// opens this way and is named by nothing would never run, and one that is named
// and does not would run with the departure unread. It is one call's alone,
// because what a target owes on the way out of that call is that call's.
void VerifyLandings(const Function& fn) {
  std::vector<std::size_t> named(fn.blocks.size(), 0);
  for (const BasicBlock& block : fn.blocks) {
    if (const auto* call =
            std::get_if<DepartingCallInstr>(&block.terminator.data)) {
      ++named[call->landing.value];
    }
  }
  for (std::size_t index = 0; index < fn.blocks.size(); ++index) {
    const std::vector<Instr>& instrs = fn.blocks[index].instrs;
    for (std::size_t at = 0; at < instrs.size(); ++at) {
      if (!std::holds_alternative<ReceiveDepartureInstr>(instrs[at].data)) {
        continue;
      }
      if (at != 0) {
        throw InternalError(
            "lir verify: a departure is received somewhere other than at the "
            "start of a landing");
      }
    }
    const bool receives =
        !instrs.empty() &&
        std::holds_alternative<ReceiveDepartureInstr>(instrs[0].data);
    if (receives != (named[index] != 0)) {
      throw InternalError(
          receives ? "lir verify: a landing no departing call names"
                   : "lir verify: a departing call names a block that receives "
                     "no departure");
    }
    if (named[index] > 1) {
      throw InternalError(
          "lir verify: a landing more than one departing call names");
    }
  }
}

void VerifyFunction(const CompilationUnit& unit, const Function& fn) {
  const bool is_coroutine = unit.types.Get(fn.result_type).Is<CoroutineType>();
  for (const BasicBlock& block : fn.blocks) {
    for (const Instr& instr : block.instrs) {
      VerifyInstr(unit, fn, instr);
    }
    // Only a body whose call protocol is the coroutine one can hand control
    // back to the scheduler; a suspension anywhere else has no one to resume
    // it. An abandonment is reached only from a suspension, so it is bounded
    // by the same fact.
    const bool parks =
        std::holds_alternative<SuspendTerm>(block.terminator.data) ||
        std::holds_alternative<AbandonTerm>(block.terminator.data);
    if (parks && !is_coroutine) {
      throw InternalError(
          "lir verify: a suspension appears in a body whose result type is not "
          "a coroutine");
    }
  }
  VerifyLandings(fn);
}

}  // namespace

void Verify(const CompilationUnit& unit) {
  for (const Function& fn : unit.functions) {
    VerifyFunction(unit, fn);
  }
}

}  // namespace lyra::lir
