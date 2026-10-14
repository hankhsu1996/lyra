#include "lyra/lir/verify.hpp"

#include <cstddef>
#include <format>
#include <optional>
#include <span>
#include <string_view>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/failure_context.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/place_query.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/support/value_domain.hpp"

namespace lyra::lir {

namespace {

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

// A part reached by value projection is a view of its whole -- a bit of a
// packed value, a member of a union. A part that is
// storage of its own (LRM 7.2, 7.4, 7.8, 7.10) is a step of a place instead,
// because a write has to land in it and a value built out of it is a copy no
// write reaches.
void RequireViewedPart(
    const CompilationUnit& unit, const Function& fn, const Operand& aggregate) {
  const std::optional<TypeId> type = OperandType(fn, aggregate);
  if (!type) {
    throw InternalError("lir verify: a projected aggregate has no type");
  }
  const std::optional<Holding> held = unit.types.Get(*type).HeldAs();
  const auto* domain =
      held.has_value() ? std::get_if<support::ValueDomain>(&*held) : nullptr;
  if (domain != nullptr && support::PartsAreStorage(*domain)) {
    throw InternalError(
        "lir verify: a part that is storage of its own is projected by value "
        "rather than reached as a step of a place");
  }
}

// Bits are written within an integral value, through parts that are each bits
// of one too and each start at a value of an integral type, and what is
// written is a value of the last part's type.
void RequireBitsStoreWellFormed(
    const CompilationUnit& unit, const Function& fn, TypeId whole,
    std::span<const BitsPart> parts, const Operand& bits) {
  const auto is_integral = [&](TypeId type) {
    return unit.types.Get(type).Is<IntegralType>();
  };
  if (!is_integral(whole)) {
    throw InternalError(
        "lir verify: bits are written into a value that is not integral");
  }
  if (parts.empty()) {
    throw InternalError("lir verify: a write of bits names no part to write");
  }
  for (const BitsPart& part : parts) {
    const std::optional<TypeId> start = OperandType(fn, part.start);
    if (!is_integral(part.type) || !start.has_value() || !is_integral(*start)) {
      throw InternalError(
          "lir verify: a part of an integral value is stated at a type, or "
          "starts at a value, that is not integral");
    }
  }
  if (OperandType(fn, bits) != parts.back().type) {
    throw InternalError(
        "lir verify: the bits written are not a value of the type the part "
        "written is stated at");
  }
}

// What a call's target requires of the operands the call states. A write of
// designated bits takes the designation and the bits.
void RequireCallWellFormed(
    const CompilationUnit& unit, const Function& fn, const CallTarget& target,
    std::span<const Operand> args) {
  const auto* bits = std::get_if<DesignatedBitsTarget>(&target);
  if (bits == nullptr) {
    return;
  }
  if (args.size() != 2) {
    throw InternalError(
        "lir verify: a write of designated bits takes the designation and "
        "the bits");
  }
  RequireBitsStoreWellFormed(unit, fn, bits->value, bits->parts, args.back());
}

// The values that name storage of dynamic duration just allocated, by value
// id: storage holding no value yet, as a frame slot is before its first.
auto AllocatedStorage(const Function& fn) -> std::vector<bool> {
  std::vector<bool> allocated(fn.values.size(), false);
  for (const BasicBlock& block : fn.blocks) {
    for (const Instr& instr : block.instrs) {
      const auto* assign = std::get_if<AssignInstr>(&instr.data);
      const auto* call =
          assign != nullptr ? std::get_if<CallInstr>(&assign->value) : nullptr;
      const auto* storage =
          call != nullptr ? std::get_if<DynamicStorageTarget>(&call->target)
                          : nullptr;
      const auto* defined = assign != nullptr
                                ? std::get_if<Transient>(&assign->destination)
                                : nullptr;
      if (storage == nullptr || defined == nullptr) {
        continue;
      }
      switch (storage->op) {
        case DynamicStorageTarget::Op::kAllocate:
          allocated.at(defined->value.value) = true;
          break;
        case DynamicStorageTarget::Op::kDeallocate:
          break;
      }
    }
  }
  return allocated;
}

// Whether `place` is storage holding no value until it is initialized: a frame
// slot, or what storage just allocated holds.
auto HoldsNothingYet(
    const Function& fn, const std::vector<bool>& allocated, const Place& place)
    -> bool {
  if (place.chain.empty()) {
    return IsPlaceLocal(fn, place.base);
  }
  const auto* base = std::get_if<Use>(&place.base);
  return base != nullptr && allocated.at(base->value.value) &&
         place.chain.size() == 1 &&
         std::holds_alternative<DerefProjection>(place.chain.front());
}

// The type of the value an assignment puts where `destination` says.
auto DestinationType(
    const CompilationUnit& unit, const Function& fn,
    const Destination& destination) -> TypeId {
  return std::visit(
      Overloaded{
          [&](const Transient& defined) {
            return fn.values.Get(defined.value).type;
          },
          [&](const Place& place) { return PlaceType(unit, fn, place); }},
      destination);
}

void VerifyRvalue(
    const CompilationUnit& unit, const Function& fn, const Rvalue& value,
    TypeId result_type);

void VerifyInstr(
    const CompilationUnit& unit, const Function& fn,
    const std::vector<bool>& allocated, const Instr& instr) {
  // A store puts a value of the place's type into storage that can hold one.
  const auto write_into = [&](const Place& place, const Operand& value,
                              std::string_view what) {
    const TypeId place_type = PlaceType(unit, fn, place);
    if (unit.types.Get(place_type).IsAddressOnly()) {
      throw InternalError(
          std::format(
              "lir verify: {} into a place whose storage is only addressable",
              what));
    }
    const TypeId value_type = OperandType(fn, value);
    if (value_type != place_type) {
      throw InternalError(
          std::format(
              "lir verify: {} value type does not match its place type "
              "(value Type[{}], place Type[{}])",
              what, value_type.value, place_type.value));
    }
  };
  std::visit(
      Overloaded{
          // A value is put into storage only where it is made, and only into
          // storage holding nothing yet, which it initializes.
          [&](const AssignInstr& assign) {
            std::visit(
                Overloaded{
                    [](const Transient&) {},
                    [&](const Place& place) {
                      if (!MakesValue(assign.value) ||
                          !HoldsNothingYet(fn, allocated, place)) {
                        throw InternalError(
                            "lir verify: a value is put into storage where "
                            "nothing is made, or where the storage already "
                            "holds one");
                      }
                      if (unit.types.Get(PlaceType(unit, fn, place))
                              .IsAddressOnly()) {
                        throw InternalError(
                            "lir verify: a value is put into storage that is "
                            "only addressable");
                      }
                    }},
                assign.destination);
            VerifyRvalue(
                unit, fn, assign.value,
                DestinationType(unit, fn, assign.destination));
          },
          [&](const StoreInstr& store) {
            write_into(store.place, store.value, "store");
          },
          [&](const BitsStoreInstr& store) {
            RequireBitsStoreWellFormed(
                unit, fn, PlaceType(unit, fn, store.place), store.parts,
                store.bits);
          },
          [&](const StorageInstr& storage) {
            // A made value's bytes come into use with the instruction that
            // makes it, so only a slot is stated to.
            switch (storage.op) {
              case StorageInstr::Op::kLive:
                if (!fn.values.Get(storage.of).NamesStorage()) {
                  throw InternalError(
                      "lir verify: storage is stated to come into use for a "
                      "value that is not a slot");
                }
                break;
              case StorageInstr::Op::kDead:
                break;
            }
          },
          [](const CloseVariablesInstr&) {}},
      instr.data);
}

void VerifyRvalue(
    const CompilationUnit& unit, const Function& fn, const Rvalue& value,
    TypeId result_type) {
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
          [&](const AddrOfInstr& addr) {
            const TypeId place_type = PlaceType(unit, fn, addr.place);
            const Type& result = unit.types.Get(result_type);
            const std::optional<TypeId> pointee = result.Pointee();
            // A reference states the values the storage it names holds rather
            // than that storage's own type: what it is lent may be a
            // subscribable variable or storage nothing subscribes to, and the
            // body holding it is lowered once for both (LRM 13.5.2). Every
            // other address names the storage it points at.
            const TypeId named = result.Is<VariableRefType>()
                                     ? LentValues(unit, place_type)
                                     : place_type;
            if (!pointee || *pointee != named) {
              throw InternalError(
                  "lir verify: address-of result does not name what its place "
                  "holds");
            }
          },
          // Between two integral values a cast only renames what the program
          // holds the bits to be, so the two must be one type: where they are
          // not, the reshape meant to precede this is missing and the value
          // silently changes width.
          [&](const CastInstr& cast) {
            const std::optional<TypeId> operand_type =
                OperandType(fn, cast.operand);
            if (!operand_type) {
              throw InternalError("lir verify: cast operand has no type");
            }
            const auto* from = unit.types.Get(*operand_type).As<IntegralType>();
            const auto* to = unit.types.Get(result_type).As<IntegralType>();
            if (from != nullptr && to != nullptr && *from != *to) {
              throw InternalError(
                  "lir verify: cast changes its value's representation");
            }
          },
          [&](const AggregateExtractInstr& extract) {
            RequireViewedPart(unit, fn, extract.aggregate);
          },
          [&](const AggregateUpdateInstr& update) {
            RequireViewedPart(unit, fn, update.aggregate);
          },
          [](const HandleCastInstr&) {}, [](const DynamicCastInstr&) {},
          [&](const CallInstr& call) {
            RequireCallWellFormed(unit, fn, call.target, call.args);
          },
          [](const TupleInstr&) {},
          [&](const ClosureInstr& built) {
            const auto* closure = unit.types.Get(result_type).As<ClosureType>();
            if (closure == nullptr ||
                built.captures.size() !=
                    unit.closures.Get(closure->closure_id).captures.size()) {
              throw InternalError(
                  "lir verify: a closure is built of something other than "
                  "every capture its declaration lists");
            }
          },
          [](const ArrayInstr&) {}, [](const BinaryInstr&) {},
          [](const UnaryInstr&) {},
          // Where it may stand is a property of the block rather than of the
          // instruction, so it is held where the blocks are walked.
          [](const ReceiveDepartureInstr&) {}, [](const OpenVariablesInstr&) {},
          [&](const UseInstr& use) {
            if (OperandType(fn, use.operand) != result_type) {
              throw InternalError(
                  "lir verify: a value moved in is of another type than where "
                  "it goes");
            }
          },
          [&](const VariableAddressInstr& reached) {
            if (reached.position.value >= fn.variables.size()) {
              throw InternalError(
                  "lir verify: a variable is reached at a position the body "
                  "stated no variable at");
            }
          }},
      value);
}

// A landing is a block one departing call names and that opens by receiving
// what arrived. Neither half stands without the other: a body reaches a landing
// no other way, and a departure has nowhere else to be read -- so a block that
// opens this way and is named by nothing would never run, and one that is named
// and does not would run with the departure unread. It is one call's alone,
// because what a target owes on the way out of that call is that call's.
auto ReceivesDeparture(const Instr& instr) -> bool {
  const auto* assign = std::get_if<AssignInstr>(&instr.data);
  return assign != nullptr &&
         std::holds_alternative<ReceiveDepartureInstr>(assign->value);
}

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
      if (!ReceivesDeparture(instrs[at])) {
        continue;
      }
      if (at != 0) {
        throw InternalError(
            "lir verify: a departure is received somewhere other than at the "
            "start of a landing");
      }
    }
    const bool receives = !instrs.empty() && ReceivesDeparture(instrs[0]);
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
  const auto in_function = diag::FailureContext::InFunction(fn.name);
  const bool is_coroutine = unit.types.Get(fn.result_type).Is<CoroutineType>();
  const std::vector<bool> allocated = AllocatedStorage(fn);
  for (const BasicBlock& block : fn.blocks) {
    for (const Instr& instr : block.instrs) {
      VerifyInstr(unit, fn, allocated, instr);
    }
    if (const auto* call =
            std::get_if<DepartingCallInstr>(&block.terminator.data)) {
      if (const auto* place = std::get_if<Place>(&call->destination);
          place != nullptr && (!CallMakesValue(call->target) ||
                               !HoldsNothingYet(fn, allocated, *place))) {
        throw InternalError(
            "lir verify: a call puts a value into storage where it makes none, "
            "or where the storage already holds one");
      }
      RequireCallWellFormed(unit, fn, call->target, call->args);
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
