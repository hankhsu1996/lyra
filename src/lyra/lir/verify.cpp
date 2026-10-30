#include "lyra/lir/verify.hpp"

#include <algorithm>
#include <cstddef>
#include <format>
#include <optional>
#include <span>
#include <string>
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
#include "lyra/support/builtin_fn.hpp"

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

// An element is built by value out of a sequence of handles only. A part that
// is storage of its own (LRM 7.2, 7.3, 7.4, 7.8, 7.10) is a step of a place
// instead, because a write has to land in it and a value built out of it is a
// copy no write reaches.
void RequireHandleSequence(
    const CompilationUnit& unit, const Function& fn, const Operand& vector) {
  if (!unit.types.Get(OperandType(fn, vector)).Is<VectorType>()) {
    throw InternalError(
        "lir verify: an element is taken by value out of something other than "
        "a sequence of handles");
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
    if (!is_integral(part.type) || !is_integral(OperandType(fn, part.start))) {
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

// Whether a call of `target` answers storage of dynamic duration holding no
// value yet: the heap's, or what an allocation function of the library hands
// out.
auto AllocatesStorage(const CallTarget& target) -> bool {
  if (const auto* storage = std::get_if<DynamicStorageTarget>(&target)) {
    switch (storage->op) {
      case DynamicStorageTarget::Op::kAllocate:
        return true;
      case DynamicStorageTarget::Op::kDeallocate:
        return false;
    }
    throw InternalError("lir verify: unknown dynamic-storage operation");
  }
  if (const auto* entry = std::get_if<BuiltinTarget>(&target)) {
    return std::holds_alternative<support::AllocationFunction>(
        support::RuntimeEntryOf(entry->fn).declaration);
  }
  return false;
}

// The values that name storage of dynamic duration just allocated, by value
// id: storage holding no value yet, as a frame slot is before its first. An
// allocation that may raise is an invoke, and defines its value the same way.
auto AllocatedStorage(const Function& fn) -> std::vector<bool> {
  std::vector<bool> allocated(fn.values.size(), false);
  const auto record = [&](const Destination& destination,
                          const CallTarget& target) {
    const auto* defined = std::get_if<Transient>(&destination);
    if (defined != nullptr && AllocatesStorage(target)) {
      allocated.at(defined->value.value) = true;
    }
  };
  for (const BasicBlock& block : fn.blocks) {
    for (const Instr& instr : block.instrs) {
      const auto* assign = std::get_if<AssignInstr>(&instr.data);
      const auto* call =
          assign != nullptr ? std::get_if<CallInstr>(&assign->value) : nullptr;
      if (call != nullptr) {
        record(assign->destination, call->target);
      }
    }
    if (const auto* invoke = std::get_if<InvokeTerm>(&block.terminator.data)) {
      record(invoke->destination, invoke->target);
    }
  }
  return allocated;
}

// Whether `place` is storage holding no value until it is initialized: a frame
// slot, what storage just allocated holds, or a part -- a component, an element
// -- of the body's return place, which holds nothing until the body makes its
// answer there, part by part as rustc's clone shim builds its answer field by
// field. A part of any other storage lies in a value that storage holds. This
// admits the shapes a construction takes but does not prove each part is made
// exactly once, which no check here tracks.
auto HoldsNothingYet(
    const Function& fn, const std::vector<bool>& allocated, const Place& place)
    -> bool {
  if (place.chain.empty()) {
    return IsPlaceLocal(fn, place.base);
  }
  const auto* base = std::get_if<Use>(&place.base);
  if (base == nullptr) {
    return false;
  }
  if (fn.return_place == base->value) {
    return std::ranges::all_of(place.chain, [](const Projection& step) {
      return std::visit(
          Overloaded{
              [](const ComponentProjection&) { return true; },
              [](const ElementProjection&) { return true; },
              // Storage reached through what the answer holds is somebody
              // else's, which holds a value of its own.
              [](const DerefProjection&) { return false; },
              [](const MemberProjection&) { return false; }},
          step);
    });
  }
  return allocated.at(base->value.value) && place.chain.size() == 1 &&
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

// A value is put into storage only where it is made, and only into storage
// holding nothing yet that can hold one, which it initializes; whether what
// puts it there makes one is `makes_value`.
void RequireInitializes(
    const CompilationUnit& unit, const Function& fn,
    const std::vector<bool>& allocated, const Destination& destination,
    bool makes_value) {
  std::visit(
      Overloaded{
          [](const Transient&) {},
          [&](const Place& place) {
            if (!makes_value || !HoldsNothingYet(fn, allocated, place)) {
              const auto* base = std::get_if<Use>(&place.base);
              throw InternalError(
                  std::format(
                      "lir verify: a value is put into storage where nothing "
                      "is made, or where the storage already holds one (into "
                      "{}{})",
                      base != nullptr ? std::format("%{}", base->value.value)
                                      : std::string("storage no value names"),
                      place.chain.empty() ? "" : " through a projection"));
            }
            if (unit.types.Get(PlaceType(unit, fn, place)).IsAddressOnly()) {
              throw InternalError(
                  "lir verify: a value is put into storage that is only "
                  "addressable");
            }
          }},
      destination);
}

// A variable is named by a position the body stated a variable at.
void RequireStatedVariable(const Function& fn, VariablePosition position) {
  if (position.value >= fn.variables.size()) {
    throw InternalError(
        "lir verify: a variable is reached at a position the body stated no "
        "variable at");
  }
}

void VerifyRvalue(
    const CompilationUnit& unit, const Function& fn, const Rvalue& value,
    TypeId result_type);

void VerifyInstr(
    const CompilationUnit& unit, const Function& fn,
    const std::vector<bool>& allocated, const Instr& instr) {
  std::visit(
      Overloaded{
          [&](const AssignInstr& assign) {
            RequireInitializes(
                unit, fn, allocated, assign.destination,
                MakesValue(assign.value));
            VerifyRvalue(
                unit, fn, assign.value,
                DestinationType(unit, fn, assign.destination));
          },
          // A store puts a value of the place's type into storage that can
          // hold one.
          [&](const StoreInstr& store) {
            const TypeId place_type = PlaceType(unit, fn, store.place);
            if (unit.types.Get(place_type).IsAddressOnly()) {
              throw InternalError(
                  "lir verify: store into a place whose storage is only "
                  "addressable");
            }
            const TypeId value_type = OperandType(fn, store.value);
            if (value_type != place_type) {
              throw InternalError(
                  std::format(
                      "lir verify: store value type does not match its place "
                      "type (value Type[{}], place Type[{}])",
                      value_type.value, place_type.value));
            }
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
          [&](const EndVariableInstr& ended) {
            RequireStatedVariable(fn, ended.position);
          },
          [&](const BeginStorageInstr& begun) {
            PlaceType(unit, fn, begun.place);
          },
          [&](const EndStorageInstr& ended) {
            PlaceType(unit, fn, ended.place);
          },
          // Only an object has a class to be given.
          [&](const SetObjectClassInstr& set) {
            if (!unit.types.Get(set.cls).Is<ObjectType>()) {
              throw InternalError(
                  "lir verify: something other than an object is given a "
                  "class");
            }
          },
          // What replaces a value is made where it lies, so it has to be a
          // value made, and of the type the storage holds.
          [&](const EndAndReplaceInstr& replaced) {
            const TypeId place_type = PlaceType(unit, fn, replaced.place);
            if (unit.types.Get(place_type).IsAddressOnly()) {
              throw InternalError(
                  "lir verify: a value is ended and replaced in storage that "
                  "is only addressable");
            }
            if (!MakesValue(replaced.value)) {
              throw InternalError(
                  "lir verify: a value is replaced by nothing made");
            }
            VerifyRvalue(unit, fn, replaced.value, place_type);
          },
          [](const BeginHandlingInstr&) {}, [](const EndHandlingInstr&) {}},
      instr.data);
}

void VerifyRvalue(
    const CompilationUnit& unit, const Function& fn, const Rvalue& value,
    TypeId result_type) {
  // A read answers a value of the type its place holds, and storage reached
  // only through its address holds none.
  const auto verify_read = [&](const Place& place) {
    const TypeId place_type = PlaceType(unit, fn, place);
    if (unit.types.Get(place_type).IsAddressOnly()) {
      throw InternalError(
          "lir verify: load of a place whose storage is only addressable");
    }
    if (result_type != place_type) {
      throw InternalError(
          std::format(
              "lir verify: load result type does not match its place type "
              "(result Type[{}], place Type[{}])",
              result_type.value, place_type.value));
    }
  };
  std::visit(
      Overloaded{
          [&](const LoadInstr& load) { verify_read(load.place); },
          [&](const OverlaidLoadInstr& load) { verify_read(load.place); },
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
            const auto* from = unit.types.Get(OperandType(fn, cast.operand))
                                   .As<IntegralType>();
            const auto* to = unit.types.Get(result_type).As<IntegralType>();
            if (from != nullptr && to != nullptr && *from != *to) {
              throw InternalError(
                  "lir verify: cast changes its value's representation");
            }
          },
          [&](const VectorGetInstr& get) {
            RequireHandleSequence(unit, fn, get.vector);
          },
          [&](const UnionInstr& built) {
            const std::optional<std::span<const TypeId>> members =
                OverlaidMembers(unit, result_type);
            if (!members.has_value() || built.member.value >= members->size()) {
              throw InternalError(
                  "lir verify: a union is built holding a member its type "
                  "does not have");
            }
          },
          [&](const DiscriminantInstr& read) {
            if (!IsTaggedUnion(unit, PlaceType(unit, fn, read.place))) {
              throw InternalError(
                  "lir verify: a tag is read of something that is no tagged "
                  "union");
            }
          },
          [&](const CallInstr& call) {
            RequireCallWellFormed(unit, fn, call.target, call.args);
          },
          [](const ProductInstr&) {},
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
          [](const UnaryInstr&) {}, [](const VariableStorageInstr&) {},
          // Where it may stand is a property of the block rather than of the
          // instruction, so it is held where the blocks are walked.
          [](const LandingInstr&) {},
          [&](const UseInstr& use) {
            if (OperandType(fn, use.operand) != result_type) {
              throw InternalError(
                  "lir verify: a value moved in is of another type than where "
                  "it goes");
            }
          },
          [&](const BeginVariableInstr& begun) {
            RequireStatedVariable(fn, begun.position);
          }},
      value);
}

// A landing is a block one invoke names and that opens by taking what is
// unwinding. Neither half stands without the other: a body reaches a landing no
// other way, and what is unwinding has nowhere else to be taken -- so a block
// that opens this way and is named by nothing would never run, and one that is
// named and does not would run with nothing taken. It is one call's alone,
// because what a target owes on the way out of that call is that call's.
auto TakesUnwinding(const Instr& instr) -> bool {
  const auto* assign = std::get_if<AssignInstr>(&instr.data);
  return assign != nullptr &&
         std::holds_alternative<LandingInstr>(assign->value);
}

void VerifyLandings(const Function& fn) {
  std::vector<std::size_t> named(fn.blocks.size(), 0);
  for (const BasicBlock& block : fn.blocks) {
    if (const auto* call = std::get_if<InvokeTerm>(&block.terminator.data)) {
      ++named[call->landing.value];
    }
  }
  for (std::size_t index = 0; index < fn.blocks.size(); ++index) {
    const std::vector<Instr>& instrs = fn.blocks[index].instrs;
    for (std::size_t at = 0; at < instrs.size(); ++at) {
      if (!TakesUnwinding(instrs[at])) {
        continue;
      }
      if (at != 0) {
        throw InternalError(
            "lir verify: what is unwinding is taken somewhere other than at "
            "the start of a landing");
      }
    }
    const bool takes = !instrs.empty() && TakesUnwinding(instrs[0]);
    if (takes != (named[index] != 0)) {
      throw InternalError(
          takes ? "lir verify: a landing no invoke names"
                : "lir verify: an invoke names a block that takes nothing "
                  "unwinding");
    }
    if (named[index] > 1) {
      throw InternalError("lir verify: a landing more than one invoke names");
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
    if (const auto* call = std::get_if<InvokeTerm>(&block.terminator.data)) {
      RequireInitializes(
          unit, fn, allocated, call->destination, CallMakesValue(call->target));
      RequireCallWellFormed(unit, fn, call->target, call->args);
    }
    // Only a body whose call protocol is the coroutine one can hand control
    // back to the scheduler; a suspension anywhere else has no one to resume
    // it. An abandonment is reached only from a suspension, so it is bounded
    // by the same fact, and so is completing an execution with a departure,
    // which only the top of one can do.
    const bool coroutine_only =
        std::holds_alternative<SuspendTerm>(block.terminator.data) ||
        std::holds_alternative<AbandonTerm>(block.terminator.data) ||
        std::holds_alternative<DepartTerm>(block.terminator.data);
    if (coroutine_only && !is_coroutine) {
      throw InternalError(
          "lir verify: a suspension, or a completion with a departure, appears "
          "in a body whose result type is not a coroutine");
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
