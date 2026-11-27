#include "lyra/lir/verify.hpp"

#include <algorithm>
#include <cstddef>
#include <format>
#include <optional>
#include <ranges>
#include <span>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include "lyra/base/component_index.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/failure_context.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/function_id.hpp"
#include "lyra/lir/place_query.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/value_operation.hpp"

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

// A place reached through a write in progress -- bits of the value it
// designates -- is the target of a store and of nothing else: the store is a
// call on the designation, which hears which bits move, and nothing lies there
// to read or to take the address of.
void RequireDesignatedBitsOnlyStored(
    const CompilationUnit& unit, const Function& fn, const Place& place,
    bool stored) {
  if (!unit.types.Get(OperandType(fn, place.base)).Is<DesignationType>()) {
    return;
  }
  const bool bits_through_it =
      place.chain.size() > 1 &&
      std::holds_alternative<DerefProjection>(place.chain.front()) &&
      std::ranges::all_of(
          place.chain | std::views::drop(1), [](const Projection& step) {
            return std::holds_alternative<SliceProjection>(step);
          });
  if (!stored || !bits_through_it) {
    throw InternalError(
        "lir verify: a place reached through a write in progress is other "
        "than bits of its value stored through it");
  }
}

// A step selecting bits is taken within an integral value, starts at a value
// of an integral type and is stated at an integral type, and only another such
// step follows it, since bits have no parts but bits.
void RequireSlicesWellFormed(
    const CompilationUnit& unit, const Function& fn, const Place& place) {
  const auto is_integral = [&](TypeId type) {
    return unit.types.Get(type).Is<IntegralType>();
  };
  bool within_bits = false;
  for (std::size_t at = 0; at < place.chain.size(); ++at) {
    const auto* slice = std::get_if<SliceProjection>(&place.chain[at]);
    if (slice == nullptr) {
      if (within_bits) {
        throw InternalError(
            "lir verify: a step other than one selecting bits follows bits");
      }
      continue;
    }
    within_bits = true;
    if (!is_integral(PlaceTypeBefore(
            unit, fn, place, static_cast<std::ptrdiff_t>(at))) ||
        !is_integral(slice->part) ||
        !is_integral(OperandType(fn, slice->start))) {
      throw InternalError(
          "lir verify: bits are selected within a value, stated at a type, or "
          "started at a value that is not integral");
    }
  }
}

// Whether `argument` meets `parameter`, the parameter at `position`. A body of
// a class entering what a class it derives from declares hands its own object,
// whose class is another type (LRM 8.13), so a receiver is met by an address of
// the same kind of any object, as MIR leaves a receiver out of what a call
// meets.
auto MeetsParameter(
    const TypePool& types, std::size_t position, TypeId argument,
    TypeId parameter) -> bool {
  if (argument == parameter) {
    return true;
  }
  const auto* handed = types.Get(argument).As<PointerType>();
  const auto* taken = types.Get(parameter).As<PointerType>();
  return position == 0 && handed != nullptr && taken != nullptr &&
         handed->ownership == taken->ownership &&
         types.Get(handed->pointee).Is<ObjectType>() &&
         types.Get(taken->pointee).Is<ObjectType>();
}

// A call of a function of this unit states what that function takes and
// answers. Below this layer two references are one machine address, so nothing
// later can tell a call of the wrong type from a call of the right one.
void RequireCallMeetsFunction(
    const TypePool& types, const Function& caller, const Function& callee,
    std::span<const Operand> args, TypeId result_type) {
  if (args.size() != callee.params.size()) {
    throw InternalError(
        std::format(
            "lir verify: a call of '{}' states {} arguments where it takes {}",
            callee.name, args.size(), callee.params.size()));
  }
  for (std::size_t i = 0; i < args.size(); ++i) {
    const TypeId argument = OperandType(caller, args[i]);
    const TypeId parameter = callee.values.Get(callee.params[i]).type;
    if (!MeetsParameter(types, i, argument, parameter)) {
      throw InternalError(
          std::format(
              "lir verify: argument {} of a call of '{}' is of another type "
              "than its parameter ({} [{}], parameter {} [{}])",
              i, callee.name, types.Get(argument).KindName(), argument.value,
              types.Get(parameter).KindName(), parameter.value));
    }
  }
  if (result_type != callee.result_type) {
    throw InternalError(
        std::format(
            "lir verify: a call of '{}' takes its answer at another type than "
            "it answers",
            callee.name));
  }
}

// What a call's target requires of the operands the call states and of the
// answer it takes.
void RequireCallWellFormed(
    const CompilationUnit& unit, const Function& fn, const CallTarget& target,
    std::span<const Operand> args, TypeId result_type) {
  if (const auto* called = std::get_if<FunctionTarget>(&target)) {
    RequireCallMeetsFunction(
        unit.types, fn, unit.functions.Get(called->function), args,
        result_type);
    return;
  }
}

// Whether a call of `target` answers storage holding no value yet: the heap's,
// what an allocation function of the library hands out, or where the reserved
// placement form is told to place one.
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
    return support::HandsOutStorage(
        support::RuntimeEntryOf(entry->fn).declaration);
  }
  return false;
}

// The values that name storage of dynamic duration just allocated, by value
// id: storage holding no value yet, as a frame slot is before its first. An
// allocation stated as an invoke defines its value the same way.
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
      record(invoke->destination, invoke->call.target);
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
              [](const MemberProjection&) { return false; },
              // Bits are written into the value already there.
              [](const SliceProjection&) { return false; },
              [](const IndexProjection&) { return true; }},
          step);
    });
  }
  return allocated.at(base->value.value) && place.chain.size() == 1 &&
         std::holds_alternative<DerefProjection>(place.chain.front());
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

// A tagged union holds only a member its type has, whether it is built whole
// or tagged ahead of the member made in it.
void RequireOverlaidMember(
    const CompilationUnit& unit, TypeId type, base::ComponentIndex member) {
  const std::optional<std::span<const TypeId>> members =
      OverlaidMembers(unit, type);
  if (!members.has_value() || member.value >= members->size()) {
    throw InternalError(
        "lir verify: a tagged union is built or tagged holding a member its "
        "type does not have");
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
            RequireDesignatedBitsOnlyStored(unit, fn, store.place, true);
            RequireSlicesWellFormed(unit, fn, store.place);
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
          // A union is tagged in storage holding nothing yet, which the member
          // made after it then initializes.
          [&](const SetDiscriminantInstr& set) {
            RequireInitializes(unit, fn, allocated, set.place, true);
            RequireOverlaidMember(
                unit, PlaceType(unit, fn, set.place), set.member);
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
          }},
      instr.data);
}

void VerifyRvalue(
    const CompilationUnit& unit, const Function& fn, const Rvalue& value,
    TypeId result_type) {
  // A read answers a value of the type its place holds, and storage reached
  // only through its address holds none.
  const auto verify_read = [&](const Place& place) {
    RequireDesignatedBitsOnlyStored(unit, fn, place, false);
    RequireSlicesWellFormed(unit, fn, place);
    const TypeId place_type = PlaceType(unit, fn, place);
    if (unit.types.Get(place_type).IsAddressOnly()) {
      throw InternalError(
          "lir verify: load of a place whose storage is only addressable");
    }
    if (result_type != place_type) {
      throw InternalError(
          std::format(
              "lir verify: load result type does not match its place type "
              "(result Type[{}], a {}; place Type[{}], a {})",
              result_type.value, unit.types.Get(result_type).KindName(),
              place_type.value, unit.types.Get(place_type).KindName()));
    }
  };
  std::visit(
      Overloaded{
          [&](const LoadInstr& load) { verify_read(load.place); },
          [&](const AddrOfInstr& addr) {
            RequireDesignatedBitsOnlyStored(unit, fn, addr.place, false);
            if (std::ranges::any_of(addr.place.chain, [](const auto& step) {
                  return std::holds_alternative<SliceProjection>(step);
                })) {
              throw InternalError(
                  "lir verify: an address is taken of bits, which lie at no "
                  "address of their own");
            }
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
          [&](const TaggedUnionInstr& built) {
            RequireOverlaidMember(unit, result_type, built.member);
          },
          [&](const DiscriminantInstr& read) {
            if (!IsTaggedUnion(unit, PlaceType(unit, fn, read.place))) {
              throw InternalError(
                  "lir verify: a tag is read of something that is no tagged "
                  "union");
            }
          },
          [&](const CallInstr& call) {
            RequireCallWellFormed(
                unit, fn, call.target, call.args, result_type);
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

// Whether an instruction calls something that can be unwound out of.
auto CallsWhatCanUnwind(
    const CompilationUnit& unit, const Function& fn, const Instr& instr)
    -> bool {
  if (const auto* assign = std::get_if<AssignInstr>(&instr.data)) {
    const auto* call = std::get_if<CallInstr>(&assign->value);
    return call != nullptr &&
           CallCanUnwind(
               unit, call->target,
               DestinationType(unit, fn, assign->destination));
  }
  if (const auto* replaced = std::get_if<EndAndReplaceInstr>(&instr.data)) {
    const auto* call = std::get_if<CallInstr>(&replaced->value);
    return call != nullptr &&
           CallCanUnwind(
               unit, call->target, PlaceType(unit, fn, replaced->place));
  }
  return false;
}

// Two rules about unwinding.
//
// A cleanup leaves only by running to its end: a block reached only by
// unwinding calls nothing that can be unwound out of, goes on only to another
// such block, on out of the body, or to where the body takes what is
// unwinding, and is reached only that way -- from a call naming it, from
// another such block, or from a taking point handing on what is not the
// design's. Where unwinding is taken, control goes on in an ordinary block.
//
// A function declared not to unwind lets no unwinding out of it, so its
// `nounwind` is true: nothing in it goes on unwinding out of the body, from a
// call or from a taking point. A taking point carries on whatever is not the
// design's, so no call in such a function can be unwound out of at all.
void VerifyUnwinding(
    const CompilationUnit& unit, const Function& fn, bool cannot_unwind) {
  const auto is_cleanup = [&](BlockId id) {
    return fn.blocks[id.value].is_cleanup;
  };
  const auto refuse = [](std::string_view what) {
    throw InternalError(
        std::format("lir verify: {} -- please report this as a bug", what));
  };
  for (const BasicBlock& block : fn.blocks) {
    for (const Instr& instr : block.instrs) {
      if (!CallsWhatCanUnwind(unit, fn, instr)) {
        continue;
      }
      if (block.is_cleanup) {
        refuse(
            "a block reached only by unwinding calls something that can be "
            "unwound out of");
      }
      if (cannot_unwind) {
        refuse(
            "a function declared not to unwind calls something that can be "
            "unwound out of with nothing in it to take what unwinds");
      }
    }
    const auto goes_on_alike = [&](BlockId target) {
      if (is_cleanup(target) != block.is_cleanup) {
        refuse(
            "control passes between a block reached only by unwinding and an "
            "ordinary one other than by unwinding or by taking it");
      }
    };
    std::visit(
        Overloaded{
            [&](const InvokeTerm& call) {
              if (block.is_cleanup) {
                refuse(
                    "a block reached only by unwinding names one to unwind to");
              }
              if (!CallCanUnwind(
                      unit, call.call.target,
                      DestinationType(unit, fn, call.destination))) {
                refuse(
                    "a call names a block to unwind to though its callee "
                    "cannot be unwound out of");
              }
              if (!is_cleanup(call.unwind) || is_cleanup(call.returned)) {
                refuse(
                    "a call unwinds to an ordinary block or returns to one "
                    "reached only by unwinding");
              }
            },
            [&](const ReceiveDepartureTerm& taken) {
              if (!block.is_cleanup) {
                refuse(
                    "unwinding is taken somewhere other than on a way out by "
                    "unwinding");
              }
              if (is_cleanup(taken.then)) {
                refuse(
                    "what is taken goes on in a block reached only by "
                    "unwinding");
              }
              if (!is_cleanup(taken.otherwise)) {
                refuse("what is not taken goes on in an ordinary block");
              }
            },
            [&](const UnwindResumeTerm&) {
              if (!block.is_cleanup) {
                refuse("an ordinary block goes on unwinding");
              }
              if (cannot_unwind) {
                refuse("a function declared not to unwind goes on unwinding");
              }
            },
            [&](const BranchTerm& branch) { goes_on_alike(branch.target); },
            [&](const CondBranchTerm& branch) {
              goes_on_alike(branch.if_true);
              goes_on_alike(branch.if_false);
            },
            [&](const SwitchTerm& dispatch) {
              for (const SwitchTarget& target : dispatch.targets) {
                goes_on_alike(target.target);
              }
              goes_on_alike(dispatch.otherwise);
            },
            [&](const SuspendTerm& suspension) {
              goes_on_alike(suspension.resume);
              goes_on_alike(suspension.abandoned);
            },
            [](const ReturnTerm&) {}, [](const AbandonTerm&) {},
            [](const DepartTerm&) {}, [](const UnreachableTerm&) {}},
        block.terminator.data);
  }
}

// A type's default is reached through what the type is told to the library by,
// with no call stating what it answers.
void VerifyDefaults(const CompilationUnit& unit) {
  for (const auto& [operation, function] : unit.type_operations) {
    if (function.has_value() &&
        operation.answers ==
            support::ValueOperation{support::DefaultItem::kDeclaredDefault} &&
        unit.functions.Get(*function).result_type != operation.type) {
      throw InternalError(
          "lir verify: a type's default answers a value of another type");
    }
  }
}

void VerifyFunction(
    const CompilationUnit& unit, FunctionId id, const Function& fn) {
  const auto in_function = diag::FailureContext::InFunction(fn.name);
  const bool cannot_unwind = !unit.functions.Attributes(id).can_unwind;
  const bool is_coroutine = unit.types.Get(fn.result_type).Is<CoroutineType>();
  const std::vector<bool> allocated = AllocatedStorage(fn);
  for (const BasicBlock& block : fn.blocks) {
    for (const Instr& instr : block.instrs) {
      const diag::FailureContext at(instr.span);
      VerifyInstr(unit, fn, allocated, instr);
    }
    const diag::FailureContext ended_at(block.terminator.span);
    if (const auto* call = std::get_if<InvokeTerm>(&block.terminator.data)) {
      RequireInitializes(
          unit, fn, allocated, call->destination,
          CallMakesValue(call->call.target));
      RequireCallWellFormed(
          unit, fn, call->call.target, call->call.args,
          DestinationType(unit, fn, call->destination));
    }
    if (const auto* taken =
            std::get_if<ReceiveDepartureTerm>(&block.terminator.data)) {
      RequireInitializes(
          unit, fn, allocated, taken->departure,
          CallMakesValue(ReceiveDepartureCall().target));
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
  VerifyUnwinding(unit, fn, cannot_unwind);
}

}  // namespace

void Verify(const CompilationUnit& unit) {
  for (const FunctionId id : unit.functions.Ids()) {
    if (!unit.functions.IsDefined(id)) {
      throw InternalError(
          std::format(
              "lir verify: unit '{}' declares function {} and defines nothing "
              "for it -- please report this as a bug",
              unit.body_name, id.value));
    }
  }
  for (const FunctionId id : unit.functions.Ids()) {
    VerifyFunction(unit, id, unit.functions.Get(id));
  }
  VerifyDefaults(unit);
}

}  // namespace lyra::lir
