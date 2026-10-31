#include "lyra/lowering/hir_to_mir/access_path.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/diag_message.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/object_change.hpp"
#include "lyra/lowering/hir_to_mir/select_position.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/value_operations.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/mir/verify.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// What a path is taken for at one of its steps: the part's value, the part as
// a place a write lands in, the part designated within a write in progress, or
// a reference to the part.
enum class StepUse : std::uint8_t { kRead, kWrite, kDesignate, kLend };

// Only an element and a component may be passed by reference (LRM 13.5.2), so
// a step to anything else taken on a reference is a producer that lent what the
// language does not.
[[noreturn]] void RefuseReference() {
  throw InternalError(
      "access path: only an element and a component may be passed by "
      "reference (LRM 13.5.2), and this lent target steps into neither");
}

// A queue's run of elements is read as a queue of its own (LRM 7.10.1), and
// written by the queue's own slice write, which the store builds rather than
// a step, as an array's run of elements is (LRM 7.6).
auto EntryToQueueRun(StepUse use) -> support::BuiltinFn {
  switch (use) {
    case StepUse::kRead:
      return support::BuiltinFn::kQueueSlice;
    case StepUse::kWrite:
    case StepUse::kDesignate:
      throw InternalError(
          "access path: a queue's slice is written by the queue's own slice "
          "write, which the store builds, and this descent takes a step into "
          "one for a write");
    case StepUse::kLend:
      RefuseReference();
  }
  throw InternalError("access path: unknown step use");
}

// The call that takes one step on a receiver: the entry, what its callee
// states beside the receiver, and what it is handed beside it.
struct StepCall {
  support::BuiltinFn entry;
  std::optional<mir::CallPart> part = std::nullopt;
  std::vector<mir::TypeId> type_arguments = {};
  std::vector<mir::ExprId> arguments;
};

// A step taken by a call answering a reference into what it is taken on,
// whose dereference is the part: an element, read or written, as rustc's
// `*Index::index(&a, i)` and `*IndexMut::index_mut(&mut a, i)` are.
struct ReferenceStep {
  StepCall call;
  mir::Mutability mutability;
};

// How one step is taken: as a field of what it is taken on, by a call
// answering a reference to the part, or by a call answering the part.
using StepTaken =
    std::variant<mir::ComponentFieldTarget, ReferenceStep, StepCall>;

// An element is reached by a read form and a write form, each answering a
// reference, since reading a position that names no element answers the
// element default and creates nothing while writing one creates or discards it
// (LRM 7.4.6, 7.8.6, 7.8.7); within a write in progress it is designated, and
// on a reference it is referred to.
auto ElementStepTaken(
    StepUse use, support::BuiltinFn read, support::BuiltinFn write,
    support::BuiltinFn designate, support::BuiltinFn lend, mir::ExprId operand)
    -> StepTaken {
  switch (use) {
    case StepUse::kRead:
      return ReferenceStep{
          .call = StepCall{.entry = read, .arguments = {operand}},
          .mutability = mir::Mutability::kReadOnly};
    case StepUse::kWrite:
      return ReferenceStep{
          .call = StepCall{.entry = write, .arguments = {operand}},
          .mutability = mir::Mutability::kMutable};
    case StepUse::kDesignate:
      return StepCall{.entry = designate, .arguments = {operand}};
    case StepUse::kLend:
      return StepCall{.entry = lend, .arguments = {operand}};
  }
  throw InternalError("access path: unknown step use");
}

// A character is no storage of its own (LRM 6.16), so it is read by the
// string's `getc`; a write puts it with the string's `putc`, which the store
// builds rather than a step.
auto CharacterStepTaken(StepUse use, mir::ExprId position) -> StepTaken {
  switch (use) {
    case StepUse::kRead:
      return StepCall{
          .entry = support::BuiltinFn::kGetc, .arguments = {position}};
    case StepUse::kWrite:
    case StepUse::kDesignate:
    case StepUse::kLend:
      throw InternalError(
          "access path: a character of a string is no storage of its own, so "
          "a write puts it with the string's own operation and nothing is "
          "designated or lent");
  }
  throw InternalError("access path: unknown step use");
}

// A product's component is storage of its own and every component is live at
// once, so reading it and writing it are the one field step (LRM 7.2); within
// a write in progress, and on a reference, it is a step taken on that value.
auto ComponentStep(StepUse use, base::ComponentIndex position) -> StepTaken {
  switch (use) {
    case StepUse::kRead:
    case StepUse::kWrite:
      return mir::ComponentFieldTarget{.position = position};
    case StepUse::kDesignate:
      return StepCall{
          .entry = support::BuiltinFn::kDesignateComponent,
          .part = mir::CallPart{position},
          .arguments = {}};
    case StepUse::kLend:
      return StepCall{
          .entry = support::BuiltinFn::kReferComponent,
          .part = mir::CallPart{position},
          .arguments = {}};
  }
  throw InternalError("access path: unknown step use");
}

// A union's member is a component of the union, lying at the union's own start
// (LRM 7.3), and is taken as one; it has no identity of its own, so nothing
// lends it by reference (LRM 13.5.2).
auto UnionMemberStep(StepUse use, base::ComponentIndex position) -> StepTaken {
  switch (use) {
    case StepUse::kRead:
    case StepUse::kWrite:
      return ComponentStep(use, position);
    case StepUse::kDesignate:
      return StepCall{
          .entry = support::BuiltinFn::kDesignateMember,
          .part = mir::CallPart{position},
          .arguments = {}};
    case StepUse::kLend:
      throw InternalError(
          "access path: a union's member has no identity of its own, and this "
          "descent lends one");
  }
  throw InternalError("access path: unknown step use");
}

// A run of elements is several elements rather than one storage (LRM 7.4.6),
// so it is read by a step answering the run as a value of its own count, which
// is a type of its own named on the callee, and written by its array's own
// operation, which the store builds rather than a step. `bounds` are where the
// run starts and how many elements it takes.
auto ElementRunStepTaken(
    StepUse use, const DescentStep& step, std::vector<mir::ExprId> bounds)
    -> StepTaken {
  switch (use) {
    case StepUse::kRead:
      return StepCall{
          .entry = support::BuiltinFn::kElementSlice,
          .type_arguments = {step.part_type},
          .arguments = std::move(bounds)};
    case StepUse::kWrite:
    case StepUse::kDesignate:
      throw InternalError(
          "access path: a run of elements is written by its array's own slice "
          "write, which the store builds, and this descent takes a step into "
          "one for a write");
    case StepUse::kLend:
      RefuseReference();
  }
  throw InternalError("access path: unknown step use");
}

// Bits of a packed value are read by the value's own operation on it (LRM
// 11.5.1), and written through a reference to them the store takes, as
// `std::bitset` answers a reference to one bit; no reference the program
// holds ends on them (LRM 13.5.2).
auto BitsStepTaken(StepUse use, const DescentStep& step, mir::ExprId start)
    -> StepTaken {
  switch (use) {
    case StepUse::kRead:
      return StepCall{
          .entry = support::BuiltinFn::kSlice,
          .type_arguments = {step.part_type},
          .arguments = {start}};
    case StepUse::kWrite:
    case StepUse::kDesignate:
      throw InternalError(
          "access path: bits are written through a reference to them, which "
          "the store takes, and this descent takes a step into them for a "
          "write");
    case StepUse::kLend:
      RefuseReference();
  }
  throw InternalError("access path: unknown step use");
}

// A step to bits is at the type the bits are read at, which no
// operand states; a step to a component or a member names it by position, the
// part having a type of its own; a run of elements is handed how many it takes
// as a machine count.
auto StepTakenFor(
    const mir::CompilationUnit& unit, mir::Builder block,
    const DescentStep& step, StepUse use) -> StepTaken {
  return std::visit(
      Overloaded{
          [&](const StepToBits& bits) {
            return BitsStepTaken(use, step, bits.start);
          },
          [&](const StepToElementRun& run) -> StepTaken {
            return ElementRunStepTaken(
                use, step,
                {run.start, mir::BuildMachineCount(unit, block, run.count)});
          },
          [&](const StepToElement& element) {
            return ElementStepTaken(
                use, support::BuiltinFn::kElement,
                support::BuiltinFn::kElementMut,
                support::BuiltinFn::kDesignateElement,
                support::BuiltinFn::kReferElement, element.position);
          },
          [&](const StepToAssocElement& element) {
            return ElementStepTaken(
                use, support::BuiltinFn::kAssocElement,
                support::BuiltinFn::kAssocElementMut,
                support::BuiltinFn::kAssocDesignateElement,
                support::BuiltinFn::kAssocReferElement, element.key);
          },
          [&](const StepToCharacter& character) {
            return CharacterStepTaken(use, character.position);
          },
          [&](const StepToQueueRun& run) -> StepTaken {
            return StepCall{
                .entry = EntryToQueueRun(use),
                .arguments = {run.lowest, run.highest}};
          },
          [&](const StepToComponent& component) {
            return ComponentStep(use, component.position);
          },
          [&](const StepToUnionMember& member) {
            return UnionMemberStep(use, member.position);
          },
          [&](const StepToTaggedMember& member) {
            return UnionMemberStep(use, member.position);
          }},
      step.to);
}

// Whether a step reaches a part a write in progress designates within itself:
// an element, a component, or a union's member, each one place a write lands
// on. Bits and several elements in a row are no one place, and a character
// lies inside the string's own value.
auto DesignatedWithinAWrite(const DescentStep& step) -> bool {
  return std::visit(
      Overloaded{
          [](const StepToElement&) { return true; },
          [](const StepToAssocElement&) { return true; },
          [](const StepToComponent&) { return true; },
          [](const StepToUnionMember&) { return true; },
          [](const StepToTaggedMember&) { return true; },
          [](const StepToBits&) { return false; },
          [](const StepToElementRun&) { return false; },
          [](const StepToQueueRun&) { return false; },
          [](const StepToCharacter&) { return false; }},
      step.to);
}

// How many of `path`'s steps a write opened on its owner takes within itself:
// where the owner is a capability wrapper, the write designates each part it
// can land on, so it knows what forming each one did and keeps no more than
// the part it lands on, for as long as each step reaches such a part. Any step
// left is taken on what the write lands on, or on the last designation by an
// operation of the value's own. A write onto anything else takes none within
// itself.
auto StepsTakenWithinTheWrite(
    const mir::CompilationUnit& unit, const mir::Block& block,
    const AccessPath& path) -> std::size_t {
  const auto* owner = std::get_if<mir::ExprId>(&path.owner);
  if (owner == nullptr) {
    return 0;
  }
  const mir::Type& owner_ty = unit.types.Get(block.exprs.Get(*owner).type);
  if (!owner_ty.IsCapabilityWrapper()) {
    return 0;
  }
  return static_cast<std::size_t>(
      std::ranges::find_if_not(path.descent, DesignatedWithinAWrite) -
      path.descent.begin());
}

// The call `call` takes on `receiver`, answering `type`.
auto StepCallExpr(StepCall call, mir::ExprId receiver, mir::TypeId type)
    -> mir::Expr {
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{
                      .target = call.entry,
                      .receiver = receiver,
                      .part = call.part,
                      .type_arguments = std::move(call.type_arguments)},
              .arguments = std::move(call.arguments)},
      .type = type};
}

// The call a reference step takes on `receiver`, answering a reference to the
// part, of type `type`, at the step's mutability.
auto ReferenceStepCall(
    mir::CompilationUnit& unit, ReferenceStep step, mir::ExprId receiver,
    mir::TypeId type) -> mir::Expr {
  return StepCallExpr(
      std::move(step.call), receiver,
      unit.types.Intern(
          mir::Type{mir::ReferenceType{
              .referent = type, .mutability = step.mutability}}));
}

// The part a reference step reaches in `receiver`: the dereference of the
// reference the call answers, whose referent is `type`.
auto ReferencedPart(
    mir::CompilationUnit& unit, mir::Builder block, ReferenceStep step,
    mir::ExprId receiver, mir::TypeId type) -> mir::Expr {
  return mir::MakeDerefExpr(
      block.Add(ReferenceStepCall(unit, std::move(step), receiver, type)),
      type);
}

// One step taken on `receiver` for `use`, answering `type`.
auto StepExpr(
    mir::CompilationUnit& unit, mir::Builder block, const DescentStep& step,
    StepUse use, mir::ExprId receiver, mir::TypeId type) -> mir::Expr {
  return std::visit(
      Overloaded{
          [&](const mir::ComponentFieldTarget& field) {
            return mir::MakeFieldAccessExpr(receiver, field, type);
          },
          [&](ReferenceStep reference) {
            return ReferencedPart(
                unit, block, std::move(reference), receiver, type);
          },
          [&](StepCall call) {
            return mir::MakeBuiltinCall(
                unit, block.Body(), call.entry, receiver, call.part,
                std::move(call.type_arguments), std::move(call.arguments),
                type);
          }},
      StepTakenFor(unit, block, step, use));
}

auto TakeStep(
    mir::CompilationUnit& unit, mir::Builder block, const DescentStep& step,
    StepUse use, mir::ExprId receiver, mir::TypeId type) -> mir::ExprId {
  return block.Add(StepExpr(unit, block, step, use, receiver, type));
}

// A net's resolved cell is readable and observable, but no value gets into it
// this way at all: a net takes one only through a driver (LRM 6.5), and a net
// is not a variable, so it is neither a store destination nor a legal `ref`
// actual (LRM 13.5.2). A producer hands this the driver in the cell's place;
// arriving with the cell means it did not.
void RefuseNetCell(const mir::Type& place_ty) {
  if (place_ty.Is<mir::ResolvedType>()) {
    throw InternalError(
        "access path: a net's cell takes no value a write may put there; the "
        "destination is one of its drivers");
  }
}

// The owner, where it is a capability wrapper whose whole contents a store may
// replace by acting on the wrapper.
auto StoredWrapper(
    const mir::CompilationUnit& unit, const mir::Block& block,
    const PathOwner& owner) -> std::optional<mir::ExprId> {
  return std::visit(
      Overloaded{
          [&](mir::ExprId place) -> std::optional<mir::ExprId> {
            const mir::Type& type = unit.types.Get(block.exprs.Get(place).type);
            if (!type.IsCapabilityWrapper() || type.Is<mir::ResolvedType>()) {
              return std::nullopt;
            }
            return place;
          },
          [](const ObjectProperty&) -> std::optional<mir::ExprId> {
            return std::nullopt;
          }},
      owner);
}

// The node `id` of `from`, named again in `to`. Only a node that evaluates
// nothing can be: a name, a constant, or a place formed over those means the
// same wherever it is written, while a computation written again would run
// again.
auto NamedAgain(const mir::Block& from, mir::Builder to, mir::ExprId id)
    -> mir::ExprId {
  const mir::Expr& node = from.exprs.Get(id);
  const auto again = [&](mir::ExprId operand) {
    return NamedAgain(from, to, operand);
  };
  const auto computes = []() -> mir::ExprData {
    throw InternalError(
        "access path: a settled path evaluates nothing, and this one holds a "
        "node that computes");
  };
  mir::ExprData data = std::visit(
      Overloaded{
          [](const mir::StringLiteral& e) -> mir::ExprData { return e; },
          [](const mir::NullLiteral& e) -> mir::ExprData { return e; },
          [](const mir::MachineBoolLiteral& e) -> mir::ExprData { return e; },
          [](const mir::MachineIntLiteral& e) -> mir::ExprData { return e; },
          [](const mir::MachineFloatLiteral& e) -> mir::ExprData { return e; },
          [](const mir::ReferenceExpr& e) -> mir::ExprData { return e; },
          [&](const mir::DerefExpr& e) -> mir::ExprData {
            return mir::DerefExpr{.pointer = again(e.pointer)};
          },
          [&](const mir::AddressOfExpr& e) -> mir::ExprData {
            return mir::AddressOfExpr{.operand = again(e.operand)};
          },
          [&](const mir::BorrowExpr& e) -> mir::ExprData {
            return mir::BorrowExpr{.operand = again(e.operand)};
          },
          [&](const mir::CopyExpr& e) -> mir::ExprData {
            return mir::CopyExpr{.place = again(e.place)};
          },
          [&](const mir::DiscriminantExpr& e) -> mir::ExprData {
            return mir::DiscriminantExpr{.tagged = again(e.tagged)};
          },
          [&](const mir::FieldAccessExpr& e) -> mir::ExprData {
            return mir::FieldAccessExpr{
                .receiver = again(e.receiver), .field = e.field};
          },
          [&](const mir::UnaryExpr&) { return computes(); },
          [&](const mir::BinaryExpr&) { return computes(); },
          [&](const mir::CastExpr&) { return computes(); },
          [&](const mir::ConditionalExpr&) { return computes(); },
          [&](const mir::BlockExpr&) { return computes(); },
          [&](const mir::AssignExpr&) { return computes(); },
          [&](const mir::CallExpr&) { return computes(); },
          [&](const mir::NewExpr&) { return computes(); },
          [&](const mir::MoveExpr&) { return computes(); },
          [&](const mir::ClosureExpr&) { return computes(); },
          [&](const mir::CompositeExpr&) { return computes(); },
          [&](const mir::UnionExpr&) { return computes(); },
          [&](const mir::AwaitExpr&) { return computes(); },
          [&](const mir::WaitExpr&) { return computes(); },
          [&](const mir::VectorGetExpr&) { return computes(); }},
      node.data);
  return to.Add(mir::Expr{.data = std::move(data), .type = node.type});
}

// The type of the value the last step of `path`, which takes at least one,
// selects within. Every step into a packed value names bits of that one vector
// (LRM 7.2.1), so where the steps at the end of the descent each enter a packed
// value, what the last of them selects within is the outermost of those.
auto ValueSelectedWithin(
    const mir::CompilationUnit& unit, const mir::Block& block,
    const AccessPath& path) -> mir::TypeId {
  const auto stepped_into = [&](std::size_t step) {
    return step == 0 ? OwnerValueType(unit, block, path.owner)
                     : path.descent[step - 1].part_type;
  };
  std::size_t first = path.descent.size() - 1;
  while (first > 0 && unit.types.Get(stepped_into(first - 1)).IsIntegral()) {
    --first;
  }
  return stepped_into(first);
}

}  // namespace

auto DescendInto(AccessPath base, DescentStep step) -> AccessPath {
  base.descent.push_back(std::move(step));
  return base;
}

auto OwnerPlace(const AccessPath& path) -> mir::ExprId {
  return std::visit(
      Overloaded{
          [](mir::ExprId place) { return place; },
          [](const ObjectProperty&) -> mir::ExprId {
            throw InternalError(
                "access path: this construct's owner is a place, and a "
                "property of an object reached it");
          }},
      path.owner);
}

auto OwnerValueType(
    const mir::CompilationUnit& unit, const mir::Block& block,
    const PathOwner& owner) -> mir::TypeId {
  return std::visit(
      Overloaded{
          [&](mir::ExprId place) {
            const mir::TypeId owner_type = block.exprs.Get(place).type;
            const mir::Type& owner_ty = unit.types.Get(owner_type);
            return owner_ty.IsCapabilityWrapper() ? owner_ty.WrappedValueType()
                                                  : owner_type;
          },
          [](const ObjectProperty& property) { return property.type; }},
      owner);
}

auto PathValueType(
    const mir::CompilationUnit& unit, const mir::Block& block,
    const AccessPath& path) -> mir::TypeId {
  if (!path.descent.empty()) {
    return path.descent.back().part_type;
  }
  return OwnerValueType(unit, block, path.owner);
}

namespace {

// Where a write in progress that took every step of a path within itself ends:
// landed on the part those steps designate, which is then a place, or still
// the designation, which an operation of the value's own then writes through.
enum class WriteEnd : std::uint8_t { kLanded, kDesignated };

// What a write along `path` reaches, `end` saying how a write in progress
// that took every step within itself ends.
auto ReachedForWrite(
    mir::CompilationUnit& unit, mir::Builder block, const AccessPath& path,
    WriteEnd end) -> mir::ExprId {
  auto step = path.descent.begin();
  const auto from_place = [&](mir::ExprId owner) -> mir::ExprId {
    const mir::Type& owner_ty = unit.types.Get(block.Get(owner).type);
    if (!owner_ty.IsCapabilityWrapper()) {
      return owner;
    }
    RefuseNetCell(owner_ty);
    // The write is opened on the wrapper, the whole of what the wrapper holds
    // is designated within it, and the steps the write takes within itself
    // designate their parts there before it lands.
    mir::TypeId value = owner_ty.WrappedValueType();
    const auto designated = [&](mir::TypeId part) {
      return unit.types.Intern(mir::Type{mir::DesignationType{.value = part}});
    };
    const mir::ExprId write = block.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{
                            .target = support::BuiltinFn::kOpenForWrite,
                            .receiver = owner},
                    .arguments = {}},
            .type = unit.types.Intern(
                mir::Type{mir::OpenWriteType{.value = value}})});
    mir::ExprId designation = block.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{
                            .target = support::BuiltinFn::kDesignateWhole,
                            .receiver = write},
                    .arguments = {}},
            .type = designated(value)});
    const auto within = path.descent.begin() +
                        static_cast<std::ptrdiff_t>(
                            StepsTakenWithinTheWrite(unit, block.Body(), path));
    for (; step != within; ++step) {
      value = step->part_type;
      designation = TakeStep(
          unit, block, *step, StepUse::kDesignate, designation,
          designated(value));
    }
    if (step == path.descent.end()) {
      switch (end) {
        case WriteEnd::kLanded:
          break;
        case WriteEnd::kDesignated:
          return designation;
      }
    }
    return block.Add(mir::MakeDerefExpr(designation, value));
  };
  // A property is written through a write opened on its object alone, which
  // the object hears when the write ends; the property is reached through the
  // write.
  mir::ExprId reached = std::visit(
      Overloaded{
          from_place,
          [&](const ObjectProperty& owner) {
            return block.Add(PropertyStorage(
                unit, block, OpenObjectWrite(unit, block, owner.object),
                owner.property, owner.type));
          }},
      path.owner);
  for (; step != path.descent.end(); ++step) {
    reached =
        TakeStep(unit, block, *step, StepUse::kWrite, reached, step->part_type);
  }
  return reached;
}

// What the operation writing the trailing bits or run of elements of a path is
// taken on: the place the value they are part of lies in, or, where a write in
// progress designated that value, its designation, so the write hears what the
// operation moved.
auto WriteOperandOf(
    mir::CompilationUnit& unit, mir::Builder block, const AccessPath& path,
    std::size_t steps) -> mir::ExprId {
  const AccessPath whole{
      .owner = path.owner,
      .descent = {
          path.descent.begin(),
          path.descent.begin() + static_cast<std::ptrdiff_t>(steps)}};
  return ReachedForWrite(unit, block, whole, WriteEnd::kDesignated);
}

// The part `path` names, reached for a read where it lies: what the owner
// holds, then each step as a read takes it.
auto PlaceForRead(
    mir::CompilationUnit& unit, mir::Builder block, const AccessPath& path)
    -> mir::ExprId {
  mir::ExprId reached = std::visit(
      Overloaded{
          [&](mir::ExprId place) {
            const mir::Type& owner_ty = unit.types.Get(block.Get(place).type);
            if (!owner_ty.IsCapabilityWrapper()) {
              return place;
            }
            return block.Add(block.WrapperContents(
                unit.types, place, owner_ty.WrappedValueType()));
          },
          [&](const ObjectProperty& owner) {
            return block.Add(PropertyStorage(
                unit, block, owner.object, owner.property, owner.type));
          }},
      path.owner);
  for (const DescentStep& step : path.descent) {
    reached = block.Add(StepForRead(unit, block, step, reached));
  }
  return reached;
}

// Whether the access a check stands before reads the member or writes it,
// which the check's message names (LRM 11.9).
enum class CheckedAccess : std::uint8_t { kRead, kWrite };

auto AccessText(CheckedAccess access) -> std::string_view {
  switch (access) {
    case CheckedAccess::kRead:
      return "read of";
    case CheckedAccess::kWrite:
      return "write to";
  }
  throw InternalError("access path: unknown access");
}

// The checks a read or a write along `path` owes before it is made: one for
// each step into a member of a tagged union, over the value the steps before it
// reach, read where it lies (LRM 11.9). They stand where the path is taken, so
// a write that lands after the statement naming it -- a nonblocking update, an
// output argument copied back -- is checked when it lands.
void AppendTagChecks(
    mir::CompilationUnit& unit, mir::Builder block, const AccessPath& path,
    CheckedAccess access) {
  for (std::size_t taken = 0; taken < path.descent.size(); ++taken) {
    const std::optional<TagRequirement> tag =
        TagRequiredBy(path.descent[taken]);
    if (!tag.has_value()) {
      continue;
    }
    const AccessPath before{
        .owner = path.owner,
        .descent = {
            path.descent.begin(),
            path.descent.begin() + static_cast<std::ptrdiff_t>(taken)}};
    const mir::ExprId holds = BuildTagMatches(
        unit, block, *tag, PlaceForRead(unit, block, before),
        hir::CaseKeyword::kCase);
    mir::AppendRunTimeCheck(
        unit, block, holds,
        base::DiagMessage(
            "{} a tagged union member inconsistent with the current tag",
            AccessText(access))
            .TakeText());
  }
}

}  // namespace

auto BuildTagMatches(
    mir::CompilationUnit& unit, mir::Builder block, const TagRequirement& tag,
    mir::ExprId into, hir::CaseKeyword keyword) -> mir::ExprId {
  return std::visit(
      Overloaded{
          // The tag is the most significant bits of the union's own vector
          // (LRM 7.3.2), so where it starts and the state domain it is read
          // in are the union's.
          [&](const TagInBits& bits) {
            const mir::IntegralType& vector =
                unit.types.Get(block.Get(into).type).Integral();
            const mir::TypeId tag_type = mir::PackedVectorOf(
                unit.types, bits.tag_bits, vector.state_kind);
            const DescentStep to_tag{
                .to =
                    StepToBits{
                        .start = BuildConstantPosition(
                            unit, block,
                            static_cast<std::int64_t>(
                                vector.bit_width - bits.tag_bits))},
                .part_type = tag_type};
            const mir::ExprId carried =
                block.Add(StepRead(unit, block, to_tag, into));
            const mir::ExprId named = mir::BuildIntLiteral(
                unit, block, static_cast<std::int64_t>(bits.member.value));
            return ReduceToCondition(
                unit, block,
                BuildCaseComparison(
                    unit, block, keyword, carried,
                    ConvertToType(unit, block, named, tag_type)));
          },
          // A tag held apart is set only by building a tagged value, so it
          // carries no x or z, and every way of comparing it answers alike.
          [&](const HeldTag& held) {
            const mir::ExprId held_now = block.Add(
                mir::MakeDiscriminantExpr(into, unit.builtins.machine_int64));
            return block.Add(
                mir::MakeBinary(
                    unit, block.Body(), mir::BinaryOp::kEquality, held_now,
                    mir::BuildMachineIntLiteral(
                        unit, block,
                        static_cast<std::int64_t>(held.member.value)),
                    unit.builtins.machine_bool));
          }},
      tag);
}

void AppendReadChecks(
    mir::CompilationUnit& unit, mir::Builder block, const AccessPath& path) {
  AppendTagChecks(unit, block, path, CheckedAccess::kRead);
}

auto PathPlace(
    mir::CompilationUnit& unit, mir::Builder block, const AccessPath& path)
    -> mir::ExprId {
  AppendTagChecks(unit, block, path, CheckedAccess::kWrite);
  return ReachedForWrite(unit, block, path, WriteEnd::kLanded);
}

namespace {

// A reference to the whole of what `owner` holds: the place itself, or, for a
// property, a step taken on its object.
auto OwnerReference(
    mir::CompilationUnit& unit, mir::Builder block, const PathOwner& owner)
    -> mir::ExprId {
  return std::visit(
      Overloaded{
          [&](mir::ExprId place) {
            return BuildReferenceArg(unit, block, place, block.Get(place).type);
          },
          [&](const ObjectProperty& property) {
            return PropertyReference(
                unit, block, property.object, property.property, property.type);
          }},
      owner);
}

// `step` taken on `reference` for a reference to the part.
auto LentStep(
    mir::CompilationUnit& unit, mir::Builder block, const DescentStep& step,
    mir::ExprId reference) -> mir::ExprId {
  return TakeStep(
      unit, block, step, StepUse::kLend, reference,
      unit.types.Intern(
          mir::Type{mir::VariableRefType{
              .pointee = step.part_type,
              .mutability = mir::Mutability::kMutable}}));
}

// Whether a value of `container` is a dynamically sized array, whose elements
// it can let go of while a call leases one: a queue's or an associative array's
// element, which a removal ends, or a dynamic array's elements, which a resize
// or an assignment replaces (LRM 13.5.2). A fixed-size array's elements live
// as long as the array does.
auto IsDynamicallySized(const mir::Type& container) -> bool {
  return container.Is<mir::QueueType>() ||
         container.Is<mir::DynamicArrayType>() ||
         container.Is<mir::AssociativeArrayType>();
}

// The leasing step a call's binding takes where `step` reaches an element of a
// dynamically sized `container`, named by how the step names the element; none
// for any other step.
struct ElementLease {
  support::BuiltinFn entry;
  mir::ExprId coordinate;
};
auto ElementLeaseFor(const DescentStep& step, const mir::Type& container)
    -> std::optional<ElementLease> {
  if (!IsDynamicallySized(container)) {
    return std::nullopt;
  }
  return std::visit(
      Overloaded{
          [](const StepToElement& element) -> std::optional<ElementLease> {
            return ElementLease{
                .entry = support::BuiltinFn::kReferElementLeased,
                .coordinate = element.position};
          },
          [](const StepToAssocElement& element) -> std::optional<ElementLease> {
            return ElementLease{
                .entry = support::BuiltinFn::kAssocReferElementLeased,
                .coordinate = element.key};
          },
          [](const StepToBits&) -> std::optional<ElementLease> {
            return std::nullopt;
          },
          [](const StepToElementRun&) -> std::optional<ElementLease> {
            return std::nullopt;
          },
          [](const StepToCharacter&) -> std::optional<ElementLease> {
            return std::nullopt;
          },
          [](const StepToQueueRun&) -> std::optional<ElementLease> {
            return std::nullopt;
          },
          [](const StepToComponent&) -> std::optional<ElementLease> {
            return std::nullopt;
          },
          [](const StepToUnionMember&) -> std::optional<ElementLease> {
            return std::nullopt;
          },
          [](const StepToTaggedMember&) -> std::optional<ElementLease> {
            return std::nullopt;
          }},
      step.to);
}

// The reference to `pointee` the leased reference `leased` carries.
auto CarriedReference(
    mir::CompilationUnit& unit, mir::Builder block, mir::ExprId leased,
    mir::TypeId pointee) -> mir::ExprId {
  return block.Add(
      mir::MakeBuiltinCall(
          unit, block.Body(), support::BuiltinFn::kLeasedReference, leased,
          std::nullopt, {pointee}, {},
          unit.types.Intern(
              mir::Type{mir::VariableRefType{
                  .pointee = pointee,
                  .mutability = mir::Mutability::kMutable}})));
}

}  // namespace

auto PathReference(
    mir::CompilationUnit& unit, mir::Builder block, const AccessPath& path)
    -> mir::ExprId {
  mir::ExprId reference = OwnerReference(unit, block, path.owner);
  for (const DescentStep& step : path.descent) {
    reference = LentStep(unit, block, step, reference);
  }
  return reference;
}

auto PathBoundReference(
    mir::CompilationUnit& unit, mir::Builder block, const AccessPath& path,
    const HoldLease& hold) -> mir::ExprId {
  const mir::TypeId leased = unit.types.Intern(
      mir::Type{mir::RuntimeLibraryType{
          .kind = mir::RuntimeLibraryKind::kLeasedReference}});
  const mir::ExprId owned = OwnerReference(unit, block, path.owner);
  mir::ExprId reference = std::visit(
      Overloaded{
          [&](mir::ExprId) { return owned; },
          [&](const ObjectProperty& property) {
            return CarriedReference(
                unit, block,
                hold(block.Add(
                    mir::MakeBuiltinCall(
                        unit, block.Body(), support::BuiltinFn::kLeaseObject,
                        std::nullopt, {owned}, leased))),
                property.type);
          }},
      path.owner);
  for (const DescentStep& step : path.descent) {
    const mir::Type& container =
        unit.types.Get(unit.types.Get(block.Get(reference).type)
                           .Get<mir::VariableRefType>()
                           .pointee);
    if (const std::optional<ElementLease> lease =
            ElementLeaseFor(step, container)) {
      reference = CarriedReference(
          unit, block,
          hold(block.Add(
              mir::MakeBuiltinCall(
                  unit, block.Body(), lease->entry, reference,
                  {lease->coordinate}, leased))),
          step.part_type);
    } else {
      reference = LentStep(unit, block, step, reference);
    }
  }
  return reference;
}

auto PathAsPlace(
    mir::CompilationUnit& unit, mir::Builder block, const AccessPath& path)
    -> mir::ExprId {
  AppendTagChecks(unit, block, path, CheckedAccess::kRead);
  return PlaceForRead(unit, block, path);
}

auto PathValue(
    mir::CompilationUnit& unit, mir::Builder block, const AccessPath& path)
    -> mir::ExprId {
  return block.AsValue(unit.types, PathAsPlace(unit, block, path));
}

auto StepForRead(
    mir::CompilationUnit& unit, mir::Builder block, const DescentStep& step,
    mir::ExprId receiver) -> mir::Expr {
  return StepExpr(unit, block, step, StepUse::kRead, receiver, step.part_type);
}

auto ElementReference(
    mir::CompilationUnit& unit, mir::Builder block, const DescentStep& step,
    mir::ExprId receiver) -> mir::Expr {
  StepTaken taken = StepTakenFor(unit, block, step, StepUse::kRead);
  auto* element = std::get_if<ReferenceStep>(&taken);
  if (element == nullptr) {
    throw InternalError(
        "access path: a reference is answered by an element's read form, and "
        "this step reaches no element");
  }
  return ReferenceStepCall(unit, std::move(*element), receiver, step.part_type);
}

auto StepRead(
    mir::CompilationUnit& unit, mir::Builder block, const DescentStep& step,
    mir::ExprId receiver) -> mir::Expr {
  return block.AsValue(unit.types, StepForRead(unit, block, step, receiver));
}

auto PartSelectNaturalType(
    mir::CompilationUnit& unit, mir::TypeId source_type, mir::TypeId part_type)
    -> mir::TypeId {
  const auto& source = unit.types.Get(source_type);
  const auto& part = unit.types.Get(part_type);
  if (!source.IsIntegral() || !part.IsIntegral()) {
    return part_type;
  }
  mir::IntegralType natural = part.Integral();
  natural.signedness = mir::Signedness::kUnsigned;
  natural.state_kind = source.Integral().state_kind;
  return unit.types.Intern(mir::Type{natural});
}

auto PathValueAsDeclared(
    mir::CompilationUnit& unit, mir::Builder block, const AccessPath& path)
    -> mir::ExprId {
  // A path that descends nowhere reads the whole of what its owner holds,
  // which is a value of its own.
  if (path.descent.empty()) {
    return PathValue(unit, block, path);
  }
  AppendTagChecks(unit, block, path, CheckedAccess::kRead);
  AccessPath into = path;
  DescentStep last = std::move(into.descent.back());
  into.descent.pop_back();
  const mir::TypeId declared = last.part_type;
  last.part_type = PartSelectNaturalType(
      unit, ValueSelectedWithin(unit, block.Body(), path), declared);
  const mir::ExprId receiver = PlaceForRead(unit, block, into);
  const mir::ExprId read = block.Add(StepRead(unit, block, last, receiver));
  return ConvertToType(unit, block, read, declared);
}

auto SettledPlace(
    const UnitLowerer& unit_lowerer, const WalkFrame& frame, mir::ExprId place)
    -> mir::ExprId {
  mir::Builder block = frame.Builder();
  if (mir::EvaluatesNothing(block.Body(), place)) {
    return place;
  }
  const mir::Expr node = block.Get(place);
  // A pointer or a reference a computation yields is a value, and holding it in
  // a local names the same object. Any other value a computation yields names
  // no storage a place could be formed over again, and no path is rooted at
  // one.
  const auto computed = [&]() -> mir::ExprId {
    const mir::Type& yielded = unit_lowerer.Unit().types.Get(node.type);
    if (yielded.Is<mir::PointerType>() || yielded.Is<mir::ReferenceType>()) {
      return EvaluatedOnce(frame, place);
    }
    throw InternalError(
        "access path: a place named at more than one point is a name, or a "
        "field or a dereference over what it is reached through, and this one "
        "is a value a computation yields");
  };
  return std::visit(
      Overloaded{
          [&](const mir::FieldAccessExpr& field) -> mir::ExprId {
            return block.Add(
                mir::MakeFieldAccessExpr(
                    SettledPlace(unit_lowerer, frame, field.receiver),
                    field.field, node.type));
          },
          [&](const mir::DerefExpr& deref) -> mir::ExprId {
            const mir::TypeId reached_through = block.Get(deref.pointer).type;
            // A capability wrapper is storage, so what it is reached through
            // is settled and the wrapper stays where it is; a pointer or a
            // handle is a value, and holding it in a local names the same
            // object.
            if (unit_lowerer.Unit()
                    .types.Get(reached_through)
                    .IsCapabilityWrapper()) {
              return block.Add(
                  mir::MakeDerefExpr(
                      SettledPlace(unit_lowerer, frame, deref.pointer),
                      node.type));
            }
            return block.Add(
                mir::MakeDerefExpr(
                    EvaluatedOnce(frame, deref.pointer), node.type));
          },
          [&](const mir::StringLiteral&) { return computed(); },
          [&](const mir::NullLiteral&) { return computed(); },
          [&](const mir::MachineBoolLiteral&) { return computed(); },
          [&](const mir::MachineIntLiteral&) { return computed(); },
          [&](const mir::MachineFloatLiteral&) { return computed(); },
          [&](const mir::ReferenceExpr&) { return computed(); },
          [&](const mir::AddressOfExpr&) { return computed(); },
          [&](const mir::BorrowExpr&) { return computed(); },
          [&](const mir::CopyExpr&) { return computed(); },
          [&](const mir::DiscriminantExpr&) { return computed(); },
          [&](const mir::UnaryExpr&) { return computed(); },
          [&](const mir::BinaryExpr&) { return computed(); },
          [&](const mir::CastExpr&) { return computed(); },
          [&](const mir::ConditionalExpr&) { return computed(); },
          [&](const mir::BlockExpr&) { return computed(); },
          [&](const mir::AssignExpr&) { return computed(); },
          [&](const mir::CallExpr&) { return computed(); },
          [&](const mir::NewExpr&) { return computed(); },
          [&](const mir::MoveExpr&) { return computed(); },
          [&](const mir::ClosureExpr&) { return computed(); },
          [&](const mir::CompositeExpr&) { return computed(); },
          [&](const mir::UnionExpr&) { return computed(); },
          [&](const mir::AwaitExpr&) { return computed(); },
          [&](const mir::WaitExpr&) { return computed(); },
          [&](const mir::VectorGetExpr&) { return computed(); }},
      node.data);
}

auto SettledOwner(
    const UnitLowerer& unit_lowerer, const WalkFrame& frame,
    const PathOwner& owner) -> PathOwner {
  return std::visit(
      Overloaded{
          [&](mir::ExprId place) -> PathOwner {
            return SettledPlace(unit_lowerer, frame, place);
          },
          [&](const ObjectProperty& property) -> PathOwner {
            return ObjectProperty{
                .object = EvaluatedOnce(frame, property.object),
                .property = property.property,
                .type = property.type};
          }},
      owner);
}

auto Settled(
    const UnitLowerer& unit_lowerer, const WalkFrame& frame, AccessPath path)
    -> AccessPath {
  path.owner = SettledOwner(unit_lowerer, frame, path.owner);
  for (DescentStep& step : path.descent) {
    ForEachOperand(step, [&](mir::ExprId& operand) {
      operand = EvaluatedOnce(frame, operand);
    });
  }
  return path;
}

void SettleForTagChecks(
    const UnitLowerer& unit_lowerer, const WalkFrame& frame,
    AccessPath& target) {
  std::size_t last_checked = 0;
  for (std::size_t taken = 0; taken < target.descent.size(); ++taken) {
    if (TagRequiredBy(target.descent[taken]).has_value()) {
      last_checked = taken + 1;
    }
  }
  if (last_checked == 0) {
    return;
  }
  target.owner = SettledOwner(unit_lowerer, frame, target.owner);
  for (std::size_t settled = 0; settled + 1 < last_checked; ++settled) {
    ForEachOperand(target.descent[settled], [&](mir::ExprId& operand) {
      operand = EvaluatedOnce(frame, operand);
    });
  }
}

auto SettledForRead(
    const UnitLowerer& unit_lowerer, const WalkFrame& frame, AccessPath path)
    -> SettledPath {
  AccessPath settled = Settled(unit_lowerer, frame, std::move(path));
  return SettledPath{
      .named_in = frame.current_block,
      .owner = settled.owner,
      .descent = std::move(settled.descent)};
}

auto NamedIn(const SettledPath& settled, mir::Builder to) -> AccessPath {
  const mir::Block& from = *settled.named_in;
  AccessPath path{.owner = settled.owner, .descent = settled.descent};
  // Naming a node again reads it out of one pool while adding to the other, so
  // the two are different pools; a path already named in `to` is the answer.
  if (&from == &to.Body()) {
    return path;
  }
  const auto again = [&](mir::ExprId id) { return NamedAgain(from, to, id); };
  path.owner = std::visit(
      Overloaded{
          [&](mir::ExprId place) -> PathOwner { return again(place); },
          [&](const ObjectProperty& property) -> PathOwner {
            return ObjectProperty{
                .object = again(property.object),
                .property = property.property,
                .type = property.type};
          }},
      path.owner);
  for (DescentStep& step : path.descent) {
    ForEachOperand(step, [&](mir::ExprId& operand) {
      operand = NamedAgain(from, to, operand);
    });
  }
  return path;
}

auto ReachesOverlaidStorage(const AccessPath& path) -> bool {
  return std::ranges::any_of(path.descent, [](const DescentStep& step) {
    return std::holds_alternative<StepToUnionMember>(step.to);
  });
}

auto ReadThenWrite(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, AccessPath place)
    -> ReadThenWritten {
  AccessPath settled = Settled(unit_lowerer, frame, std::move(place));
  const mir::ExprId incoming =
      PathValueAsDeclared(unit_lowerer.Unit(), frame.Builder(), settled);
  return ReadThenWritten{.place = std::move(settled), .incoming = incoming};
}

auto BitsWithinOwner(
    mir::CompilationUnit& unit, mir::Builder block, const AccessPath& path)
    -> PathPositions {
  mir::ExprId first = BuildConstantPosition(unit, block, 0);
  for (const DescentStep& step : path.descent) {
    const auto* bits = std::get_if<StepToBits>(&step.to);
    if (bits == nullptr) {
      throw InternalError(
          "access path: a part of a packed value is reached by steps naming "
          "bits from one start, and this descent takes a step that is not one");
    }
    first = BuildPositionSum(unit, block, first, bits->start);
  }
  const mir::TypeId part = PathValueType(unit, block.Body(), path);
  return PathPositions{
      .first = first, .width = unit.types.Get(part).Integral().bit_width};
}

auto PartNamedBy(
    mir::CompilationUnit& unit, mir::Builder block, const AccessPath& path)
    -> NamedPart {
  NamedPart named;
  auto step = path.descent.begin();
  for (; step != path.descent.end(); ++step) {
    const std::optional<mir::ExprId> key = std::visit(
        Overloaded{
            [&](const StepToElement& element) -> std::optional<mir::ExprId> {
              return BuildToInt64Call(unit, block, element.position);
            },
            [&](const StepToCharacter& character)
                -> std::optional<mir::ExprId> {
              return BuildToInt64Call(unit, block, character.position);
            },
            [&](const StepToComponent& component)
                -> std::optional<mir::ExprId> {
              return mir::BuildMachineIntLiteral(
                  unit, block, component.position.value);
            },
            [&](const StepToUnionMember& member) -> std::optional<mir::ExprId> {
              return mir::BuildMachineIntLiteral(
                  unit, block, member.position.value);
            },
            [&](const StepToTaggedMember& member)
                -> std::optional<mir::ExprId> {
              return mir::BuildMachineIntLiteral(
                  unit, block, member.position.value);
            },
            [](const StepToBits&) -> std::optional<mir::ExprId> {
              return std::nullopt;
            },
            [](const StepToElementRun&) -> std::optional<mir::ExprId> {
              return std::nullopt;
            },
            [](const StepToAssocElement&) -> std::optional<mir::ExprId> {
              return std::nullopt;
            },
            [](const StepToQueueRun&) -> std::optional<mir::ExprId> {
              return std::nullopt;
            }},
        step->to);
    if (!key.has_value()) {
      break;
    }
    named.steps.push_back(*key);
  }
  if (step == path.descent.end()) {
    return named;
  }
  // A run of elements ending the path is those elements of the array the steps
  // reach (LRM 7.4.6).
  if (const auto* run = std::get_if<StepToElementRun>(&step->to);
      run != nullptr && std::next(step) == path.descent.end()) {
    named.positions = PathPositions{.first = run->start, .width = run->count};
    return named;
  }
  // What is left names bits of the packed value reached, each step from the
  // start of the one before it, or goes on in a way no name follows.
  mir::ExprId first = BuildConstantPosition(unit, block, 0);
  for (; step != path.descent.end(); ++step) {
    const auto* bits = std::get_if<StepToBits>(&step->to);
    if (bits == nullptr) {
      named.exact = false;
      return named;
    }
    first = BuildPositionSum(unit, block, first, bits->start);
  }
  const mir::TypeId part = PathValueType(unit, block.Body(), path);
  named.positions = PathPositions{
      .first = first, .width = unit.types.Get(part).Integral().bit_width};
  return named;
}

namespace {

// Where in `path` the step to a character of a string is, if it takes one.
auto CharacterAt(const AccessPath& path) -> std::optional<std::size_t> {
  for (std::size_t at = 0; at < path.descent.size(); ++at) {
    if (std::holds_alternative<StepToCharacter>(path.descent[at].to)) {
      return at;
    }
  }
  return std::nullopt;
}

// A write through the character at `at`: the string's `putc`, the write
// indexing a string is defined as (LRM Table 6-9, 6.16.2), putting `value`, or
// where the write names bits of the character (LRM 11.5.1), the character the
// string holds with those bits replaced. That reads the path before the write
// lands, which is why such a path evaluates nothing of its own.
auto PutCharacterExpr(
    mir::CompilationUnit& unit, mir::Builder block, const AccessPath& path,
    std::size_t at, mir::ExprId value) -> mir::Expr {
  const AccessPath string{
      .owner = path.owner,
      .descent = {
          path.descent.begin(),
          path.descent.begin() + static_cast<std::ptrdiff_t>(at)}};
  const auto& character = std::get<StepToCharacter>(path.descent[at].to);
  mir::ExprId put = value;
  // A character is a `byte`, so what follows it in a write names its bits,
  // once.
  if (at + 1 < path.descent.size()) {
    const auto* bits = std::get_if<StepToBits>(&path.descent[at + 1].to);
    if (bits == nullptr || at + 2 < path.descent.size()) {
      throw InternalError(
          "access path: a character is a byte, so a write names its bits "
          "once or not at all, and this descent takes another step");
    }
    const mir::ExprId held = block.AsValue(
        unit.types,
        PlaceForRead(unit, block, DescendInto(string, path.descent[at])));
    put = block.Add(
        mir::MakeBuiltinCall(
            unit, block.Body(), support::BuiltinFn::kWithSlice, std::nullopt,
            {held, bits->start, value}, block.Get(held).type));
  }
  return mir::MakeBuiltinCall(
      unit, block.Body(), support::BuiltinFn::kPutc,
      ReachedForWrite(unit, block, string, WriteEnd::kLanded),
      {character.position, put}, unit.builtins.void_type);
}

// A write of `value` into the run of elements `path` ends in: the container's
// own slice write, the run being several elements rather than one storage (LRM
// 7.4.6, 7.6) -- an array's from where it starts and how many it takes, a
// queue's between the two positions it clamps (LRM 7.10.1). Absent where the
// path ends in no run.
auto AssignSliceExpr(
    mir::CompilationUnit& unit, mir::Builder block, const AccessPath& path,
    mir::ExprId value) -> std::optional<mir::Expr> {
  if (path.descent.empty()) {
    return std::nullopt;
  }
  const auto write = [&](support::BuiltinFn entry,
                         std::vector<mir::ExprId> bounds) {
    bounds.push_back(value);
    return mir::MakeBuiltinCall(
        unit, block.Body(), entry,
        WriteOperandOf(unit, block, path, path.descent.size() - 1),
        std::move(bounds), unit.builtins.void_type);
  };
  using Write = std::optional<mir::Expr>;
  return std::visit(
      Overloaded{
          [&](const StepToElementRun& run) -> Write {
            return write(
                support::BuiltinFn::kAssignElementSlice,
                {run.start, mir::BuildMachineCount(unit, block, run.count)});
          },
          [&](const StepToQueueRun& run) -> Write {
            return write(
                support::BuiltinFn::kAssignQueueSlice,
                {run.lowest, run.highest});
          },
          [](const StepToBits&) -> Write { return std::nullopt; },
          [](const StepToElement&) -> Write { return std::nullopt; },
          [](const StepToAssocElement&) -> Write { return std::nullopt; },
          [](const StepToCharacter&) -> Write { return std::nullopt; },
          [](const StepToComponent&) -> Write { return std::nullopt; },
          [](const StepToUnionMember&) -> Write { return std::nullopt; },
          [](const StepToTaggedMember&) -> Write { return std::nullopt; }},
      path.descent.back().to);
}

// Where the bits `path` ends in start among its steps, if it ends in bits:
// the first of the steps to bits after the last step to anything else.
auto TrailingBitsAt(const AccessPath& path) -> std::optional<std::size_t> {
  std::size_t at = path.descent.size();
  while (at > 0 &&
         std::holds_alternative<StepToBits>(path.descent[at - 1].to)) {
    --at;
  }
  if (at == path.descent.size()) {
    return std::nullopt;
  }
  return at;
}

// A write of `value` into the bits `path` ends in, from the step `at`: a
// reference to them taken on the value they lie in, one part at a time, and
// the reference's `set` (LRM 11.5.1), as `std::bitset`'s `b[i] = x` assigns
// through the reference `operator[]` answers.
auto StoreBitsExpr(
    mir::CompilationUnit& unit, mir::Builder block, const AccessPath& path,
    std::size_t at, mir::ExprId value) -> mir::Expr {
  mir::ExprId reference = WriteOperandOf(unit, block, path, at);
  for (std::size_t i = at; i < path.descent.size(); ++i) {
    const DescentStep& step = path.descent[i];
    reference = block.Add(
        mir::MakeBuiltinCall(
            unit, block.Body(), support::BuiltinFn::kSliceMut, reference,
            std::nullopt, {step.part_type},
            {std::get<StepToBits>(step.to).start},
            unit.types.Intern(
                mir::Type{mir::BitsReferenceType{.bits = step.part_type}})));
  }
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{
                      .target = support::BuiltinFn::kSet,
                      .receiver = reference},
              .arguments = {value}},
      .type = unit.builtins.void_type};
}

}  // namespace

auto WritesBitsOfACharacter(const AccessPath& path) -> bool {
  const std::optional<std::size_t> at = CharacterAt(path);
  return at.has_value() && *at + 1 < path.descent.size();
}

auto BuildStoreExpr(
    mir::CompilationUnit& unit, mir::Builder block, const AccessPath& path,
    mir::ExprId rhs_id) -> mir::Expr {
  // A store carries the right-hand side to the destination's declared
  // representation (LRM 10.7, 10.8).
  rhs_id = ConvertToType(
      unit, block, rhs_id, PathValueType(unit, block.Body(), path));
  AppendTagChecks(unit, block, path, CheckedAccess::kWrite);
  // Replacing the whole of what a capability wrapper holds acts on the wrapper
  // -- the value lands in its storage and it reports the change to whatever is
  // watching -- so it is a call taking the wrapper as its destination. A store
  // that descends writes a part, which reaches storage the way a read does and
  // assigns through what it reaches.
  const std::optional<mir::ExprId> wrapper =
      StoredWrapper(unit, block.Body(), path.owner);
  if (path.descent.empty() && wrapper.has_value()) {
    // The operands are the destination and the value, and nothing else: the
    // engine the wrapper reports through is the ambient one, which has the
    // standing of a stack pointer rather than of program data.
    return mir::Expr{
        .data =
            mir::CallExpr{
                .callee =
                    mir::Direct{
                        .target = support::BuiltinFn::kSet,
                        .receiver = wrapper},
                .arguments = {rhs_id}},
        .type = unit.builtins.void_type};
  }
  if (const std::optional<std::size_t> at = CharacterAt(path)) {
    return PutCharacterExpr(unit, block, path, *at, rhs_id);
  }
  if (std::optional<mir::Expr> slice =
          AssignSliceExpr(unit, block, path, rhs_id)) {
    return *std::move(slice);
  }
  if (const std::optional<std::size_t> at = TrailingBitsAt(path)) {
    return StoreBitsExpr(unit, block, path, *at, rhs_id);
  }
  return mir::MakeAssignExpr(
      unit.builtins, ReachedForWrite(unit, block, path, WriteEnd::kLanded),
      rhs_id);
}

}  // namespace lyra::lowering::hir_to_mir
