#pragma once

#include <cstdint>
#include <functional>
#include <optional>
#include <variant>
#include <vector>

#include "lyra/base/component_index.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/case_keyword.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// A tag carried in bits of a packed union's own vector, as a step into one of
// its members requires it (LRM 7.3.2, 11.9): the `tag_bits` most significant
// bits hold the declaration-order position of `member`.
//
//   typedef union tagged packed { logic [3:0] a; logic [3:0] b; } u_t;
//   u.b    a step valid only while the one tag bit of `u` is 1
struct TagInBits {
  std::uint32_t tag_bits = 0;
  base::ComponentIndex member = {};
};

// What a step into a member of an unpacked tagged union requires of the union
// (LRM 7.3.2, 11.9): the tag it holds as its own names `member`.
struct HeldTag {
  base::ComponentIndex member;
};

// The tag the value a step is taken into has to hold for the step to be valid,
// as the union carries it: bits of a packed union's vector, or held by an
// unpacked one.
using TagRequirement = std::variant<TagInBits, HeldTag>;

// Bits of an integral value (LRM 11.5.1): where they start in the value's own
// numbering, as many as the part's type is wide. `required_tag` is what the
// value has to hold for the step to be valid, where the bits are a member of a
// tagged union; a read or a write taking the step checks it first, and the step
// itself checks nothing.
struct StepToBits {
  mir::ExprId start;
  std::optional<TagInBits> required_tag = std::nullopt;
};

// Elements of a fixed-size or dynamic array in a row (LRM 7.4.6): where they
// start in the array's own numbering, and how many there are. The part's type
// fixes the count, so it is a number rather than a value the program computes.
struct StepToElementRun {
  mir::ExprId start;
  std::uint64_t count = 0;
};

// One element of a container that numbers its elements, at the position the
// program computes (LRM 7.4.5, 7.10.1).
struct StepToElement {
  mir::ExprId position;
};

// One element of an associative array, at the key the program computes: a
// value of the type the source wrote it in, which names no place in any order
// (LRM 7.8).
struct StepToAssocElement {
  mir::ExprId key;
};

// One character of a string, at the position the program computes (LRM 6.16).
// A character has no storage of its own: it is read by the string's `getc`
// and written by its `putc`, the operations indexing a string is defined as
// (LRM Table 6-9).
struct StepToCharacter {
  mir::ExprId position;
};

// The elements of a queue between two positions the running program can move
// (LRM 7.10.1). A read of them builds a queue of its own and a write is the
// queue's own slice write, so no step lands a write in them and no reference
// names them (LRM 13.5.2).
struct StepToQueueRun {
  mir::ExprId lowest;
  mir::ExprId highest;
};

// A component of a product by its declaration-order position (LRM 7.2): a
// member of an unpacked structure, or a component of a tuple the lowering
// composes. Each is storage of its own, so reading it and writing it are the
// one field step on the place the product lies in.
struct StepToComponent {
  base::ComponentIndex position;
};

// A member of an untagged unpacked union, by its declaration-order position
// (LRM 7.3). Every member lies at the start of the union's storage, so reading
// it and writing it are the one field step on the place the union lies in, as
// a component's are; but it has no identity of its own, so a write in progress
// lands on the union before taking it, and a read of it reads storage another
// member may have written.
struct StepToUnionMember {
  base::ComponentIndex position;
};

// A member of an unpacked tagged union, by its declaration-order position, as
// rustc's downcast to a variant is a projection of its own beside a field's.
// It is taken as an untagged member is, and is valid only while the tag names
// the member (LRM 11.9), which a read or a write taking it checks first; so it
// never reads what another member wrote.
struct StepToTaggedMember {
  base::ComponentIndex position;
};

// One step of a descent into a value: which kind of part it reaches, and the
// part's type.
//
// The kind follows from the type of the value the step descends into, and this
// is the layer that holds that type, so it is settled here. Which library
// entry takes a step of a kind for each thing a path is taken for -- a read, a
// write, a reference -- is a fact of the kind, stated once where steps are
// realized.
struct DescentStep {
  std::variant<
      StepToBits, StepToElementRun, StepToElement, StepToAssocElement,
      StepToCharacter, StepToQueueRun, StepToComponent, StepToUnionMember,
      StepToTaggedMember>
      to;
  mir::TypeId part_type;
};

// Each value the program computes that `step` is handed -- a start, a
// coordinate, a bound -- as `visit` may replace it. A deferred write freezes
// these where the statement is reached (LRM 10.4.2).
template <typename Visit>
void ForEachOperand(DescentStep& step, Visit visit) {
  std::visit(
      Overloaded{
          [&](StepToBits& bits) { visit(bits.start); },
          [&](StepToElementRun& run) { visit(run.start); },
          [&](StepToElement& element) { visit(element.position); },
          [&](StepToAssocElement& element) { visit(element.key); },
          [&](StepToCharacter& character) { visit(character.position); },
          [&](StepToQueueRun& run) {
            visit(run.lowest);
            visit(run.highest);
          },
          [](StepToComponent&) {}, [](StepToUnionMember&) {},
          [](StepToTaggedMember&) {}},
      step.to);
}

// The tag `step` requires of the value it is taken into, where it reaches a
// member of a tagged union, packed or not, and so is valid only while the
// union's tag names that member (LRM 11.9).
[[nodiscard]] inline auto TagRequiredBy(const DescentStep& step)
    -> std::optional<TagRequirement> {
  const auto none = [] { return std::optional<TagRequirement>{}; };
  return std::visit(
      Overloaded{
          [](const StepToBits& bits) -> std::optional<TagRequirement> {
            return bits.required_tag;
          },
          [&](const StepToElementRun&) { return none(); },
          [&](const StepToElement&) { return none(); },
          [&](const StepToAssocElement&) { return none(); },
          [&](const StepToCharacter&) { return none(); },
          [&](const StepToQueueRun&) { return none(); },
          [&](const StepToComponent&) { return none(); },
          [&](const StepToUnionMember&) { return none(); },
          [](const StepToTaggedMember& member)
              -> std::optional<TagRequirement> {
            return HeldTag{.member = member.position};
          }},
      step.to);
}

// Whether the tag of the union `into` names the member `tag` requires, as a
// machine boolean, its bits compared as a case statement of `keyword` compares
// two values, so that under `case` a tag carrying x or z names no member.
[[nodiscard]] auto BuildTagMatches(
    mir::CompilationUnit& unit, mir::Block& block, const TagRequirement& tag,
    mir::ExprId into, hir::CaseKeyword keyword) -> mir::ExprId;

// A property of an object as what owns a value (LRM 8.4): the object, as the
// expression the source reaches it through -- a class handle, or the running
// method's own object -- and which of its properties. The object is kept apart
// from the storage the property occupies because what a path is taken for
// decides how the object is reached: a write opens a write on the object and
// reaches the property through it, a reference is a step taken on the object,
// and a read reads the member. Either way the object is what is told that it
// was written (LRM 9.4.2).
struct ObjectProperty {
  mir::ExprId object;
  mir::ClassFieldTarget property;
  mir::TypeId type;
};

// What owns the value a path descends into: a place -- a variable, a cell, a
// reference, a field of the running scope -- or a property of an object.
using PathOwner = std::variant<mir::ExprId, ObjectProperty>;

// A part of a value named from its owner: what owns the whole value, and the
// descent that reaches the part. A write lands in it, a reference is formed
// over it, a wait watches it and a join of nets covers it, and each is handed
// this one statement of which part it is. A path that names no part descends
// nowhere and is its owner's own place.
//
// Taking a path for a read or a write checks each step into a member of a
// tagged union first (LRM 11.9), by a statement appended to the block that
// names the steps before it again, so what those compute is settled before the
// path is taken.
//
// This is the lowering's own shape and reaches no layer below. What MIR carries
// is what the descent lowers to -- for each step a field step, the dereference
// of a call answering a reference to the part, or an ordinary call naming its
// own entry, composed through the receiver -- because a consumer that met the
// descent itself would have to decide which operation each step is, which is
// the decision this layer is here to make.
struct AccessPath {
  PathOwner owner;
  std::vector<DescentStep> descent;
};

// The place a path's owner is, for a construct whose owner is never a property
// of an object: the nets a join covers.
[[nodiscard]] auto OwnerPlace(const AccessPath& path) -> mir::ExprId;

// The type of the value `owner` holds: what a capability wrapper stands for,
// the type of any other place, or the property's own type.
[[nodiscard]] auto OwnerValueType(
    const mir::CompilationUnit& unit, const mir::Block& block,
    const PathOwner& owner) -> mir::TypeId;

// The same path one step deeper. This is the only thing that builds a descent,
// so the path gains exactly one step per level of the source's own nesting and
// the owner is whatever the peel reached that was not a descent.
[[nodiscard]] auto DescendInto(AccessPath base, DescentStep step) -> AccessPath;

// The type of the value a further step would descend into: the part the descent
// has reached so far, or what the owner's place holds where it has reached
// none.
[[nodiscard]] auto PathValueType(
    const mir::CompilationUnit& unit, const mir::Block& block,
    const AccessPath& path) -> mir::TypeId;

// The place the path designates: the owner's own storage, then one step per
// part. Storing into the result writes the part, and applying a method
// that changes its receiver to it changes the part, because a place is what
// both take.
//
// An owner that is a capability wrapper is opened for a write, since asking one
// which storage it stands for is an operation on it. The steps into parts that
// are storage of their own are then taken within the write in progress,
// starting from the whole of what it designates, which is how it learns what
// forming each did, and the place is where the last of them is dereferenced.
// An owner that is a property of an object is reached through a write opened on
// the object alone, as the member of the object the write answers.
[[nodiscard]] auto PathPlace(
    mir::CompilationUnit& unit, mir::Block& block, const AccessPath& path)
    -> mir::ExprId;

// The path as a reference (LRM 13.5.2): a reference to the whole of what the
// owner holds -- for a property of an object, a step taken on the object --
// then one step per part, each taken on the reference before it. What a
// reference to a part belongs to travels with it, so a write through it is a
// write of the owner at the moment it lands, however long the reference is
// held.
[[nodiscard]] auto PathReference(
    mir::CompilationUnit& unit, mir::Block& block, const AccessPath& path)
    -> mir::ExprId;

// Holds a leased reference for as long as the call it is bound for runs, and
// answers what reads it there.
using HoldLease = std::function<mir::ExprId(mir::ExprId leased)>;

// The path as a reference a call binds (LRM 13.5.2): the reference the path
// names, with a lease taken on each thing it reaches through that the program
// can let go of while the call runs -- the object a property lies in, an
// element of a queue or an associative array, a dynamic array's elements -- so
// each goes on existing until the call returns (LRM 8.29, 13.5.2). `hold`
// answers what each lease is read from: the lease itself where the binding is
// written in the call's full-expression, which then holds it to that
// full-expression's end, or a local holding it where the call runs after that
// full-expression has ended.
[[nodiscard]] auto PathBoundReference(
    mir::CompilationUnit& unit, mir::Block& block, const AccessPath& path,
    const HoldLease& hold) -> mir::ExprId;

// The reference the read form of an element step answers on `receiver`, whose
// dereference is the element: what a block of steps yields where the element
// is reached after the block, the receiver outliving it.
[[nodiscard]] auto ElementReference(
    mir::CompilationUnit& unit, mir::Block& block, const DescentStep& step,
    mir::ExprId receiver) -> mir::Expr;

// The part `step` reaches in what `receiver` names, taken for a read, at the
// part's type: the part itself where it is storage of its own -- a field, or
// what the reference an element's read form answers refers to -- and otherwise
// what the entry reading the part answers, bits of a packed value included.
[[nodiscard]] auto StepForRead(
    mir::CompilationUnit& unit, mir::Block& block, const DescentStep& step,
    mir::ExprId receiver) -> mir::Expr;

// The value of that part.
[[nodiscard]] auto StepRead(
    mir::CompilationUnit& unit, mir::Block& block, const DescentStep& step,
    mir::ExprId receiver) -> mir::Expr;

// The type a read of a part of `source_type` answers at, where the part is
// declared `part_type`. LRM 11.8.1: a part-select is unsigned regardless of the
// operands, and its state domain follows the value it selects from, so a member
// of a packed aggregate (LRM 7.2.1, selected as a part-select of the
// aggregate's vector) is produced at the member's width, unsigned, in the
// aggregate's state domain. The member's declared signedness and, for a
// 2-state member of a 4-state aggregate, its narrower state domain are reached
// by an explicit conversion of what was read. A part of a value that is not
// packed is read at its declared type.
[[nodiscard]] auto PartSelectNaturalType(
    mir::CompilationUnit& unit, mir::TypeId source_type, mir::TypeId part_type)
    -> mir::TypeId;

// The checks a read along `path` owes before it is made, one for each step into
// a member of a tagged union, appended to `block`: for a construct that takes
// the steps itself after the checks, where it lies.
void AppendReadChecks(
    mir::CompilationUnit& unit, mir::Block& block, const AccessPath& path);

// The part the path names, as the place it lies in where it is one: what the
// owner holds, then each step as a read takes it, so nothing on the way to the
// part is read whole.
[[nodiscard]] auto PathAsPlace(
    mir::CompilationUnit& unit, mir::Block& block, const AccessPath& path)
    -> mir::ExprId;

// The path as an expression yielding the part's value, for a caller that reads
// it and writes nothing.
[[nodiscard]] auto PathValue(
    mir::CompilationUnit& unit, mir::Block& block, const AccessPath& path)
    -> mir::ExprId;

// The path's value at the type the part is declared with: a packed part is
// read at the type the read answers at and brought to its declared type.
[[nodiscard]] auto PathValueAsDeclared(
    mir::CompilationUnit& unit, mir::Block& block, const AccessPath& path)
    -> mir::ExprId;

// The place `place` names, with whatever is computed on the way to it evaluated
// here, once: a pointer or a handle the place is reached through is bound to a
// local of the enclosing body, and the place is formed over that, so it may
// stand at several places. A place is a name, or a field or a dereference over
// what it is reached through; a value a computation yields is no place and is
// refused.
[[nodiscard]] auto SettledPlace(
    const UnitLowerer& unit_lowerer, const WalkFrame& frame, mir::ExprId place)
    -> mir::ExprId;

// `owner` with whatever is computed on the way to it evaluated here, once: the
// way to a place, or the object a property belongs to.
[[nodiscard]] auto SettledOwner(
    const UnitLowerer& unit_lowerer, const WalkFrame& frame,
    const PathOwner& owner) -> PathOwner;

// The same path with everything it computes evaluated here, once: the way to
// its owner and every operand of its steps. A path describes nodes that are
// evaluated wherever it is taken, so one taken at more than one place is
// settled first.
[[nodiscard]] auto Settled(
    const UnitLowerer& unit_lowerer, const WalkFrame& frame, AccessPath path)
    -> AccessPath;

// `target`, the target of a write, with what its steps compute up to its last
// step into a member of a tagged union evaluated here, once (LRM 11.4.1): the
// check of that member's tag and the write both name those steps, and the
// check stands where the write lands (LRM 11.9). A target owing no check is
// left as it is.
void SettleForTagChecks(
    const UnitLowerer& unit_lowerer, const WalkFrame& frame,
    AccessPath& target);

// A part a construct reads at several places: its owner and the descent to it,
// neither evaluating anything, and the block their nodes are named in. Every
// read of it reaches the same part.
struct SettledPath {
  const mir::Block* named_in = nullptr;
  PathOwner owner;
  std::vector<DescentStep> descent;
};

// `path` settled in the frame's block, for a construct that reads through it
// and writes nothing.
[[nodiscard]] auto SettledForRead(
    const UnitLowerer& unit_lowerer, const WalkFrame& frame, AccessPath path)
    -> SettledPath;

// `settled` as a path whose nodes are named in `to`. A node belongs to the
// block it was added to, so a block nested under the one a path was settled in
// -- a loop's body, a sequence of steps -- names the path's nodes afresh to
// read through it; they evaluate nothing, so the path named again reaches the
// same part.
[[nodiscard]] auto NamedIn(const SettledPath& settled, mir::Block& to)
    -> AccessPath;

// Whether `path` reaches its part through a member of an untagged union, so the
// part lies where another member's bits may lie (LRM 7.3.2) and is changed in
// place only by a write that leaves it as its type holds a value. It asks of a
// path what the place built from it states as its kind, for a caller that has
// to choose before the place is built.
[[nodiscard]] auto ReachesOverlaidStorage(const AccessPath& path) -> bool;

// An operand a construct reads and then writes: the place the write lands in,
// and the value it holds going in.
struct ReadThenWritten {
  AccessPath place;
  mir::ExprId incoming;
};

// `place` as an operand that is read and then written (LRM 13.5 `inout`): the
// path is settled here, and the value going in is read through it at the type
// the part is declared with, so the source's one operand is evaluated once
// however far apart the read and the write stand.
[[nodiscard]] auto ReadThenWrite(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, AccessPath place)
    -> ReadThenWritten;

// Positions in a row within a value, in the position type: where they start
// and how many there are. They are bits of a packed value, counted from its
// least significant bit, or elements of an array, counted from its first.
struct PathPositions {
  mir::ExprId first;
  std::uint64_t width = 0;
};

// The bits a path names within its owner (LRM 7.2.1, 11.5.1). Every step into
// a packed value names some of its bits, so the part starts at the sum of where
// the steps start, and that sum stays an expression because a step's position
// may be a value the program or a construction supplies.
[[nodiscard]] auto BitsWithinOwner(
    mir::CompilationUnit& unit, mir::Block& block, const AccessPath& path)
    -> PathPositions;

// Which part of its owner a path names, as a wait or a report is told of it.
// A part of an unpacked value is named by the steps that reach it: an element
// or a character of a string by its ordinal and a component by its index (LRM
// 7.4, 6.16, 7.2). Where the path
// goes on into a packed value, the part is bits of the value those steps
// reach; where it ends in a run of elements (LRM 7.4.6), it is those elements
// of the array they reach.
struct NamedPart {
  // One machine integer for each step to an element, a character or a
  // component, in order.
  std::vector<mir::ExprId> steps;
  // The bits or the elements named within the value the steps reach, none
  // where the part is the whole of it.
  std::optional<PathPositions> positions;
  // Whether that is the part itself. Where the path goes on in a way no step
  // stands for, as by an associative index, it is the last part the steps
  // name, which holds the path's part and more.
  bool exact = true;
};

[[nodiscard]] auto PartNamedBy(
    mir::CompilationUnit& unit, mir::Block& block, const AccessPath& path)
    -> NamedPart;

// Whether a write to the part `path` names lands on bits of a character of a
// string, and so reads the character before putting it back.
[[nodiscard]] auto WritesBitsOfACharacter(const AccessPath& path) -> bool;

// Builds `lhs = rhs` against the part `path` names, as an ordinary MIR node
// with nothing left to decide that yields nothing: replacing the whole of what
// a capability wrapper holds acts on the wrapper, a call taking the wrapper as
// its destination; a write through a character of a string is the string's
// own put; a run of elements is written by its container's own slice write,
// and bits by `set` through the reference `slice_mut` answers, each taken on
// the place the value lies in or on its designation within a write; every
// other write is a store into the place the path designates.
[[nodiscard]] auto BuildStoreExpr(
    mir::CompilationUnit& unit, mir::Block& block, const AccessPath& path,
    mir::ExprId rhs_id) -> mir::Expr;

}  // namespace lyra::lowering::hir_to_mir
