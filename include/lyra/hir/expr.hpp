#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/base/component_index.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/class_ref.hpp"
#include "lyra/hir/constraint_id.hpp"
#include "lyra/hir/conversion.hpp"
#include "lyra/hir/covergroup_ref.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/inc_dec_op.hpp"
#include "lyra/hir/interface_member_access.hpp"
#include "lyra/hir/pattern.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/hir/range_bounds.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/hir/timing.hpp"
#include "lyra/hir/unary_op.hpp"
#include "lyra/hir/value_ref.hpp"

namespace lyra::hir {

struct PrimaryExpr {
  Primary data;

  auto operator==(const PrimaryExpr&) const -> bool = default;
};

struct UnaryExpr {
  UnaryOp op;
  ExprId operand;

  auto operator==(const UnaryExpr&) const -> bool = default;
};

struct BinaryExpr {
  BinaryOp op;
  ExprId lhs;
  ExprId rhs;

  auto operator==(const BinaryExpr&) const -> bool = default;
};

// LRM 11.4.11 / 12.6.3. `conditions` is the predicate's clause sequence,
// always at least one entry; a plain `cond ? a : b` is the single
// pattern-free clause.
struct ConditionalExpr {
  std::vector<ConditionClause> conditions;
  ExprId then_value;
  ExprId else_value;

  auto operator==(const ConditionalExpr&) const -> bool = default;
};

// The operator an assignment operator applies (LRM 11.4.1), and the type it is
// applied at. `a op= b` is `a = a op b` with `a` reached once, so the operator
// is carried out as the binary expression would be: at the type its two
// operands fix between them (LRM 11.6.1 Table 11-21, 11.8.1), which is the
// target's only where the other operand is no wider, no less signed and holds
// no more states than the target. The answer is then assigned, which is what
// brings it to the target's type.
struct CompoundAssignOperator {
  BinaryOp op;
  TypeId applied_at;

  auto operator==(const CompoundAssignOperator&) const -> bool = default;
};

// `compound.has_value()` marks a compound assignment (`+=`, `-=`, etc.): what
// the target holds is read, combined with `rhs` and written back, the target
// being reached once (LRM 11.4.1). `rhs` is then typed as the operator takes
// its right operand -- at the type the operator is applied at, or at its own
// where the operator sizes that operand on its own, as a shift does its
// amount. LRM A.6.2 forbids compound on non-blocking, so non-blocking timing
// carrying a compound operator is an InternalError.
//
// `lhs` is an lvalue (LRM A.8.5): an expression naming storage -- a variable
// or a select on one -- or a concatenation, an assignment pattern or a
// streaming concatenation of lvalues. Lvalue-ness is positional -- determined
// by appearance in this `lhs` field, not by an extra tag on the expression.
struct AssignExpr {
  EffectTiming timing;
  ExprId lhs;
  std::optional<CompoundAssignOperator> compound = std::nullopt;
  ExprId rhs;

  auto operator==(const AssignExpr&) const -> bool = default;
};

// LRM 11.4.2: `++a`, `a++`, `--a`, `a--`. Behave as blocking assignments;
// postfix yields the operand's prior value, prefix yields the new value. The
// target names storage, in the same forms an assignment's left side may take.
// Whether a form names storage is decided by the position it appears in, and
// settled while lowering from the AST -- HIR holds only forms that passed.
struct IncDecExpr {
  IncDecOp op;
  ExprId target;

  auto operator==(const IncDecExpr&) const -> bool = default;
};

struct ConversionExpr {
  ConversionKind kind;
  ExprId operand;

  auto operator==(const ConversionExpr&) const -> bool = default;
};

// A value the unit being compiled fixes, standing where the source named it --
// a parameter of the unit's parameterization, a `$bits` of a type it holds --
// with `value` the literal or pattern that states it. A value that differs
// between the objects built from the unit is never one; it is read where it is
// held. It stays apart from a literal the source wrote because the language
// can treat the two differently: in a display list a string literal is a format
// and a string parameter is a value (LRM 21.2.1). clang names the same node
// `ConstantExpr`.
struct ConstantExpr {
  ExprId value;

  auto operator==(const ConstantExpr&) const -> bool = default;
};

// LRM 21.3.4.4 form 2d (`$fread(mem, fd, , count)`) and any future
// system-call positional-elision case lands here as `std::nullopt` at the
// elided slot; user calls and most system calls leave every entry filled.
// Per-subroutine HIR-to-MIR lowering decides whether elision is meaningful
// at each position.
struct CallExpr {
  SubroutineRef callee;
  std::vector<std::optional<ExprId>> arguments;

  auto operator==(const CallExpr&) const -> bool = default;
};

// LRM 11.4.13 value range `[lo:hi]`. slang models it as an ordinary
// expression, and so does HIR: it is only meaningful as an operand of a
// membership test, but that is a rule about where it may appear, not a reason
// to give it a shape outside the expression set. Keeping it here is what lets
// every membership operand -- an `inside` item, a case-inside label -- be a
// plain `ExprId`. A bound the source wrote as `$` is absent: it stands for the
// lowest or the highest value the type of the left operand has, so that side of
// the range leaves nothing out.
struct ValueRangeExpr {
  std::optional<ExprId> lo;
  std::optional<ExprId> hi;

  auto operator==(const ValueRangeExpr&) const -> bool = default;
};

struct InsideExpr {
  ExprId lhs;
  std::vector<ExprId> items;

  auto operator==(const InsideExpr&) const -> bool = default;
};

struct ElementSelectExpr {
  ExprId base_value;
  ExprId index;

  auto operator==(const ElementSelectExpr&) const -> bool = default;
};

struct RangeSelectExpr {
  ExprId base_value;
  RangeBounds bounds;

  auto operator==(const RangeSelectExpr&) const -> bool = default;
};

// Struct or union member access (LRM 7.2 / 7.3): `field_index` is the
// member's declaration-order position. An unpacked struct or union is a
// product of its member types, so the type is the component list and nothing
// else declares the member; the position is the whole of its identity.
struct MemberAccessExpr {
  ExprId base_value;
  base::ComponentIndex field_index;

  auto operator==(const MemberAccessExpr&) const -> bool = default;
};

// Class property access (LRM 8.4 / 8.13): `target` names the declaring
// class and the slot within that class's property arena. Owner-qualified
// because under inheritance the receiver's runtime class may not be the
// declaring class -- the receiver reaches the object through a handle to a
// class that extends the property owner. The external arm is used when the
// declaring class lives in another compilation unit.
struct ClassPropertyAccessExpr {
  ExprId base_value;
  ClassPropertyTarget target;

  auto operator==(const ClassPropertyAccessExpr&) const -> bool = default;
};

struct ConcatExpr {
  std::vector<ExprId> operands;

  auto operator==(const ConcatExpr&) const -> bool = default;
};

// LRM 11.4.14.4: the elements of a one-dimensional unpacked array a stream
// operand takes, written `with [...]`: one element, or a run of them named by
// its two ends or by a start and a width counted up or down. It is evaluated
// immediately before its array is streamed, so in a target it may read what
// the same stream unpacked into an operand before it.
using StreamRange = std::variant<ExprId, RangeBounds>;

// One operand of a stream: what is streamed, and the range of its elements a
// `with` names where one is written.
struct StreamOperand {
  ExprId value;
  std::optional<StreamRange> with;

  auto operator==(const StreamOperand&) const -> bool = default;
};

// LRM 11.4.14: `{<< n {a, b}}` -- the bits of each operand laid end to end, the
// first operand most significant, and the result then divided into `block_bits`
// blocks whose order is reversed. `block_bits` is zero for `>>`, which
// LRM 11.4.14.2 says performs no re-ordering at all and ignores any slice size
// written beside it; the front end has already folded the written size to a bit
// count, so nothing below reads a slice type or a constant expression.
//
// The same node stands on either side of an assignment: as a source it builds a
// stream, and as a target it says which variables that stream fills and in what
// order (LRM 11.4.14.3). Which of the two an occurrence is follows from where
// it stands, so it carries no discriminator.
//
// It has no type of its own, as the front end gives it none: the stream between
// packing and unpacking is never visible (LRM 11.4.14), and where it is a
// source or a cast's operand, the conversion around it carries the type the
// stream becomes.
struct StreamingConcatExpr {
  std::vector<StreamOperand> operands;
  std::uint64_t block_bits;

  auto operator==(const StreamingConcatExpr&) const -> bool = default;
};

// LRM 11.4.12: `{multiplier{...}}` is a replication built around an inner
// concatenation. The inner ExprId always points to a ConcatExpr.
struct ReplicationExpr {
  ExprId count;
  ExprId concat;

  auto operator==(const ReplicationExpr&) const -> bool = default;
};

// LRM 10.9 assignment pattern in the form that states every element by
// position: the list is in target declaration order -- most significant first
// where the target is packed -- with each item already converted to the field
// or element's declared type. The shape covers packed targets (struct / union /
// packed array), fixed-size unpacked arrays, and dynamic arrays; HIR-to-MIR
// dispatches to the right primitive based on the resolved target type.
//
// A pattern written with keys over an array keeps its keys instead, because a
// key names an element while this list names positions, and the two orders
// disagree for a descending dimension.
struct AssignmentPatternExpr {
  std::vector<ExprId> elements;

  auto operator==(const AssignmentPatternExpr&) const -> bool = default;
};

// LRM 10.9 replicated assignment pattern `'{count{items...}}`. `items` is the
// per-iteration expression list -- slang stores only one iteration's items with
// that iteration's per-field casts and requires the target's per-iter type
// chunks to repeat. Holding one iteration and a count is what stops a
// mostly-uniform aggregate costing its own length to describe; whether the
// items are expanded at all is the target type's own question.
struct AssignmentPatternReplicationExpr {
  ExprId count;
  std::vector<ExprId> items;

  auto operator==(const AssignmentPatternReplicationExpr&) const
      -> bool = default;
};

// LRM 7.5.1 `new[N]` / `new[N](other)` dynamic array constructor. The result
// type (the dynamic array type) lives on Expr::type; `size` evaluates to a
// longint per LRM 7.5.1 (slang enforces the operand type), and `initializer`
// holds the optional `(other)` source array used for copy-with-pad-or-truncate
// per LRM 7.5.1.
struct DynamicArrayNewExpr {
  ExprId size;
  std::optional<ExprId> initializer;

  auto operator==(const DynamicArrayNewExpr&) const -> bool = default;
};

// LRM 8.5 class object construction `new`. Allocates a new object of the named
// class and runs its constructor, yielding a handle. `Expr::type` is the class
// handle type. `arguments` are the constructor actuals (LRM 8.7), empty for
// the default `new`.
//
// `declaring_instance` is how this body reaches the instance of the scope that
// declares the class: a class declared in a structural scope is a type of that
// scope's instance (LRM 6.22), so the object records which instance it belongs
// to and construction is where that is supplied. How that instance is reached
// is known only where the construction is written, which is why it travels on
// the expression; it is present exactly where the class takes the instance,
// which is the class's own declaration to say.
struct ClassNewExpr {
  ClassId class_id;
  std::vector<ExprId> arguments;
  std::optional<DeclaringInstanceReach> declaring_instance;

  auto operator==(const ClassNewExpr&) const -> bool = default;
};

// LRM 19.3 `new` of a covergroup: an instance made from `arguments`, one per
// formal of the covergroup. One a class embeds (LRM 19.4) is made by an object
// of that class, `embedding`; one a structural scope declares is a type of
// that scope's instance, reached as `declaring_instance` is for a class.
struct CovergroupNewExpr {
  CovergroupId covergroup;
  std::vector<ExprId> arguments;
  std::optional<ExprId> embedding;
  std::optional<DeclaringInstanceReach> declaring_instance;

  auto operator==(const CovergroupNewExpr&) const -> bool = default;
};

// What a covergroup's method or option is reached through (LRM 19.8, 19.10):
// an instance, or the covergroup's type, which reaches the instance of the
// scope the type belongs to, where it belongs to one, as a static method's
// call does (LRM 6.22).
struct CovergroupInstanceReceiver {
  ExprId instance;

  auto operator==(const CovergroupInstanceReceiver&) const -> bool = default;
};

struct CovergroupTypeReceiver {
  std::optional<DeclaringInstanceReach> declaring_instance;

  auto operator==(const CovergroupTypeReceiver&) const -> bool = default;
};

using CovergroupReceiver =
    std::variant<CovergroupInstanceReceiver, CovergroupTypeReceiver>;

// A call of a method of a covergroup, or of one of its coverpoints or crosses
// where `item` names one (LRM 19.8): of the type for `get_coverage`
// (`cg::get_coverage()`), and of an instance otherwise. `arguments` are as the
// source wrote them, an absent one being left to its default.
struct CovergroupMethodCallExpr {
  CovergroupId covergroup;
  CovergroupReceiver receiver;
  std::optional<std::string> item;
  CovergroupMethod method = CovergroupMethod::kSample;
  std::vector<std::optional<ExprId>> arguments;

  auto operator==(const CovergroupMethodCallExpr&) const -> bool = default;
};

// `option.<field>` of a covergroup instance, or of one of its coverpoints or
// crosses where `item` names one, or `type_option.<field>` of its type (LRM
// 19.7, 19.10), read and written as a variable is.
struct CovergroupOptionExpr {
  CovergroupId covergroup;
  CovergroupReceiver receiver;
  std::optional<std::string> item;
  std::string field;

  auto operator==(const CovergroupOptionExpr&) const -> bool = default;
};

// How an invalid assignment is handled, which LRM 6.24.2 makes the whole of
// what the two spellings of the construct differ in: the answer is the only
// report where the source called the function, and the design is told where it
// called the task.
enum class InvalidAssignmentHandling : std::uint8_t { kAnswered, kReported };

// What a dynamic cast asks of the value while it runs, in the cases where the
// two declared types allow the assignment at all. Which one it is follows from
// who fixes the values the destination's type accepts: an enumeration fixes
// them where it is declared, while the classes a handle may refer to are open
// across compilation units, so there only the object itself can answer (LRM
// 8.16).
enum class RunTimeCheck : std::uint8_t {
  // Nothing is asked while it runs: the declared types are the whole answer and
  // every value of the source's passes.
  kNone,
  // An enumeration accepts the values it declares and no others, so one pair of
  // types gives two answers -- 2 into a `{RED=1, GREEN=2}` is valid and 9 is
  // not (LRM 6.19).
  kValueIsAMemberOfTheEnumeration,
  // A handle accepts an object of the destination's class or of one extending
  // it, so one pair of types gives two answers -- the same superclass handle
  // becomes a subclass one or does not, on what was constructed (LRM 8.16).
  kObjectIsOfTheDestinationClass,
  // A value of `dynamic_type` is of whichever type it was given, and the
  // storage a `dynamic_type` reference names is of its own, so one pair of
  // types gives two answers -- a message is received only where the two are
  // equivalent (LRM 15.4.5, 6.22.2).
  kValueIsOfAnEquivalentType,
};

// The two declared types allow no assignment between them at all (LRM 6.22.4,
// 6.22.5), so the destination is written on no run and the standard defines no
// conversion to write it with.
struct NoAssignmentAllowed {
  auto operator==(const NoAssignmentAllowed&) const -> bool = default;
};

// They allow one, and `check` is what the value is still asked.
struct AssignmentAllowed {
  RunTimeCheck check;

  auto operator==(const AssignmentAllowed&) const -> bool = default;
};

// What the front end settled about the pair of declared types before any value
// is in hand (LRM 6.24.2, 6.22.3, 6.22.4). Whether there is an assignment to
// state at all comes first, because where the types allow none there is no
// conversion for one to be built out of and nothing left to ask the value.
using AssignmentValidity = std::variant<NoAssignmentAllowed, AssignmentAllowed>;

// LRM 6.24.2 `$cast`, the dynamic cast: the source asks for an assignment the
// two declared types would not ordinarily allow, and the destination takes the
// value where that assignment turns out to be valid. The expression answers 1
// where it was and 0 where it was not, and the destination is written only in
// the first case, so an invalid assignment leaves it holding what it held.
//
// `destination` names storage the way an assignment's left side does, by
// standing in this position rather than by any tag on the expression. What
// decides validity is the type that destination was declared with together
// with the value, never the source expression's own type alone: an integral
// becomes an enumeration only where it is one of the members (LRM 6.19), and a
// handle becomes one of a subclass only where the object it refers to is of
// that class (LRM 8.16).
//
// `validity` is the front end's verdict on the pair of declared types, which is
// the half of the answer that does not wait for a value. It is carried rather
// than re-derived because compatibility between two SystemVerilog types is the
// front end's to decide, and a run-time check standing where the types already
// make the assignment invalid answers a question that was settled.
struct DynamicCastExpr {
  ExprId destination;
  ExprId source;
  AssignmentValidity validity;
  InvalidAssignmentHandling on_invalid;

  auto operator==(const DynamicCastExpr&) const -> bool = default;
};

// LRM 11.9 tagged union expression `tagged Member primary`. `member_index` is
// the declaration-order position of the tagged member inside the union type,
// which is the tag: a member is reached by where it sits, never by its name.
// `payload` is absent when the member is `void`.
struct TaggedUnionExpr {
  base::ComponentIndex member_index;
  std::optional<ExprId> payload;

  auto operator==(const TaggedUnionExpr&) const -> bool = default;
};

// LRM 7.9.11 associative-array literal `'{index: value, ..., default: d}`. Each
// entry pairs a key expression with a value expression; the optional default is
// the persistent fallback a read of an absent key returns (LRM 7.8.6), so it
// outlives the build rather than being spent filling elements. The keys are
// arbitrary values with no positional meaning, which is why the key structure
// is retained here rather than flattened into an element list.
struct AssociativeAssignmentPatternExpr {
  struct Entry {
    ExprId key;
    ExprId value;

    auto operator==(const Entry&) const -> bool = default;
  };
  std::vector<Entry> entries;
  std::optional<ExprId> default_value;

  auto operator==(const AssociativeAssignmentPatternExpr&) const
      -> bool = default;
};

// LRM 10.9.1 `'{index: value, ..., default: value}` over an array. An
// `array_pattern_key` is either an index naming one element or the `default`
// key standing for every element no index named; the source may write either
// kind, and LRM 10.9.1 admits at most one `default`. What an unnamed element
// takes is stated as the value of that element's own type: where the source's
// `default` or type key reached a subarray or a structure, it is the pattern
// that key makes of one.
//
// The elements the default stands for are not written out. How many there are
// belongs to the target's type and appears nowhere in the source, so no count
// is carried: naming one would invent a number the source does not contain,
// and listing the elements instead would make a mostly-uniform aggregate cost
// its own length to describe -- a 32768-element array reaches the target
// language as a four-megabyte expression that no compiler will accept.
//
// A key designates an element of the target rather than computing one, the way
// a structure pattern's key names a member. It is carried as the expression the
// source wrote, evaluated where the pattern is: the standard requires the key
// to be settled before the program runs, and what settles it may be an
// elaboration-time input the compiled form does not fix -- a loop index the
// construction supplies -- so reading the value the front end reached would
// bake one repetition's answer into every one of them.
//
// What it yields is an index and not a storage offset, because the two orders
// differ: offsets run from the dimension's left end, which is the most
// significant element of a packed array, while indices run whichever way the
// dimension was declared. Resolving one to the other is the target type's own
// arithmetic and belongs wherever that type is in hand.
struct AssignmentPatternKeyedExpr {
  struct Entry {
    ExprId index;
    ExprId value;

    auto operator==(const Entry&) const -> bool = default;
  };
  std::vector<Entry> entries;
  std::optional<ExprId> default_value;

  auto operator==(const AssignmentPatternKeyedExpr&) const -> bool = default;
};

// The variables a call of `randomize()` chooses (LRM 18.11, 18.12): those the
// object declares random whose modes are on, or exactly those the call lists,
// which `randomize(null)` lists none of, so that it checks instead (LRM
// 18.11.1). Each listed variable is a place.
struct DeclaredRandomVariables {
  auto operator==(const DeclaredRandomVariables&) const -> bool = default;
};
struct ListedRandomVariables {
  std::vector<ExprId> variables;

  auto operator==(const ListedRandomVariables&) const -> bool = default;
};
using RandomVariables =
    std::variant<DeclaredRandomVariables, ListedRandomVariables>;

// What a call of `randomize()` randomizes: an object, reached through a handle
// or as the running method's own (LRM 18.6.1), with the variables it chooses;
// or the variables a scope randomize lists (LRM 18.12).
struct RandomizedClassObject {
  ExprId object;
  RandomVariables variables;

  auto operator==(const RandomizedClassObject&) const -> bool = default;
};
struct RandomizedScopeVariables {
  std::vector<ExprId> variables;

  auto operator==(const RandomizedScopeVariables&) const -> bool = default;
};
using RandomizeTarget =
    std::variant<RandomizedClassObject, RandomizedScopeVariables>;

// A call of `randomize()` (LRM 18.6.1, 18.7, 18.11, 18.12), and the constraint
// block written with it where one is (LRM 18.7, 18.12.1), in this body's arena.
// It answers 1 where it set every variable it chooses and 0 where nothing
// satisfies the constraints.
struct RandomizeExpr {
  RandomizeTarget target;
  std::optional<ConstraintId> with;

  auto operator==(const RandomizeExpr&) const -> bool = default;
};

// What `rand_mode()` and `constraint_mode()` act on (LRM 18.8, 18.9): one
// random variable, a place; every random variable or every constraint block of
// an object; one constraint block, by its name on an object; and a static
// block, whose mode every object of its class shares.
struct RandomVariableMode {
  ExprId variable;

  auto operator==(const RandomVariableMode&) const -> bool = default;
};
struct ObjectModes {
  ExprId object;

  auto operator==(const ObjectModes&) const -> bool = default;
};
struct ConstraintBlockMode {
  ExprId object;
  std::string name;

  auto operator==(const ConstraintBlockMode&) const -> bool = default;
};
// A static block's mode is its class's (LRM 18.5.10): the class that
// introduced the block, whichever class's block of that name the call reached.
struct StaticConstraintBlockMode {
  ClassId introducer;
  std::string block;

  auto operator==(const StaticConstraintBlockMode&) const -> bool = default;
};

// `rand_mode(on)` called as a task, and `rand_mode()` as a function (LRM
// 18.8).
struct RandModeSetExpr {
  std::variant<RandomVariableMode, ObjectModes> target;
  ExprId on;

  auto operator==(const RandModeSetExpr&) const -> bool = default;
};
struct RandModeGetExpr {
  RandomVariableMode target;

  auto operator==(const RandModeGetExpr&) const -> bool = default;
};

// `constraint_mode(on)` and `constraint_mode()` (LRM 18.9).
struct ConstraintModeSetExpr {
  std::variant<ConstraintBlockMode, StaticConstraintBlockMode, ObjectModes>
      target;
  ExprId on;

  auto operator==(const ConstraintModeSetExpr&) const -> bool = default;
};
struct ConstraintModeGetExpr {
  std::variant<ConstraintBlockMode, StaticConstraintBlockMode> target;

  auto operator==(const ConstraintModeGetExpr&) const -> bool = default;
};

using ExprData = std::variant<
    PrimaryExpr, UnaryExpr, BinaryExpr, ConditionalExpr, AssignExpr, IncDecExpr,
    CallExpr, ConversionExpr, ConstantExpr, ValueRangeExpr, InsideExpr,
    ElementSelectExpr, RangeSelectExpr, MemberAccessExpr,
    ClassPropertyAccessExpr, InterfaceMemberAccessExpr,
    InterfaceInstanceAccessExpr, ConcatExpr, StreamingConcatExpr,
    ReplicationExpr, AssignmentPatternExpr, AssignmentPatternReplicationExpr,
    DynamicArrayNewExpr, ClassNewExpr, AssociativeAssignmentPatternExpr,
    AssignmentPatternKeyedExpr, TaggedUnionExpr, DynamicCastExpr,
    CovergroupNewExpr, CovergroupMethodCallExpr, CovergroupOptionExpr,
    RandomizeExpr, RandModeSetExpr, RandModeGetExpr, ConstraintModeSetExpr,
    ConstraintModeGetExpr>;

struct Expr {
  TypeId type;
  ExprData data;
  diag::SourceSpan span;

  auto operator==(const Expr&) const -> bool = default;
};

}  // namespace lyra::hir
