#pragma once

#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/backend/llvm/entry_realizations.hpp"
#include "lyra/backend/llvm/runtime_entry.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/declared_member_storage.hpp"
#include "lyra/support/library_decl.hpp"
#include "lyra/support/runtime_object.hpp"
#include "lyra/support/value_domain.hpp"

namespace lyra::backend::llvm_backend {

// How one argument of a call crosses to its callee, the role rustc gives
// `PassMode`: the value as it is; the value and what its type states to a
// callee reading its bits, or reading it as a number; the value and the
// constant of a type the callee keeps and acts on; or the value and how wide
// an integral type is.
struct PassDirect {};
struct PassWithExtent {
  lir::TypeId type;
};
struct PassWithShape {
  lir::TypeId type;
};
struct PassWithType {
  lir::TypeId type;
};
struct PassWithWidth {
  lir::TypeId type;
};
using PassMode = std::variant<
    PassDirect, PassWithExtent, PassWithShape, PassWithType, PassWithWidth>;

// What a callee is told of a type no argument is a value of: the constant of a
// type it keeps and acts on, how wide an integral type is and whether it
// holds x or z, the constant stating a type as the element type of an array,
// which says how a value of it starts as well (LRM Table 7-1), or the
// constant stating a queue type, which says that of its elements and how many
// it may hold (LRM 7.10.5), the type's own function taking some of the
// positions of a value of it (LRM 10.6.2), the order an associative array
// indexed by `key` holds its entries in, which the library states once for each
// kind of index (LRM 7.8), or the operations the library compares and streams a
// value of the type by.
struct TypeConstantArg {
  lir::TypeId type;
};
struct TypeExtentArg {
  lir::TypeId type;
};
struct ElementTypeArg {
  lir::TypeId type;
};
struct QueueTypeArg {
  lir::TypeId type;
};
struct TakePositionsArg {
  lir::TypeId type;
};
struct IndexOrderArg {
  lir::TypeId key;
};
struct OperationsArg {
  lir::TypeId type;
};
using TypeArg = std::variant<
    TypeConstantArg, TypeExtentArg, ElementTypeArg, QueueTypeArg,
    TakePositionsArg, IndexOrderArg, OperationsArg>;

// What a callee takes ahead of the arguments a call states. An entry making
// storage for the words of a value wider than a word is told how wide the
// values held there are, one making storage for a value laid out by its type
// is told that type with how a value of it starts, and one making a net of
// such values is told how the net folds them as well.
struct NoImplicitArg {};
struct WidthMetadataArg {
  lir::TypeId of;
};
struct TypeMetadataArg {
  lir::TypeId of;
};
struct TypeAndNetFoldsArg {
  lir::TypeId of;
};
using ImplicitArg = std::variant<
    NoImplicitArg, WidthMetadataArg, TypeMetadataArg, TypeAndNetFoldsArg>;

// A part the callee names, which is no value the program computed: it crosses
// as a machine integer, ahead of the argument at `before`.
struct NamedPartArg {
  std::size_t before;
  std::uint64_t position;
};

// An operand of a call this target states on its own account: a value of a
// type of the layer above, or a machine value this target computed, as what
// holds it.
enum class MachineOperandKind : std::uint8_t { kAddress, kCount, kSpan };
struct ValueOperand {
  lir::TypeId type;
};
struct MachineOperand {
  MachineOperandKind kind;
};
using StatedOperand = std::variant<ValueOperand, MachineOperand>;

// One argument of a call, the role of rustc's `ArgAbi`: what is handed, and how
// it crosses.
struct ArgAbi {
  StatedOperand operand;
  PassMode mode;
};

// What crosses a call, the role of rustc's `FnAbi`, which holds no machine type
// either: what the callee takes ahead of the arguments, one `ArgAbi` per
// argument the call states with the part the callee names among them, and what
// the callee is told of types, which follows them all. Where its call was
// written is the last operand a call states, to a library entry and to a body
// of the program alike. It reads LIR alone, so it holds as soon as the call
// does. The machine types a call crosses at follow from this and from how the
// answer comes back.
struct FnAbi {
  ImplicitArg implicit_arg;
  std::optional<NamedPartArg> named_part;
  std::vector<ArgAbi> args;
  std::vector<TypeArg> type_args;
};

// The realization of an entry that the library compiles once per
// representation, chosen for one call: what it is told, and the type it is
// realized for -- the value whose representation names it, or the values held
// by the storage it acts on -- which is where what it is told of a type is
// read from.
struct Realization {
  Told told;
  lir::TypeId of;
};

// What `operand` is held to by being read as `reading`: an ordinal is stated as
// a position, and an operand read as the thing it is is no integral value.
void RequireReadAs(
    const lir::TypePool& types, support::OperandReading reading,
    lir::TypeId operand);

// How an operand of type `operand` crosses to an entry that reads it as
// `reading`: the reading alone decides, and the type is only where a constant
// crossing with the value is read from.
auto PassModeOf(support::OperandReading reading, lir::TypeId operand)
    -> PassMode;

// The same, told of the value the operand carries. An integral value crosses as
// the address of its bytes, so a reference to one read as bits or as a number
// crosses as that value does.
auto CrossingModeOf(
    const lir::TypePool& types, support::OperandReading reading,
    lir::TypeId operand) -> PassMode;

// What crosses a call to the entry `realization` realizes, none where the
// library realizes it once: each of `operands` as `readings` reads the operand
// at its place and as the realization is told of it, the part the callee names,
// and the type the realization is told after them and then what the entry is
// told of its answer. Every call on a library entry is arranged here, the ones
// this target states on its own account included.
auto ArrangeCall(
    const lir::TypePool& types, const support::OperandReadings& readings,
    std::span<const StatedOperand> operands,
    const std::optional<Realization>& realization,
    std::optional<NamedPartArg> named_part,
    std::vector<TypeArg> told_of_the_answer) -> FnAbi;

// What crosses a call on `op`, an entry this target names by an operation of
// its own, where no call of the layer above states it: each of `operands` as
// the entry's declaration reads the operand at its place and as `realization`,
// for values of a type, is told of it, none being chosen where the library
// realizes the entry once, and the entry told what its declaration says it is
// told of the type `called_at`.
auto ArrangeEntryCall(
    const lir::TypePool& types, RuntimeOp op,
    std::span<const StatedOperand> operands,
    const std::optional<Realization>& realization,
    std::optional<lir::TypeId> called_at) -> FnAbi;

// What an entry told `declared` of its answer is handed after its operands and
// after the type `realization` is told: what that says it is told of the type
// `called_at` the call is made at or of `answered`, the value the call answers,
// which it builds in storage it is handed where `answered_in_storage` says. A
// value laid out by its type that a realization builds is of the type it is
// realized for, since its declaration says it is told nothing else.
auto AnswerTypeArgs(
    const lir::TypePool& types, const std::optional<Realization>& realization,
    support::AnswerTold declared, std::optional<lir::TypeId> called_at,
    lir::TypeId answered, bool answered_in_storage) -> std::vector<TypeArg>;

// What crosses to the entry building a member of `type` held as `storage`,
// handed where it is.
auto ArrangeMemberBuild(
    const lir::CompilationUnit& unit, lir::TypeId type,
    support::DeclaredMemberStorage storage) -> FnAbi;

// What crosses to the entry building a variable of `type`, none where it has no
// storage.
auto ArrangeVariableBuild(const lir::CompilationUnit& unit, lir::TypeId type)
    -> std::optional<FnAbi>;

// What crosses to the entry carrying out `op`, a step of the life of a value of
// `type`, on each of `operands` values of it, as `object` says the entry acts:
// a value of the design as the realization for its domain is told it does, and
// an object that is no value of the design as the address it is.
auto ArrangeLibraryStep(
    const lir::TypePool& types, lir::TypeId type,
    const support::RuntimeObject& object, RuntimeOp op, std::size_t operands)
    -> FnAbi;

// The values a member of `type` holds: its own value, or what the storage it
// is holds. A member holding no value of the program's holds none.
auto ValuesHeldByMember(const lir::CompilationUnit& unit, lir::TypeId type)
    -> std::optional<lir::TypeId>;

// Which capability wrapper a type is, and the value that wrapper represents;
// nothing for a type that represents no storage. Classifying a wrapper in one
// place is what keeps an access reached through a place and an operation
// reached through an operand from disagreeing about what a wrapper is.
auto WrapperOf(const lir::Type& type)
    -> std::optional<std::pair<WrapperKind, lir::TypeId>>;

// The values a storage holds, where they are all of one representation;
// nothing for a type that is not such storage. A capability wrapper holds the
// value it represents, a history holds what each tick of one clocking event
// settled for one expression, and a place designated within a write reaches
// the value it designates -- one representation each way, which is what lets
// an entry reaching the storage be named once per representation rather than
// per call.
auto ValuesHeldBy(const lir::Type& type) -> std::optional<lir::TypeId>;

// The value domain a value of `type` is realized in, which names the entries
// acting on it; a type realized in no domain is one this backend does not
// carry.
auto RealizedDomainOf(const lir::CompilationUnit& unit, lir::TypeId type)
    -> diag::Result<support::ValueDomain>;

// What a variable of `type` is held as, none where this target has no storage
// for one: a body's variable, a cell built in storage it is handed, and storage
// the whole program shares are all held so.
auto VariableStorageOf(const lir::CompilationUnit& unit, lir::TypeId type)
    -> std::optional<support::DeclaredMemberStorage>;

}  // namespace lyra::backend::llvm_backend
