#pragma once

#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/backend/llvm/runtime_entry.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/value_domain.hpp"

namespace llvm {
class Type;
}  // namespace llvm

namespace lyra::backend::llvm_backend {

class CodeGenModule;
class CodeGenTypes;

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
// it may hold (LRM 7.10.5).
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
using TypeArg =
    std::variant<TypeConstantArg, TypeExtentArg, ElementTypeArg, QueueTypeArg>;

// What a callee takes ahead of the arguments a call states. An entry making
// storage for the words of a value wider than a word is told how wide the
// values held there are, and one making storage for a value laid out by its
// type is told that type.
struct NoImplicitArg {};
struct WidthMetadataArg {
  lir::TypeId of;
};
struct TypeMetadataArg {
  lir::TypeId of;
};
using ImplicitArg =
    std::variant<NoImplicitArg, WidthMetadataArg, TypeMetadataArg>;

// One argument of a call as it is arranged, the role of rustc's `ArgAbi`: the
// machine type its value crosses at, and what crosses with it.
struct ArgAbi {
  llvm::Type* type;
  PassMode mode;
};

// A part the callee names, which is no value the program computed: it crosses
// as a machine integer, ahead of the argument at `before`.
struct NamedPartArg {
  std::size_t before;
  std::uint64_t position;
};

// How a callee's answer comes back, the role clang gives the `ABIArgInfo` of a
// function's return: as the machine value it is, or built in storage the
// caller gives, which the callee is handed last. A callee building its answer
// there returns a machine value too: the address of that storage, or a count
// where the entry answers one beside the value it lays out.
struct ReturnDirect {
  llvm::Type* type;
};
struct ReturnIndirect {
  llvm::Type* returned;
};
using ReturnInfo = std::variant<ReturnDirect, ReturnIndirect>;

// An operand of a call this target states on its own account: a value of a
// type of the layer above, or a machine value this target computed.
struct ValueOperand {
  lir::TypeId type;
};
struct MachineOperand {
  llvm::Type* type;
};
using StatedOperand = std::variant<ValueOperand, MachineOperand>;

// How one call crosses to its callee, the role of rustc's `FnAbi` and clang's
// `CGFunctionInfo`: what the callee takes ahead of the arguments, one
// arrangement per argument the call states with the part the callee names
// among them, what the callee is told of types, which follows them all, and
// how its answer comes back. The machine type a callee is declared at is a
// function of this and of nothing else.
struct FnAbi {
  ImplicitArg implicit_arg;
  std::optional<NamedPartArg> named_part;
  std::vector<ArgAbi> args;
  std::vector<TypeArg> type_args;
  ReturnInfo ret;
};

// How an operand of type `operand` crosses to an entry that reads it as
// `reading`: the reading alone decides, and the type is only where a constant
// crossing with the value is read from.
auto PassModeOf(support::OperandReading reading, lir::TypeId operand)
    -> PassMode;

// The realization of an entry that the library compiles once per
// representation, chosen for one call: what it is told, and the type it is
// realized for -- the value whose representation names it, or the values held
// by the storage it acts on -- which is where what it is told of a type is
// read from.
struct Realization {
  Told told;
  lir::TypeId of;
};

// How a call crosses to the entry `realization` realizes, none where the
// library realizes it once: each of `operands` as `readings` reads the operand
// at its place and as the realization is told of it, the part the callee
// names, the type the realization is told after them and then what the entry
// is told of its answer, and its answer coming back as `ret`. Every call on a
// library entry is arranged here, the ones this target states on its own
// account included.
auto ArrangeCall(
    CodeGenModule& module, const support::OperandReadings& readings,
    std::span<const StatedOperand> operands,
    const std::optional<Realization>& realization,
    std::optional<NamedPartArg> named_part,
    std::vector<TypeArg> told_of_the_answer, ReturnInfo ret) -> FnAbi;

// How a call on `op`, an entry this target names by an operation of its own,
// crosses where no call of the layer above states it: each of `operands` as
// the entry's declaration reads the operand at its place and as `realization`,
// for values of a type, is told of it, none being chosen where the library
// realizes the entry once; the entry told what its declaration says it is
// told of the type `called_at`; and its answer coming back as `ret`.
auto FnAbiOf(
    CodeGenModule& module, RuntimeOp op,
    std::span<const StatedOperand> operands,
    const std::optional<Realization>& realization,
    std::optional<lir::TypeId> called_at, ReturnInfo ret) -> FnAbi;

// The library entry a call or an access enters: the symbol it is published
// under, and the realization that symbol names, none where the library
// realizes the entry once whatever it is applied to.
struct LibraryEntry {
  std::string symbol;
  std::optional<Realization> realization;
};

// Arranges how the arguments of a call cross, apart from emitting the call, as
// clang keeps `CodeGenTypes::arrange*` apart from `CodeGenFunction::EmitCall`.
// Everything here follows from the callee's declaration and from types, so
// nothing is emitted and no value is read.
class CallArranger {
 public:
  CallArranger(CodeGenModule& module, const lir::Function& fn);

  // How `call`, answering a value of `result_type`, crosses. A library
  // entry's arguments cross as its declaration reads them; every other callee
  // is code whose parameters are already typed, so what the call states is
  // what crosses.
  [[nodiscard]] auto FnAbiOf(
      const lir::CallInstr& call, lir::TypeId result_type) const
      -> diag::Result<FnAbi>;
  // A call this target makes on its own account, stating no call of the layer
  // above: `args` in order, answering as `ret` says, with nothing led by and
  // nothing told of a type.
  [[nodiscard]] static auto Arrange(std::vector<ArgAbi> args, ReturnInfo ret)
      -> FnAbi;
  // An argument that is the machine value of `type` it is: an address this
  // target computed, or a count it states.
  [[nodiscard]] static auto Direct(llvm::Type* type) -> ArgAbi;
  // How a callee making a value of `type` hands it back, the one return
  // arrangement its declaration, its body and every call of it read, as
  // clang's `CGFunctionInfo` is: built in storage the caller gives where the
  // value lies in storage, and as the machine value it is otherwise.
  [[nodiscard]] static auto ReturnOfValue(CodeGenTypes& types, lir::TypeId type)
      -> ReturnInfo;

  // The entry a call on a builtin enters.
  [[nodiscard]] auto EntryOf(
      const lir::BuiltinTarget& target, const lir::CallInstr& call,
      lir::TypeId result_type) const -> diag::Result<LibraryEntry>;
  // The entry that brings a value of type `result` into existence, given the
  // operands the construction states. A type comes into existence one way, so
  // naming it names the entry.
  [[nodiscard]] auto ConstructorOf(
      const lir::CallInstr& call, lir::TypeId result) const
      -> diag::Result<std::string>;

  // How operand `index`, of type `operand`, crosses to an entry realized once
  // that reads its operands as `readings`, held to what its reading asks of a
  // type: an ordinal is stated as a position, and an operand read as the thing
  // it is is no integral value.
  [[nodiscard]] auto ArgOf(
      const support::OperandReadings& readings, std::size_t index,
      lir::TypeId operand) const -> ArgAbi;

 private:
  // How the answer of `call`, a value of `result_type`, comes back: built in
  // storage the caller gives where the call makes a value its caller owns,
  // and as the machine value it is otherwise.
  [[nodiscard]] auto ReturnOf(
      const lir::CallInstr& call, lir::TypeId result_type) const -> ReturnInfo;
  // The construction of a value of type `result`: its entry, how that entry
  // reads the operands the construction states, the realization it enters,
  // none where the library realizes it once, and what the entry is told of
  // the type it answers at.
  struct Construction {
    std::string symbol;
    support::OperandReadings operands;
    std::optional<Realization> realization;
    support::AnswerTold answer_told;
  };
  [[nodiscard]] auto ConstructionOf(
      const lir::CallInstr& call, lir::TypeId result) const
      -> diag::Result<Construction>;

  // A call's operands as the values of their types they are.
  [[nodiscard]] auto Stated(const lir::CallInstr& call) const
      -> std::vector<StatedOperand>;

  [[nodiscard]] auto OperandType(const lir::Operand& operand) const
      -> lir::TypeId;
  // The storage an operand reaches: this target holds storage as its address,
  // and holds as a handle only what names storage someone else owns -- a
  // driver, which names a contribution the net owns, and a reference, which
  // names the storage a caller lent -- so an operand is either that address or
  // the handle itself, and either way reaches exactly one.
  [[nodiscard]] auto StorageReached(lir::TypeId operand) const
      -> const lir::Type&;
  // The type of the values held by the storage an operand reaches, for an
  // operation whose own name already says which storage it acts on.
  [[nodiscard]] auto ValuesBehind(lir::TypeId operand) const -> lir::TypeId;

  CodeGenModule* module_;
  const lir::Function* fn_;
};

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

}  // namespace lyra::backend::llvm_backend
