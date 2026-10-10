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
// type it keeps and acts on, how wide an integral type is and whether it holds
// x or z, or how wide alone.
struct TypeConstantArg {
  lir::TypeId type;
};
struct TypeExtentArg {
  lir::TypeId type;
};
struct TypeWidthArg {
  lir::TypeId type;
};
using TypeArg = std::variant<TypeConstantArg, TypeExtentArg, TypeWidthArg>;

// What a callee takes ahead of the arguments a call states. The host's
// `operator new` is asked for the size of a complete object of the type it
// allocates, and an entry making storage for the words of a value wider than a
// word is told how wide the values held there are.
struct NoImplicitArg {};
struct CompleteObjectSizeArg {
  lir::TypeId of;
};
struct HeldWidthArg {
  lir::TypeId of;
};
using ImplicitArg =
    std::variant<NoImplicitArg, CompleteObjectSizeArg, HeldWidthArg>;

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

// How a call on `op`, an entry this target names by an operation of its own,
// crosses where no call of the layer above states it: each of `operands` as
// the entry's declaration reads the operand at its place, the entry told what
// its declaration says it is told of the type `called_at`, and its answer
// coming back as `ret`.
auto FnAbiOf(
    CodeGenModule& module, RuntimeOp op,
    std::span<const StatedOperand> operands,
    std::optional<lir::TypeId> called_at, ReturnInfo ret) -> FnAbi;

// The types a call states that what the realization it enters is told of can
// be read from: the type of the values the storage it acts on holds, and the
// member a callee building or updating a union names. A call states only those
// its entry can be told of.
struct ToldTypes {
  std::optional<lir::TypeId> held;
  std::optional<lir::TypeId> member;
};

// How operand `index`, of type `own`, crosses where what a realization is told
// concerns it -- with what it is told, or as it; nothing where the realization
// is told nothing of that operand, which then crosses as its reading says.
auto ToldModeOf(
    const Told& told, std::size_t index, const ToldTypes& types,
    lir::TypeId own, const lir::TypePool& pool) -> std::optional<PassMode>;

// The library entry a call or an access enters: the symbol it is published
// under, and what the realization that symbol names is told, with the types
// that is read from.
struct LibraryEntry {
  std::string symbol;
  Told told;
  ToldTypes types;
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

  // How operand `index`, of type `operand`, crosses to an entry that reads its
  // operands as `readings` and whose realization is told `told`, held to what
  // its reading asks of a type: an ordinal is stated as a position, and an
  // operand read as the thing it is is no integral value.
  [[nodiscard]] auto ArgOf(
      const support::OperandReadings& readings, std::size_t index,
      lir::TypeId operand, const Told& told, const ToldTypes& types) const
      -> ArgAbi;

 private:
  // How the answer of `call`, a value of `result_type`, comes back: built in
  // storage the caller gives where the call makes a value its caller owns,
  // and as the machine value it is otherwise.
  [[nodiscard]] auto ReturnOf(
      const lir::CallInstr& call, lir::TypeId result_type) const -> ReturnInfo;
  // The construction of a value of type `result`: its entry, how that entry
  // reads the operands the construction states, and what leads them.
  struct Construction {
    std::string symbol;
    support::OperandReadings operands;
    ImplicitArg implicit_arg;
  };
  [[nodiscard]] auto ConstructionOf(
      const lir::CallInstr& call, lir::TypeId result) const
      -> diag::Result<Construction>;

  // A call's operands as an entry reading them as `readings`, whose
  // realization is told `told`, takes them.
  [[nodiscard]] auto ArgsAsRead(
      const lir::CallInstr& call, const support::OperandReadings& readings,
      const Told& told, const ToldTypes& types) const -> std::vector<ArgAbi>;

  [[nodiscard]] auto OperandType(const lir::Operand& operand) const
      -> lir::TypeId;
  // The storage an operand reaches: this target holds storage as its address,
  // and holds as a handle only what names storage someone else owns -- a
  // driver, which names a contribution the net owns, and a reference, which
  // names the storage a caller lent -- so an operand is either that address or
  // the handle itself, and either way reaches exactly one.
  [[nodiscard]] auto StorageReached(lir::TypeId operand) const
      -> const lir::Type&;
  // The representation the values held by the storage an operand reaches are
  // realized in, for an operation whose own name already says which storage it
  // acts on.
  [[nodiscard]] auto StorageDomainBehind(lir::TypeId operand) const
      -> diag::Result<support::ValueDomain>;

  CodeGenModule* module_;
  const lir::Function* fn_;
};

// Which capability wrapper a type is, and the value that wrapper represents;
// nothing for a type that represents no storage. Classifying a wrapper in one
// place is what keeps an access reached through a place and an operation
// reached through an operand from disagreeing about what a wrapper is.
auto WrapperOf(const lir::Type& type)
    -> std::optional<std::pair<WrapperKind, lir::TypeId>>;

// The type of a union's member `index`. Both union kinds hold their member
// types positionally, so this reads either one.
auto UnionMemberType(
    const lir::CompilationUnit& unit, lir::TypeId union_type,
    std::uint32_t index) -> lir::TypeId;

}  // namespace lyra::backend::llvm_backend
