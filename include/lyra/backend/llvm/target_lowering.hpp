#pragma once

#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <variant>
#include <vector>

#include "lyra/backend/llvm/entry_realizations.hpp"
#include "lyra/backend/llvm/fn_abi.hpp"
#include "lyra/backend/llvm/runtime_entry.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/runtime/object_layout.hpp"
#include "lyra/support/declared_member_storage.hpp"
#include "lyra/support/library_decl.hpp"
#include "lyra/support/value_domain.hpp"

namespace lyra::backend::llvm_backend {

// A library entry a node of LIR is carried out by: the symbol it is published
// under, and what crosses to it.
struct EntryCall {
  std::string symbol;
  FnAbi abi;
};

// The library entry a call or an access enters: the symbol it is published
// under, and the realization that symbol names, none where the library
// realizes the entry once whatever it is applied to.
struct LibraryEntry {
  std::string symbol;
  std::optional<Realization> realization;
};

// A value cell whose value lies where the library states, so it is read and
// written there with no entry.
struct CellStore {
  support::ValueDomain domain;
  runtime::HeldContents contents;
};

// How a call of the layer above is carried out, the role of clang's `CGCallee`:
// on the entry or the body it names; as the operand it was handed, where a
// unique pointer is the address of the storage it takes over, as a Rust `Box`
// is; or where a value cell states its value lies.
struct OrdinaryCall {};
struct ForwardedOperand {};
struct CellLoad {
  support::ValueDomain domain;
};
using CallForm =
    std::variant<OrdinaryCall, ForwardedOperand, CellLoad, CellStore>;

// How an operator is carried out on operands of a type: by the machine, on
// addresses or machine integers; on identities of references owning nothing;
// on the floats a real is; on the words of an integral value; or by the entry
// the library realizes for the type's domain.
struct MachineOperator {
  lir::Signedness signedness;
};
struct IdentityOperator {};
struct RealOperator {};
struct IntegralOperator {};
using OperatorForm = std::variant<
    MachineOperator, IdentityOperator, RealOperator, IntegralOperator,
    EntryCall>;

// Which way a place is reached for, which is what a position naming nothing
// answers and which entry a container is asked through. A step into a container
// reaches a different element for each: a read of one that is not there reads
// the default and changes nothing, and a write to one allocates or discards it
// by the container's own rule.
enum class PlaceAccess : std::uint8_t { kRead, kWrite };

// A place naming a capability wrapper's contents, which have no address of
// their own, so an access goes through the wrapper: which wrapper it is, the
// domain that picks its library entries, and the place naming it.
struct WrapperPlace {
  support::ValueDomain domain{};
  WrapperKind kind{};
  lir::Place wrapper;
};

// How a store is carried out: through a wrapper, which is what gives the write
// its meaning; by a value cell's entry or, where the cell states its value
// lies, there; or into a value already at the place.
struct WrapperStore {
  lir::Place wrapper;
  EntryCall entry;
};
struct CellEntryStore {
  EntryCall entry;
};
struct AddressStore {};
using StoreForm =
    std::variant<WrapperStore, CellEntryStore, CellStore, AddressStore>;

// How a step into one element is taken: arithmetic within a fixed-size unpacked
// array, with what a position naming no element is answered by (LRM 7.4.5), or
// the entry its container is asked through and how each coordinate crosses.
struct StepWithinArray {
  EntryCall no_element;
};
struct StepByEntry {
  StepEntry entry;
  std::vector<PassMode> coordinates;
};
using ElementStep = std::variant<StepWithinArray, StepByEntry>;

// How each node of one function is carried out on this target, the role of
// LLVM's `TargetLowering`, which says whether an operation is an instruction or
// a library call. The query for what a unit needs and emission both ask it,
// which is what keeps them one decision. An answer here reads the types of a
// function's operands, so it is made per function, and reads LIR alone.
class TargetLowering {
 public:
  TargetLowering(const lir::CompilationUnit& unit, const lir::Function& fn);

  // How `call`, answering a value of `result_type`, is carried out.
  [[nodiscard]] auto CallFormOf(
      const lir::CallInstr& call, lir::TypeId result_type) const
      -> diag::Result<CallForm>;
  // What crosses `call`, an ordinary call answering a value of `result_type`. A
  // library entry's arguments cross as its declaration reads them; every other
  // callee is code whose parameters are already typed, so what the call states
  // is what crosses, and a cell's being built is what crosses to the entry
  // building it.
  [[nodiscard]] auto FnAbiOf(
      const lir::CallInstr& call, lir::TypeId result_type) const
      -> diag::Result<FnAbi>;
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
  // How `binary` is carried out.
  [[nodiscard]] auto OperatorFormOf(const lir::BinaryInstr& binary) const
      -> diag::Result<OperatorForm>;
  // How `store` is carried out.
  [[nodiscard]] auto StoreFormOf(const lir::StoreInstr& store) const
      -> diag::Result<StoreForm>;
  // The entry writing the bits `store` states within a value of type `whole`,
  // where the machine does not.
  [[nodiscard]] auto BitsStoreCallOf(
      const lir::BitsStoreInstr& store, lir::TypeId whole) const -> EntryCall;
  // How a step at `coordinates` into the value of `container` is taken for
  // `access`.
  [[nodiscard]] auto ElementStepOf(
      lir::TypeId container, std::span<const lir::Operand> coordinates,
      PlaceAccess access) const -> diag::Result<ElementStep>;
  // The storage begun or ended at `place`, of the type the place has, as the
  // declaration it lies in gives a member of it: none for the parts of an
  // object, which its class begins and ends in a function of their own.
  [[nodiscard]] auto StorageAt(const lir::Place& place) const
      -> diag::Result<std::optional<support::DeclaredMemberStorage>>;
  // The capability wrapper `place` names the contents of, where it does. It is
  // the one site that asks that of a place, so however deep the chain is an
  // access through a wrapper is classified once.
  [[nodiscard]] auto WrapperPlaceOf(const lir::Place& place) const
      -> diag::Result<std::optional<WrapperPlace>>;
  // The domain of the value cell `place` names, where the storage it reaches is
  // one, read and written through itself rather than off its address.
  [[nodiscard]] auto PlaceValueCellDomain(
      const lir::Place& place, lir::TypeId value) const
      -> std::optional<support::ValueDomain>;
  // Whether `call`, answering a value of `result_type`, builds its answer in
  // storage the caller gives, where the call makes a value its caller owns.
  [[nodiscard]] auto AnswersInStorage(
      const lir::CallInstr& call, lir::TypeId result_type) const -> bool;

 private:
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
  // The same for a type the library declares, whose declaration names
  // `constructor` as what builds one, each entry reached as the shared entry
  // declaration names it.
  [[nodiscard]] auto ConstructorEntryOf(
      const support::LibraryConstructor& constructor,
      const lir::CallInstr& call, lir::TypeId result) const
      -> diag::Result<Construction>;

  // A call's operands as the values of their types they are.
  [[nodiscard]] auto StatedOperands(const lir::CallInstr& call) const
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

  const lir::CompilationUnit* unit_;
  const lir::Function* fn_;
};

}  // namespace lyra::backend::llvm_backend
