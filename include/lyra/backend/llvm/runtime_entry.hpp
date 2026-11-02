#pragma once

#include <cstdint>
#include <string>
#include <string_view>

#include "lyra/lir/function.hpp"
#include "lyra/lir/operator.hpp"
#include "lyra/lir/value_step.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/declared_member_storage.hpp"
#include "lyra/support/runtime_object.hpp"
#include "lyra/support/value_domain.hpp"

namespace lyra::backend::llvm_backend {

// What every runtime-library entry's symbol opens with. It is what tells an
// undefined name in a generated module apart from the ones resolved some other
// way -- another unit's generated symbol, a foreign function, the host's
// allocator -- so a reader of a module can say which absences the library is
// answerable for.
inline constexpr std::string_view kRuntimeSymbolPrefix = "lyra_rt_";

// A library entry a step into a value is taken by, and the symbol its
// realization for that value is published under.
struct StepEntry {
  support::BuiltinFn entry;
  std::string symbol;
};

// The entries a step to one element of a value is taken by: the one answering
// with the element as it is, and the one answering with the storage a write
// lands in. An associative array is reached by a key, which is an entry of its
// own serving every one, and every other container by a position, realized per
// value domain it is held in (LRM 7.8, 7.4.5). A queue's write has an entry of
// its own too, the one that can report (LRM 7.10.5).
struct ElementEntries {
  StepEntry read;
  StepEntry write;
};
auto KeyedElementEntries() -> ElementEntries;
auto QueueElementEntries() -> ElementEntries;
auto PositionedElementEntries(support::ValueDomain domain) -> ElementEntries;

// How this target realizes an instruction or a construction the layer above it
// states, where that realization is a library call. Nothing outside this target
// names one: what is to be done is stated a layer up and how it happens is this
// target's alone, so another realizing the same instruction differently shares
// none of these. An operation any layer above can state is named in the entry
// set both targets read instead, and reaches a symbol through that name.
enum class RuntimeOp : std::uint8_t {
  // A reference built over an address: over a subscribable variable's cell,
  // named by the domain the cell holds since where its value lies inside it is
  // the cell's type's to say, or over storage nothing is told about, which is
  // the value's own address and so names nothing.
  kCellRefer,
  kReferStorage,
  kRunProgram,
  kSequenceMake,
  kSequenceElement,
  kClosureBoxEnd,
  kObjectAdopt,
  kSharedCellMake,
  kSharedPointerDeref,
  // The table of the bodies an object's class answers one interface it
  // implements with, found among those its definition lists.
  kInterfaceTable,
  // The part of an object the runtime library defines, built and ended where
  // the object lies, before and after every member of its own.
  kBeginLibraryPart,
  kEndLibraryPart,
  kToBool,
  // What a construction taking nothing builds holding nothing, for a value
  // this target builds whose type the library does not declare: a null class
  // handle, and a reference to a value of any type referring to nothing.
  kDefault,
  // A fixed-size unpacked array built from an element list, laid down as many
  // times over as fill the array: how many elements there are is the type's
  // to say.
  kFromElements,
  kSettleDeparture,
  // What stands between a type's own operations and the type the library asks
  // them through: the stream a type's function answers written into the stream
  // the type was handed, and a value built by its type's function out of such
  // a stream; and the same two for a type whose stream is a run of bits as long
  // as the value makes it.
  kStreamWrite,
  kStreamBuild,
  kRunWrite,
  kRunBuild,
  // Bits written into an integral value where they lie (LRM 11.5.1): into the
  // words of a value held at a place, and into the value a write in progress
  // designates, which hears which of them moved and is named by the domain
  // that value is held in.
  kInsertBits,
  kInsertDesignatedBits,
  // What a position of a fixed-size unpacked array naming no element reaches
  // (LRM 7.4.5), which the element type answers since no array is asked: the
  // default a read yields, and storage no read reaches for a write to land in.
  kElementDefault,
  kDiscardedWrite,
  // Ending an object held in the generated body's own storage, copying one into
  // further storage, moving one into storage that takes it over -- a slot, or
  // what a caller gave for a body's answer -- and writing one into an object
  // already there, which goes on being that object. Each is named by the object
  // it acts on. Building one member's storage empty where its owner was laid
  // out is named by that storage, and so is ending it.
  kConstruct,
  kDestroy,
  kCopy,
  kMove,
  kAssign,
};

// The operation's stable spelling, which is the operation half of the symbol
// of every entry realizing it.
auto RuntimeOpName(RuntimeOp op) -> std::string_view;

// The operation of the library's that carries out `step` on a value it
// realizes.
auto RuntimeOpOf(lir::ValueStep step) -> RuntimeOp;

// The symbol of the type of a value held in `domain`, which the library
// defines once per kind of value it holds as an object of its own and
// generated code hands over beside such a value. It names data, where every
// other symbol here names an entry.
auto ValueTypeSymbol(support::ValueDomain domain) -> std::string;

// The symbol of the order an associative array indexed by values held in
// `domain` keeps its entries in (LRM 7.8), which the library defines once per
// kind of index: one for every integral type, and one for each other kind the
// language admits as an index, a wildcard index's among them (LRM 7.8.1).
auto IndexOrderSymbol(support::ValueDomain domain) -> std::string;

// The symbols of what the library states of the language's operations on a
// value held in `domain`: the functions of a kind, which a container type's
// operations name with its element's, and the whole of the operations of a
// kind no other type parameterizes.
auto OperationFunctionsSymbol(support::ValueDomain domain) -> std::string;
auto ValueOperationsSymbol(support::ValueDomain domain) -> std::string;

// Witnesses the library defines once for every type of a kind, which generated
// code names where it states such a type, and the entry a table names for an
// operation the language does not define for its type.
inline constexpr std::string_view kIntegralValueWitnessTableSymbol =
    "lyra_rt_integral_value_witness_table";
inline constexpr std::string_view kIntegralOperationFunctionsSymbol =
    "lyra_rt_integral_operation_functions";
inline constexpr std::string_view kFixedArrayPartsWitnessTableSymbol =
    "lyra_rt_fixed_array_parts_witness_table";
inline constexpr std::string_view kTypeLacksOperationSymbol =
    "lyra_rt_type_lacks_operation";

// Which capability wrapper storage is reached through. The wrappers share one
// access vocabulary -- a load, a store, the install that fixes the storage's
// declared representation, and the pair that arms one to retain what a time
// slot moved away from and reads back what it retained -- and differ in which
// of those they define, so this is what a type is classified into before an
// access through it is named, whether it arrived as a place or as an operand.
//
// A reference is the one whose answer is not decided by its type. It names
// storage the caller lent (LRM 13.5.2), which may be a subscribable variable
// or storage nothing subscribes to, and a body that takes one is lowered once
// for every caller -- so which of the two it holds travels with the address
// and the entry resolves it, where every other wrapper here is settled by the
// type alone.
enum class WrapperKind : std::uint8_t { kCell, kNet, kDriver, kRef };

// The family of entries a capability wrapper defines its accesses in, which is
// part of each one's symbol.
auto WrapperName(WrapperKind wrapper) -> std::string_view;

// The symbol a runtime entry is published under. An operation realized per
// value representation leads with that representation, so one library serves
// every representation, and naming one for a representation the library does
// not realize it for is refused.
//
// Every overload takes the operation as itself rather than as text, so a symbol
// cannot be spelled from a string: an operation is nameable here only if some
// closed set already publishes its spelling.
auto RuntimeSymbol(RuntimeOp op) -> std::string;
auto RuntimeSymbol(support::ValueDomain domain, RuntimeOp op) -> std::string;
auto RuntimeSymbol(support::RuntimeObject object, RuntimeOp op) -> std::string;
// A member's storage leads with the domain it holds values of, where its kind
// holds any, and then the kind.
auto RuntimeSymbol(support::DeclaredMemberStorage storage, RuntimeOp op)
    -> std::string;
auto RuntimeSymbol(support::ValueDomain domain, lir::BinaryOp op)
    -> std::string;
auto RuntimeSymbol(lir::ControlEffectTarget::Op op) -> std::string;
auto RuntimeSymbol(lir::CoroutineTarget::Op op) -> std::string;
auto RuntimeSymbol(support::ValueDomain domain, lir::ValueCellTarget::Op op)
    -> std::string;
auto RuntimeSymbol(support::ValueDomain domain, lir::OpenWriteTarget::Op op)
    -> std::string;
auto RuntimeSymbol(support::BuiltinFn fn) -> std::string;
auto RuntimeSymbol(support::ValueDomain domain, support::BuiltinFn fn)
    -> std::string;

// An access through a capability wrapper leads with the wrapper as well, since
// a cell, a net, a driver and a reference each answer a read of one domain
// differently. An access the wrapper does not define is refused rather than
// spelled, because it is an upstream mistake and not a gap.
auto RuntimeSymbol(
    support::ValueDomain domain, WrapperKind wrapper, support::BuiltinFn fn)
    -> std::string;
auto RuntimeSymbol(
    support::ValueDomain destination, support::BuiltinFn fn,
    support::ValueDomain source) -> std::string;

// How an entry this target names by an operation of its own reads each operand
// it is handed, the object or the storage it acts on first. One that states
// none takes every operand as the thing it is.
auto OperandReadingsOf(RuntimeOp op) -> support::OperandReadings;
// What such an entry is told of the type it answers at.
auto AnswerToldOf(RuntimeOp op) -> support::AnswerTold;
auto OperandReadingsOf(lir::ValueCellTarget::Op op) -> support::OperandReadings;
auto OperandReadingsOf(lir::OpenWriteTarget::Op op) -> support::OperandReadings;

}  // namespace lyra::backend::llvm_backend
