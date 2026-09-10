#pragma once

#include <cstddef>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <variant>

#include "lyra/lir/function.hpp"
#include "lyra/lir/operator.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/net_resolution.hpp"
#include "lyra/support/value_domain.hpp"

namespace lyra::lir {
struct CompilationUnit;
}  // namespace lyra::lir

namespace lyra::backend::llvm_backend {

// What every runtime-library entry's symbol opens with. It is what tells an
// undefined name in a generated module apart from the ones resolved some other
// way -- another unit's generated symbol, a foreign function, the host's
// allocator -- so a reader of a module can say which absences the library is
// answerable for.
inline constexpr std::string_view kRuntimeSymbolPrefix = "lyra_rt_";

// The domain a LIR type is realized in, absent for a type the runtime library
// has no value realization for. The one place a LIR type is classified, so the
// entry a call names and the storage a cell owns cannot disagree.
auto ValueDomainOf(const lir::CompilationUnit& unit, lir::TypeId type)
    -> std::optional<support::ValueDomain>;

// The fold a LIR net type resolves under, as the runtime names it. The one
// place a LIR resolution is classified, so the storage a net owns and the
// backend that declared it cannot disagree about how it resolves.
auto NetResolutionOf(lir::NetResolution resolution) -> support::NetResolution;

// The type a keyed container's declared index is, absent for a container whose
// coordinates are ordinals its entries already name. An associative array holds
// no prototype for an index -- LRM 7.8 gives it no index bounds and no index
// default -- so nothing on the far side could know an index's representation,
// and the index states it by crossing erased in the representation the
// container's own declaration names.
auto DeclaredIndexType(const lir::CompilationUnit& unit, lir::TypeId container)
    -> std::optional<lir::TypeId>;

// How this target realizes an instruction or a construction the layer above it
// states, where that realization is a library call. Nothing outside this target
// names one: what is to be done is stated a layer up and how it happens is this
// target's alone, so another realizing the same instruction differently shares
// none of these. An operation any layer above can state is named in the entry
// set both targets read instead, and reaches a symbol through that name.
enum class RuntimeOp : std::uint8_t {
  kCellAlloc,
  kMemberAddress,
  kSequenceMake,
  kSequenceElement,
  kClosureMake,
  kObjectMake,
  kObjectDeref,
  kObjectMemberAddress,
  kObjectMethod,
  kClosureCapture,
  kConst,
  kToBool,
  kValueBox,
  kMake,
  kExtract,
  kUpdate,
  kTagMatches,
  kWithElement,
  kWithSlice,
  kDefault,
  kFromLiteral,
  kFromLiteralBounded,
  kFromEntriesDefault,
  kMakeScope,
  kMakeSegment,
  kMakeTrigger,
  kMakeObservedTrigger,
  kMakeObservation,
  kMakeQualifiedObservation,
  kMakeConditionObservation,
  kMakePackedRange,
  kMakePackedType,
  kMakePrintLiteralItem,
  kMakePrintValueItem,
  kMakeFormatSpec,
};

// What a member slot is for, which two declarations answer differently for a
// slot of the same type: a variable is written through its own store for as
// long as its owner lives, and a snapshot is filled once where its owner is
// built and only read afterwards.
enum class MemberSlotRole : std::uint8_t { kVariable, kSnapshot };

// How a member's storage is held. Everything that acts on a member reads it
// from here: the runtime side builds the storage the kind names, and code
// generation realizes a read and a write through it, so the storage a member
// gets and the access emitted for it are one statement rather than two.
enum class MemberStorageKind : std::uint8_t {
  // A subscribable variable: reached only through its own address, and a write
  // through it wakes whoever waited on it.
  kObservableCell,
  // A net's resolution node, likewise reached only through its address; a value
  // reaches it through a driver rather than by being written (LRM 6.5).
  kResolvedNet,
  // A variable the owner holds that nothing subscribes to: written and read
  // through its own storage, so a write keeps the representation the
  // declaration gave it and a read copies out rather than aliasing.
  kValueCell,
  // A value filled once where the owner is built and only read afterwards, so
  // the storage itself is what a read hands back.
  kInlineValue,
  // A box holding a handle the owner does not own, so a read reads the box
  // rather than what it names.
  kBorrowedHandle,
  // A named event (LRM 15.5), a scope's cancellation target (LRM 9.6.2), and
  // the joint cancel state of the channels a deferred write targets (LRM
  // 21.3.2). Each is a runtime record the owner holds and reaches only through
  // its address; none is read out as a value.
  kNamedEvent,
  kCancellationTarget,
  kChannelCancellation,
};

// The storage kind a member of `type` needs, or nothing where this backend has
// no realization for such a member. One arm per LIR type and no catch-all,
// because the kinds differ in what a write has to do: a type gained later fails
// to compile here until someone says which storage it needs.
auto MemberStorageKindOf(
    const lir::CompilationUnit& unit, lir::TypeId type, MemberSlotRole role)
    -> std::optional<MemberStorageKind>;

// Which capability wrapper storage is reached through. The wrappers share one
// access vocabulary -- a load, a store, and the install that fixes the
// storage's declared representation -- and differ in which of those they
// define, so this is what a type is classified into before an access through it
// is named, whether it arrived as a place or as an operand.
enum class WrapperKind : std::uint8_t { kCell, kNet, kDriver };

// The library realizes an operation once, whatever it is applied to: the
// runtime performs the work and what it acts on -- the engine, the file broker,
// a scope, a process -- has a single realization.
struct NamedAlone {};

// The library realizes the operation once per representation of one of the
// values the call carries, because `size` on a string and `size` on a dynamic
// array are different code. That value is the one the call qualifies itself
// with; where it qualifies itself with nothing, `operand` says which argument
// carries it -- the receiver for an operation on a value, and the destination
// for one that answers through an argument the call names.
struct NamedByValue {
  std::size_t operand = 0;
};

// The operation acts on the capability wrapper an argument reaches rather than
// on a value it is handed, and the wrappers each define it -- reading what one
// holds, replacing the whole of it, installing the storage's declared
// representation. So both halves come from that wrapper: the domain from the
// representation its storage holds, and which family of entries from which
// wrapper it is.
struct NamedByWrapper {};

// The operation likewise acts on the wrapper an argument reaches, but exists on
// one wrapper only -- attaching a driver, which only a net does -- so its own
// name is already unambiguous and only the domain comes from the wrapper.
struct NamedByWrapperDomain {};

// A conversion crosses two representations and its realization depends on both,
// so neither alone names it: the destination is the one the call qualifies
// itself with, and the source is its operand's.
struct NamedByConversion {};

// The operation has a shape this ABI cannot express, and carries which shape,
// since that is a property of the operation and not something a call site could
// answer.
struct NotRealized {
  std::string_view shape;
};

using EntryNaming = std::variant<
    NamedAlone, NamedByValue, NamedByWrapper, NamedByWrapperDomain,
    NamedByConversion, NotRealized>;

// How the entry behind a builtin is named. Total over the builtin set: what the
// library realizes for a builtin, and what it does not, is a property of the
// runtime library, so a builtin gaining an entry is a fact stated here.
auto EntryNamingOf(support::BuiltinFn fn) -> EntryNaming;

// The symbol a runtime entry is published under. An operation realized per
// value representation leads with that representation, so one library serves
// every representation and nothing about a value's type crosses at run time.
//
// Every overload takes the operation as itself rather than as text, so a symbol
// cannot be spelled from a string: an operation is nameable here only if some
// closed set already publishes its spelling.
auto RuntimeSymbol(RuntimeOp op) -> std::string;
auto RuntimeSymbol(support::ValueDomain domain, RuntimeOp op) -> std::string;
auto RuntimeSymbol(support::ValueDomain domain, lir::BinaryOp op)
    -> std::string;
auto RuntimeSymbol(support::ValueDomain domain, lir::UnaryOp op) -> std::string;
auto RuntimeSymbol(lir::ControlEffectTarget::Op op) -> std::string;
auto RuntimeSymbol(lir::CoroutineTarget::Op op) -> std::string;
auto RuntimeSymbol(support::ImportedRuntimeMethod method) -> std::string;
auto RuntimeSymbol(support::ValueDomain domain, lir::ValueCellTarget::Op op)
    -> std::string;
auto RuntimeSymbol(support::BuiltinFn fn) -> std::string;
auto RuntimeSymbol(support::ValueDomain domain, support::BuiltinFn fn)
    -> std::string;

// An access through a capability wrapper leads with the wrapper as well, since
// a cell, a net and a driver each answer a read of one domain differently. It
// refuses the two accesses no wrapper defines, which are upstream mistakes
// rather than gaps: a net's value is the fold of its drivers, so a value
// reaches it through one of them and never by being written (LRM 6.5), and a
// driver installs no representation of its own, because what it contributes
// before it drives is the identity the net gave it when it attached.
auto RuntimeSymbol(
    support::ValueDomain domain, WrapperKind wrapper, support::BuiltinFn fn)
    -> std::string;
auto RuntimeSymbol(
    support::ValueDomain destination, support::BuiltinFn fn,
    support::ValueDomain source) -> std::string;

}  // namespace lyra::backend::llvm_backend
