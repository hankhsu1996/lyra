#pragma once

#include <cstddef>
#include <cstdint>
#include <initializer_list>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <variant>

#include "lyra/lir/function.hpp"
#include "lyra/lir/operator.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/integral_operation.hpp"
#include "lyra/support/member_storage_kind.hpp"
#include "lyra/support/runtime_object.hpp"
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

// The runtime object a value of a LIR type is held as, and nothing for a type
// whose value is held as itself or reached where it lives. The layer above
// says of an integral value only that it is its bits; which layout holds them
// -- up to a word the storage unit one plane takes and whether an unknown
// plane follows it, and above a word which planes it has -- is classified
// here.
auto HeldObjectOf(const lir::Type& type)
    -> std::optional<support::RuntimeObject>;

// The domain a LIR type is realized in, absent for a type whose values are not
// values of the design. It is the type's held object where that object is a
// domain's, so the entry a call names and the storage a cell owns read one
// classification.
auto ValueDomainOf(const lir::CompilationUnit& unit, lir::TypeId type)
    -> std::optional<support::ValueDomain>;

// Whether a value held in `domain` is reached by a key rather than by a
// position: an associative array, which holds no index type, so a key crosses
// with the type it was written in (LRM 7.8).
auto ReachedByKey(support::ValueDomain domain) -> bool;

// Some of the value domains.
class DomainSet {
 public:
  constexpr DomainSet() = default;
  constexpr DomainSet(std::initializer_list<support::ValueDomain> domains) {
    for (const support::ValueDomain domain : domains) {
      bits_ |= BitOf(domain);
    }
  }

  [[nodiscard]] constexpr auto Holds(support::ValueDomain domain) const
      -> bool {
    return (bits_ & BitOf(domain)) != 0;
  }

  [[nodiscard]] constexpr auto operator|(DomainSet other) const -> DomainSet {
    DomainSet both;
    both.bits_ = bits_ | other.bits_;
    return both;
  }

 private:
  static constexpr auto BitOf(support::ValueDomain domain) -> std::uint32_t {
    return std::uint32_t{1} << static_cast<std::uint32_t>(domain);
  }

  std::uint32_t bits_ = 0;
};

// What one realization of an entry is told of a type, beyond what the readings
// of its operands cross with. Storage holding a value's bytes reads everything
// off its own layout, so most realizations are told nothing. Storage holding
// the words of a wider value, and a designation of an integral value, are told
// how wide the values held there are, after operand `after`, and an entry
// making such storage out of nothing is told it ahead of whatever it takes. A
// memory reached by a key is told the type its keys are built at (LRM 21.4.1),
// and a union the type of the member its callee names (LRM 7.3), each after
// the operand it concerns.
struct ToldNothing {};
struct ToldHeldWidth {
  std::size_t after;
};
struct ToldHeldWidthAhead {};
struct ToldKeyType {
  std::size_t after;
};
struct ToldMemberType {
  std::size_t after;
};
using Told = std::variant<
    ToldNothing, ToldHeldWidth, ToldHeldWidthAhead, ToldKeyType,
    ToldMemberType>;

// The domains the library realizes an entry for that are told the same thing.
// An entry states one of these per thing its realizations are told, and a
// domain in none of them is one the library holds no realization for.
struct RealizedFor {
  DomainSet domains;
  Told told = ToldNothing{};
};

// A library entry a step into a value is taken by, and the symbol its
// realization for that value is published under.
struct StepEntry {
  support::BuiltinFn entry;
  std::string symbol;
};

// The entries a step to one element of a value held in `domain` is taken by:
// the one answering with the element as it is, and the one answering with the
// storage a write lands in. An associative array is reached by a key and every
// other container by a position (LRM 7.8, 7.4.5), which are entries of their
// own.
struct ElementEntries {
  StepEntry read;
  StepEntry write;
};
auto ElementEntriesOf(support::ValueDomain domain) -> ElementEntries;

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
  kClosureMake,
  kObjectAdopt,
  kSharedCellMake,
  kSharedPointerDeref,
  kHandleView,
  kHandleWithView,
  kConst,
  kToBool,
  kMake,
  // An index of a wildcard-indexed associative array as the array keeps one:
  // the integral value with the type it was written in, which no index type
  // states (LRM 7.8.1).
  kMakeWildcardIndex,
  kWithComponent,
  kWithElement,
  kWithSlice,
  kDefault,
  kFromLiteral,
  kFromLiteralBounded,
  kFromEntriesDefault,
  kFromEntriesDefaultWildcard,
  kMakeSegment,
  kMakeTrigger,
  kMakePrintLiteralItem,
  // A print item and a format argument over a value: one entry per domain of a
  // value the library holds as an object, and one serving every integral type,
  // which reads the value as a number (LRM 21.2.1).
  kMakePrintValueItem,
  kMakeIntegralPrintValueItem,
  kMakeFormatSpec,
  kMakeFormatArg,
  kMakeIntegralFormatArg,
  kMakeDpiBitBuffer,
  kMakeDpiLogicBuffer,
  kMakeDpiOpenArray,
  kSettleDeparture,
  // An integral value's bits written into a stream of bits, and the integral
  // value planes hold laid out as one: what stands between a structure's own
  // operations and the type the library asks them through.
  kStreamWrite,
  kStreamRead,
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

// The symbol of the type of a value held in `domain`, which the library
// defines once per kind of value it holds as an object of its own and
// generated code hands over beside such a value. It names data, where every
// other symbol here names an entry.
auto ValueTypeSymbol(support::ValueDomain domain) -> std::string;

// What a member slot is for, which two declarations answer differently for a
// slot of the same type: a variable is written through its own store for as
// long as its owner lives, and a snapshot is filled once where its owner is
// built and only read afterwards.
enum class MemberSlotRole : std::uint8_t { kVariable, kSnapshot };

// What the members a declaration gives its values are for. A closure's are
// copies taken where it is built and only read after; every other
// declaration's are variables of the value that holds them.
auto MemberSlotRoleOf(const lir::TypeDeclaration& declaration)
    -> MemberSlotRole;

// The storage kind a member of `type` needs, or nothing where this backend has
// no realization for such a member. One arm per LIR type and no catch-all,
// because the kinds differ in what a write has to do: a type gained later fails
// to compile here until someone says which storage it needs.
auto MemberStorageKindOf(
    const lir::CompilationUnit& unit, lir::TypeId type, MemberSlotRole role)
    -> std::optional<support::MemberStorageKind>;

// What an artifact states about one member's storage: the kind above, and the
// value domain that kind holds. Which type the domain is read from follows from
// the kind, so this is the one place a declaration's type becomes the pair the
// runtime builds storage from.
auto DeclaredStorageOf(
    const lir::CompilationUnit& unit, lir::TypeId type, MemberSlotRole role)
    -> std::optional<support::DeclaredMemberStorage>;

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

// The library realizes an operation once, whatever it is applied to: the
// runtime performs the work and what it acts on -- the engine, the file broker,
// a scope, a process -- has a single realization.
struct NamedAlone {};

// The library realizes the operation once per representation of one of the
// values the call carries, because `size` on a string and `size` on a dynamic
// array are different code. `operand` says which argument carries it -- the
// receiver for an operation on a value, and the destination for one that
// answers through an argument the call names.
struct NamedByValue {
  std::size_t operand = 0;
  std::span<const RealizedFor> realized;
};

// The library realizes the operation once per representation likewise, but the
// value whose representation names it is the one the call builds: a factory
// acts on no object and takes no destination, so nothing it is handed carries
// the representation and only its own result does.
struct NamedByResult {
  std::span<const RealizedFor> realized;
};

// The domains one capability wrapper defines an access for that are told the
// same thing. A wrapper an access states none for does not define it: a net's
// value is the fold of its drivers, so it takes no store and no write (LRM
// 6.5), and its install names the fold it resolves under; a driver and a
// reference install nothing, the storage each names having been installed
// where it was declared; and what a time slot moved away from is retained
// only where a variable holds it (LRM 16.5.1).
struct ThroughWrapper {
  WrapperKind wrapper;
  DomainSet domains;
  Told told = ToldNothing{};
};

// The operation acts on the capability wrapper an argument reaches rather than
// on a value it is handed, and the wrappers each define it -- reading what one
// holds, replacing the whole of it, installing the storage's declared
// representation. So both halves come from that wrapper: the domain from the
// representation its storage holds, and which family of entries from which
// wrapper it is.
struct NamedByWrapper {
  std::span<const ThroughWrapper> realized;
};

// The operation likewise acts on storage an argument reaches rather than on a
// value it is handed, but its own name already says which storage -- attaching
// a driver, which only a net does; filling, appending to and reading a sampled
// value history -- so only the domain comes from the storage, and it is the
// representation of the values that storage holds.
struct NamedByStorageDomain {
  std::span<const RealizedFor> realized;
};

// One conversion the library realizes: the representation it builds, out of
// which.
struct Conversion {
  support::ValueDomain destination;
  support::ValueDomain source;
};

// A conversion crosses two representations and its realization depends on both,
// so neither alone names it: the destination is the value the call builds, and
// the source is its operand's.
struct NamedByConversion {
  std::span<const Conversion> realized;
};

// An operation over integral values, which this target emits as that operation
// and never as a call on an entry named here.
struct OverIntegralValues {};

// The operation has a shape this ABI cannot express, and carries which shape,
// since that is a property of the operation and not something a call site could
// answer.
struct NotRealized {
  std::string_view shape;
};

using EntryNaming = std::variant<
    NamedAlone, NamedByValue, NamedByResult, NamedByWrapper,
    NamedByStorageDomain, NamedByConversion, OverIntegralValues, NotRealized>;

// How the entry behind a builtin is named, and which representations the
// library realizes it for. Total over the builtin set: what the library
// realizes for a builtin, and what it does not, is a property of the runtime
// library, so a builtin gaining an entry or a realization is a fact stated
// here, and a call on a representation outside what is stated is refused where
// its symbol is composed.
auto EntryNamingOf(support::BuiltinFn fn) -> EntryNaming;

// The representations the library realizes an entry of this target's own for,
// where it realizes that entry per representation of a value; none for an
// entry it realizes once or per object.
auto RealizationsOf(RuntimeOp op) -> std::span<const RealizedFor>;
auto RealizationsOf(lir::ValueCellTarget::Op op)
    -> std::span<const RealizedFor>;
auto RealizationsOf(lir::OpenWriteTarget::Op op)
    -> std::span<const RealizedFor>;
auto RealizationsOf(lir::DesignatedBitsTarget::Op op)
    -> std::span<const RealizedFor>;

// What the realization of an entry for `domain` is told. A representation the
// library holds no realization of the entry for is a compiler defect, refused
// here and where its symbol is composed alike.
auto ToldOf(support::ValueDomain domain, RuntimeOp op) -> Told;
auto ToldOf(support::ValueDomain domain, lir::ValueCellTarget::Op op) -> Told;
auto ToldOf(support::ValueDomain domain, lir::OpenWriteTarget::Op op) -> Told;
auto ToldOf(support::ValueDomain domain, lir::DesignatedBitsTarget::Op op)
    -> Told;
auto ToldOf(support::ValueDomain domain, support::BuiltinFn fn) -> Told;
auto ToldOf(
    support::ValueDomain domain, WrapperKind wrapper, support::BuiltinFn fn)
    -> Told;

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
// Whether the entry building a member's storage is told, after the storage it
// builds, how wide the values held there are: a variable nothing subscribes to
// that holds the words of a value wider than a word has no declaration to
// install it, so it is built at its width.
auto BuiltAtTheWidthHeld(support::DeclaredMemberStorage storage) -> bool;
auto RuntimeSymbol(support::ValueDomain domain, lir::BinaryOp op)
    -> std::string;
auto RuntimeSymbol(support::ValueDomain domain, lir::UnaryOp op) -> std::string;
auto RuntimeSymbol(lir::ControlEffectTarget::Op op) -> std::string;
auto RuntimeSymbol(lir::CoroutineTarget::Op op) -> std::string;
auto RuntimeSymbol(support::ValueDomain domain, lir::ValueCellTarget::Op op)
    -> std::string;
auto RuntimeSymbol(support::ValueDomain domain, lir::OpenWriteTarget::Op op)
    -> std::string;
auto RuntimeSymbol(
    support::ValueDomain domain, lir::DesignatedBitsTarget::Op op)
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
auto OperandReadingsOf(lir::DesignatedBitsTarget::Op op)
    -> support::OperandReadings;
auto AnswerToldOf(lir::DesignatedBitsTarget::Op op) -> support::AnswerTold;

// The operation an operator of the layer above is over integral values (LRM
// 11.4).
auto IntegralOpOf(lir::BinaryOp op) -> support::IntegralOp;
auto IntegralOpOf(lir::UnaryOp op) -> support::IntegralOp;

}  // namespace lyra::backend::llvm_backend
