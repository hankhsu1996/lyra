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

// An operation the execution model performs that the source language does not
// spell as a call. The builtin set names the ones it does spell; these are the
// rest, listed together because the program never writes them, so nothing
// upstream carries a name for them.
enum class RuntimeOp : std::uint8_t {
  kCellAlloc,
  kCellInitialize,
  kCellGet,
  kCellSet,
  kNetInitialize,
  kNetGet,
  kDriverGet,
  kDriverSet,
  kMemberAddress,
  kSequenceMake,
  kSequenceElement,
  kClosureMake,
  kObjectMake,
  kObjectDeref,
  kObjectMemberAddress,
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
  kDefaultBounded,
  kFromLiteral,
  kFromLiteralBounded,
  kFromEntries,
  kFromEntriesDefault,
  kMakeScope,
  kMakeSegment,
  kMakeTrigger,
  kMakePackedRange,
  kMakePackedType,
  kMakePrintLiteralItem,
  kMakePrintValueItem,
  kMakeFormatSpec,
  kMakeFormatSpecOfKind,
};

// Which capability wrapper a place reaches storage through. The wrappers share
// one access vocabulary -- a load, a store, and the install that fixes the
// storage's declared representation -- and differ in which of those they define
// and which entry realizes each, so this is what a place's type is classified
// into before an access through it is named.
enum class WrapperKind : std::uint8_t { kCell, kNet, kDriver };

// The entry a load through a wrapper reaches. Every wrapper defines one: a cell
// and a net answer with the value they hold, and a driver with the contribution
// a partial drive is about to replace part of (LRM 6.6.1).
auto LoadOpOf(WrapperKind kind) -> RuntimeOp;

// The entry a store through a wrapper reaches. A net defines none, because its
// value is the fold of its drivers: a value reaches it through one of them and
// never by being written (LRM 6.5), so a store naming a net is something
// upstream built that it should not have.
auto StoreOpOf(WrapperKind kind) -> RuntimeOp;

// The entry that fixes a wrapper's storage at its declared representation. A
// driver defines none: what it contributes before it drives is the fold's
// identity at the net's shape, which the net gives it when it attaches.
auto InstallOpOf(WrapperKind kind) -> RuntimeOp;

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

// The operation acts on the capability wrapper an argument addresses rather
// than on a value it is handed, and it is one every wrapper defines under its
// own entry name -- installing the storage's declared representation. So both
// halves come from that wrapper: the domain from the representation its storage
// holds, and which entry from which wrapper it is.
struct NamedByWrapperInstall {};

// The operation likewise acts on the wrapper an argument addresses, but exists
// on one wrapper only -- attaching a driver, which only a net does -- so its
// own name is already unambiguous and only the domain comes from the wrapper.
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
    NamedAlone, NamedByValue, NamedByWrapperInstall, NamedByWrapperDomain,
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
auto RuntimeSymbol(support::ValueDomain domain, lir::ValueCellTarget::Op op)
    -> std::string;
auto RuntimeSymbol(support::BuiltinFn fn) -> std::string;
auto RuntimeSymbol(support::ValueDomain domain, support::BuiltinFn fn)
    -> std::string;
auto RuntimeSymbol(
    support::ValueDomain destination, support::BuiltinFn fn,
    support::ValueDomain source) -> std::string;

}  // namespace lyra::backend::llvm_backend
