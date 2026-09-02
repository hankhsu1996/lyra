#pragma once

#include <cstddef>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <variant>

#include "lyra/lir/function.hpp"
#include "lyra/lir/operator.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/support/builtin_fn.hpp"
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
  kMemberAddress,
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

// The operation reaches through a storage cell rather than acting on a value it
// is handed, so the representation that names it is the one the cell's contents
// take.
struct NamedByCellValue {};

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
    NamedAlone, NamedByValue, NamedByCellValue, NamedByConversion, NotRealized>;

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
