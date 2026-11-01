#pragma once

#include <cstddef>
#include <cstdint>
#include <initializer_list>
#include <optional>
#include <span>
#include <string_view>
#include <variant>

#include "lyra/backend/llvm/runtime_entry.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/declared_member_storage.hpp"
#include "lyra/support/value_domain.hpp"

namespace lyra::backend::llvm_backend {

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
// making such storage, or a net of any integral value, is told it ahead of
// whatever it takes. A memory reached by a key is told the type its keys are
// built at (LRM 21.4.1), and an associative array built from a literal the
// type of the pairs its entries cross as (LRM 7.9.11), each after the operand
// it concerns.
struct ToldNothing {};
struct ToldWidthMetadata {
  std::size_t after;
};
struct ToldWidthMetadataAhead {};
// Storage laying a value out after itself by the value's type is told that
// type, with how a value of it starts, ahead of whatever it takes where it is
// made, and an entry acting on such storage once it is made is told nothing of
// the values it is handed, which the storage keeps the type of.
struct ToldTypeMetadataAhead {};
// A net of a value laid out by its type is told that type and how a net folds
// values of it, ahead of whatever it takes where it is made, since the net is
// the one holder that folds one value into another (LRM 6.6).
struct ToldTypeAndNetFoldsAhead {};
// A tuple, a union and a fixed-size unpacked array are their bytes laid out by
// their type and nothing more, so a realization for one is told that type,
// after its operands, once however many values of it it takes or builds, as a
// generic function takes its type argument once. That holds where it reaches
// the value through a reference or a designation too, which name bytes lying
// in something else. A value of another type it is handed is read with its
// own.
struct ToldLaidOutType {};
struct ToldKeyType {
  std::size_t after;
};
struct ToldEntryType {
  std::size_t after;
};
// A takeover begun on a value laid out by its type is told, after its
// operands, the type's own function taking some of its positions from another
// value of it (LRM 10.6.2), which the takeover keeps while it shows anything.
struct ToldTakePositions {};
// A realization comparing or streaming a whole value of a container the library
// holds, or of a fixed-size unpacked array, is told, after its operands, the
// operations of the type it is realized for, which name its element's (LRM
// 11.4.5, 20.6.2, 20.9, 6.24.3): no container keeps them.
struct ToldOperations {};
using Told = std::variant<
    ToldNothing, ToldWidthMetadata, ToldWidthMetadataAhead,
    ToldTypeMetadataAhead, ToldTypeAndNetFoldsAhead, ToldLaidOutType,
    ToldKeyType, ToldEntryType, ToldTakePositions, ToldOperations>;

// The domains the library realizes an entry for that are told the same thing.
// An entry states one of these per thing its realizations are told, and a
// domain in none of them is one the library holds no realization for. One
// that states nothing is told what every realization for its domain is.
struct RealizedFor {
  DomainSet domains;
  std::optional<Told> told = std::nullopt;
};

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
  std::optional<Told> told = std::nullopt;
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

// What the realization of an entry for `domain` is told. A representation the
// library holds no realization of the entry for is a compiler defect, refused
// here and where its symbol is composed alike.
auto ToldOf(support::ValueDomain domain, RuntimeOp op) -> Told;
auto ToldOf(support::ValueDomain domain, lir::ValueCellTarget::Op op) -> Told;
auto ToldOf(support::ValueDomain domain, lir::OpenWriteTarget::Op op) -> Told;
auto ToldOf(support::ValueDomain domain, support::BuiltinFn fn) -> Told;
auto ToldOf(
    support::ValueDomain domain, WrapperKind wrapper, support::BuiltinFn fn)
    -> Told;
// What the realization of an operator for `domain` is told, which is the same
// for every operator. No table states which domains realize an operator, so
// none is refused here.
auto ToldOfAnOperator(support::ValueDomain domain) -> Told;
// What the entry building a member's storage is told of the values held there,
// ahead of the storage it is built in, as a cell the library makes is. Storage
// laying the words of a value wider than a word out after itself is built at
// their width, and storage laying a value out after itself by the value's
// type with that type, so the value has its place from the moment the
// storage exists; and a net of any integral type is built at its width, which
// is how many positions it has and which a layout of a narrower type does not
// say.
auto ToldBuilding(support::DeclaredMemberStorage storage) -> Told;

// Whether the library states the type of a value held in `domain`, which a
// container holding one reads; the operations of the language on such a value
// a container type's operations name with its element's; and the operations of
// a kind no other type parameterizes, stated whole.
auto StatesValueType(support::ValueDomain domain) -> bool;
auto StatesOperationFunctions(support::ValueDomain domain) -> bool;
auto StatesValueOperations(support::ValueDomain domain) -> bool;

// The order the library keeps the entries of an associative array indexed by
// values held in `domain` in (LRM 7.8): one for every integral value, or one
// of the domain's own for each other kind the language admits as an index (LRM
// 7.8.1 -- 7.8.4), and nothing for a kind it admits as none.
enum class IndexOrder : std::uint8_t { kIntegral, kOwn };
auto IndexOrderOf(support::ValueDomain domain) -> std::optional<IndexOrder>;

// Whether the library realizes the conversion `fn` names building a value of
// `destination` out of one of `source`.
auto RealizesConversion(
    support::BuiltinFn fn, support::ValueDomain destination,
    support::ValueDomain source) -> bool;

}  // namespace lyra::backend::llvm_backend
