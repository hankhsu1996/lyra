#pragma once

#include <cstddef>
#include <optional>
#include <vector>

#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/code_source.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/language_trait.hpp"
#include "lyra/support/library_trait.hpp"
#include "lyra/support/value_operation.hpp"

namespace lyra::mir {

// The implementation, for the type `self_type`, of the library's traits, as
// rustc's impl of a trait for a type names its items: each trait the type
// implements, and each operation of the type's answering a requirement of
// one. A library declaration lists the traits its types implement, a
// run-time-sized container's value's life among them, its elements being no
// parts a type lays out. A fixed-size array, which no declaration states, has
// as items each operation of its interface some trait requires, since the
// library asks each of a value apart from the rest; its steps follow from its
// parts. Each target's library asks the items it needs, however it is told
// of them, and no target decides which items the type has.
struct Impl {
  TypeId self_type;
  std::vector<const support::LibraryTrait*> traits;
  std::vector<support::ValueOperation> items;
};

// The implementation of the library's traits for `type`, where it has one.
[[nodiscard]] auto ImplOf(const CompilationUnit& unit, TypeId type)
    -> std::optional<Impl>;

// The operation of the type `call` answers, `type`, that `call` is, where it
// is a construction of no arguments of a run-time-sized container type: the
// type's default, as C++'s `T()` runs a class's default constructor. Any other
// construction is built from what it is handed, or the type's parts.
[[nodiscard]] auto ConstructedByTheTypesDefault(
    const TypePool& types, const CallExpr& call, TypeId type)
    -> std::optional<TypeOperationTarget>;

// The operations a value of a type has, as rustc's impl block for a type
// names its items: each operation of a fixed signature, and each family
// of operations taken at a type argument. It names them and nothing more; what
// each takes and answers is its declaration, stated where it is asked, and its
// body is built only where something needs it. Every call of an operation of
// the type names one of them, and a backend represents the interface as its
// own target spells a type's operations.
// A type implementing the library's traits has every operation they
// require among its operations, as a C++ class declares its constructors,
// assignments and destructor, or a Rust type implements `Clone` and `Drop`.
struct TypeInterface {
  TypeId type;
  std::vector<support::ValueOperation> operations;
  std::vector<support::ValueOperation> families;
};

// The interface of `type`, a type no declaration of the source states its
// operations for -- a fixed-size array (LRM 7.4), a tuple, a dynamic array
// (LRM 7.5), an associative array (LRM 7.8), a queue (LRM 7.10) -- or a
// record, whose declaration's methods are each one of it: the operations on a
// whole value its parts decide, as a generic impl of rustc's exists where its
// type arguments meet its bounds, and a container's own. A type whose
// operations are stated otherwise has none here.
[[nodiscard]] auto TypeInterfaceOf(const CompilationUnit& unit, TypeId type)
    -> std::optional<TypeInterface>;

// The allocation function a new element of a dynamic array of `type` is built
// in -- the generation the array is building, told the generation and the
// position -- with how many arguments it takes, the container first; nothing
// for any other type.
struct ElementAllocation {
  support::BuiltinFn allocate;
  std::size_t arguments;
};
[[nodiscard]] auto ElementAllocationOf(const CompilationUnit& unit, TypeId type)
    -> std::optional<ElementAllocation>;

// What a run-time-sized array's write of an element through a position answers
// (LRM 7.10.1): where the element of `element` lies, no address where the
// position names none, and whether the write made it there.
[[nodiscard]] auto FoundElementType(const CompilationUnit& unit, TypeId element)
    -> TypeId;

// Whether `interface` declares `answers`, as an operation or as a family.
[[nodiscard]] auto Declares(
    const TypeInterface& interface, support::ValueOperation answers) -> bool;

// The operation `source` names, declared and not defined, as rustc's fn_sig
// states an item's signature apart from its body: each parameter a reference
// to what it takes, the first the receiver where it is asked of a value, then
// where it was called where it reports there, and its answer. An operation
// takes and answers the same at every type its interface declares it for. A
// body built for `source` is this declaration with its body; an operation its
// type's interface does not declare has none.
[[nodiscard]] auto OperationDeclarationOf(
    const CompilationUnit& unit, const TypeOperationSource& source)
    -> std::optional<CallableCode>;

// The operation of `type`'s interface a call naming the library entry `entry`
// asks: the type's own method or the item of a language trait it implements of
// that name, or the item of a library trait it implements that the entry names,
// as rustc finds a method by its name among the items of the receiver type's
// impls. Nothing where the type declares none, and the library's entry answers
// the call.
[[nodiscard]] auto OperationNamedByEntry(
    const CompilationUnit& unit, TypeId type, support::BuiltinFn entry)
    -> std::optional<support::ValueOperation>;

// Whether `type` implements the language trait `trait`: whether its interface
// declares every requirement, which a construct asks of its operand's type
// before it calls one, as rustc selects an impl for a lang item.
[[nodiscard]] auto ImplementsLanguageTrait(
    const CompilationUnit& unit, TypeId type,
    const support::LanguageTrait& trait) -> bool;

// Which of the kinds of body the operation `answers` at `type` is, and
// nothing where no unit can state it.
[[nodiscard]] auto TypeOperationKindOf(
    const CompilationUnit& unit, TypeId type, support::ValueOperation answers)
    -> std::optional<TypeOperationKind>;

// The type a sort's order is answered at, a dynamic array of longint
// positions (LRM 7.12.2).
[[nodiscard]] auto SortOrderType(const TypePool& types) -> TypeId;

// The type a queue's slice is read as: a queue of `element` declaring no
// bound, whatever the bound of the queue it was taken from (LRM 7.10.1).
[[nodiscard]] auto UnboundedQueueOf(const TypePool& types, TypeId element)
    -> TypeId;

// The type a write of several elements in a row answers what it changed at:
// the position of the first element that took a different value and the one
// past the last, the same position twice where none did (LRM 4.3, 7.6).
[[nodiscard]] auto ChangedRangeType(const TypePool& types) -> TypeId;

// How many values a bit can hold: 0, 1, x and z.
inline constexpr unsigned kBitValueCount = 4;

// The type of the set a count is taken under: a bit holds one of four values,
// each at most once in the set, one per bit position. A set naming fewer
// repeats one it names, which admits nothing twice (LRM 20.9), so every set is
// a value of this one type.
[[nodiscard]] auto BitCountControlType(const TypePool& types) -> TypeId;

}  // namespace lyra::mir
