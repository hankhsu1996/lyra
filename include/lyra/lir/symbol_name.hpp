#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <string_view>

#include "lyra/lir/class_id.hpp"
#include "lyra/lir/closure_id.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/support/class_name.hpp"
#include "lyra/support/def_path.hpp"
#include "lyra/support/runtime_class.hpp"
#include "lyra/support/value_operation.hpp"

namespace lyra::lir {

// Which declaration of the program a symbol names. A category is part of the
// symbol rather than a word joined into it, so two declarations of different
// categories never reach one name however they were spelled -- and a category
// the compiler mints, having no source name at all, is nameable without
// borrowing a spelling the source could also write.
enum class SymbolCategory : std::uint8_t {
  kClassDefinition,
  kClosureDefinition,
  kConstructor,
  kClassCallable,
  kMethodEntry,
  kRecordMethod,
  kNamespaceCallable,
  kNamespaceStorageInstall,
  kNamespaceStorageInitialize,
  kObjectEntry,
  kNamespaceVariable,
  kStaticProperty,
  kClosureInvoke,
  kTypeOperation,
  kBaseObjectCreation,
  kAllocatingCreation,
  kBaseObjectDestructor,
  kCompleteObjectDestructor,
  kDeletingDestructor,
  kClassConstant,
  kDispatchTable,
};

// One component of a symbol: a name the source wrote, an ordinal the compiler
// counted, a callback of the runtime library a class answers, a type spelled
// by its structure, or the steps of the path that tells a declaration from the
// others of its unit.
// Each self-delimits and says which kind it is, so a sequence of them composes
// to one string no other sequence composes to: a block and a class carrying one
// label are different parts, and a method the source named after a library
// callback is not the body answering it.
struct SymbolPart {
  static auto Name(std::string_view name) -> SymbolPart;
  static auto Ordinal(std::uint32_t value) -> SymbolPart;
  static auto LibraryCallback(support::LibraryCallback function) -> SymbolPart;

  std::string encoded;
};

// The part a declaration contributes to a symbol: the identifier the source
// wrote, or the position the declaration sits at where the source wrote none.
// A part says which kind it is, so the two ranges never meet and a declaration
// the source named can never compose the symbol of one it did not.
auto SymbolPartOf(std::optional<std::string_view> name, std::uint32_t ordinal)
    -> SymbolPart;

// What a declaration another unit names contributes: one part per step of its
// path, in order, each opening with the marker of the kind of scope it is. A
// step is told from every other kind of part by its marker, so the steps end
// where a part of another kind begins or the symbol does, and the class an
// instance of its unit is, which has no step, contributes nothing.
auto SymbolPartOf(const support::DefPath& path) -> SymbolPart;

// The same for a class, which has a path where another unit names it and its
// place among the classes its unit built for its own use where none does.
auto SymbolPartOf(const support::ClassName& name) -> SymbolPart;

// The symbol a declaration is linked under, program-wide.
//
// A SystemVerilog identifier admits every printable character but white space
// (LRM 5.6.1), so no character of it is free to separate one name from the
// next and no word of it is free to mean something the source did not write.
// Each part therefore carries its own extent, and what the compiler mints is a
// category rather than a spelling. The unit that emits a declaration and every
// unit that reaches it build the same parts from the names each already holds,
// so the two agree with no table between them.
auto SymbolName(
    SymbolCategory category, std::initializer_list<SymbolPart> parts)
    -> std::string;

// The symbols of what belongs to a class. A class's own part is unique only
// inside its unit while the whole program links into one name space, so the
// unit qualifies it; a member's name is unique only inside its class, so the
// class qualifies that. Every part is a name where the source declared one and
// a position where it did not -- a scope of the design hierarchy is a class the
// lowering built, and a body the lowering synthesized is reached by no call
// site that could spell it.
auto ConstructorSymbol(std::string_view unit_name, SymbolPart cls)
    -> std::string;
auto ClassDefinitionSymbol(std::string_view unit_name, SymbolPart cls)
    -> std::string;
auto ClassCallableSymbol(
    std::string_view unit_name, SymbolPart cls, SymbolPart callable)
    -> std::string;
// The entry of a class's method, through which an object whose class the caller
// does not know is entered: named by the method it enters, as every unit that
// names it names the method.
auto MethodEntrySymbol(
    std::string_view unit_name, SymbolPart cls, SymbolPart method)
    -> std::string;
// A record's method (LRM 7.2, 7.3), under the record the unit declares. The
// operation it answers is its name, since a record has one method for each.
auto RecordMethodSymbol(
    std::string_view unit_name, const support::DefPath& record,
    support::ValueOperation operation) -> std::string;
// A class's cell, under the class that owns it. The pool holding a class's
// cells also takes what its bodies keep for the whole class, which the source
// never declared.
auto StaticPropertySymbol(
    std::string_view unit_name, SymbolPart cls, SymbolPart property)
    -> std::string;

// The symbols of what a unit's namespace owns directly. A body the source
// declared answers to the identifier it declared it under, which is what
// another unit reaches it by; a body the compiler synthesized into the
// namespace answers to none and takes its position instead, exactly as a
// class's own bodies and as this namespace's storage already do.
auto NamespaceCallableSymbol(std::string_view unit_name, SymbolPart callable)
    -> std::string;

// The symbols of the bodies a unit publishes that the source declares none of:
// the two its namespace is brought up through, and for each body it is
// compiled to the one that makes an object of that body. None is composed from
// a name, because none has one; a category over the unit, and the body where
// there is one per body, is what lets the unit that defines one and whoever
// calls it arrive at the same symbol with nothing shared between them.
auto NamespaceStorageInstallSymbol(std::string_view unit_name) -> std::string;
auto NamespaceStorageInitializeSymbol(std::string_view unit_name)
    -> std::string;
// The entry making an object of a body, or a block of the block body standing
// at `block` among the body's.
auto ObjectEntrySymbol(
    std::string_view unit_name, std::string_view body_name,
    std::optional<std::uint32_t> block) -> std::string;
// A unit's own storage. A package variable answers to the identifier the source
// declared, which is what another unit reaches it by; the cell a subroutine's
// static-lifetime local keeps answers to none, and takes its position instead.
auto NamespaceVariableSymbol(std::string_view unit_name, SymbolPart variable)
    -> std::string;

// A closure is counted rather than named, having no declaration of the source
// to take a name from, so its body's symbol is composed from its ordinal.
auto ClosureInvokeSymbol(std::string_view unit_name, std::uint32_t ordinal)
    -> std::string;

// What a type contributes to a symbol: what it is -- its kind, what fixes it,
// and the types it is made of, each in turn -- and a declaration it names by
// the symbol that declaration is defined under. Two types compose the same part
// exactly where they are the same type, so it is the type's name however the
// unit came to name it.
auto SymbolPartOf(const CompilationUnit& unit, TypeId type) -> SymbolPart;

// A function answering an operation at a type with no declaration is named by
// the type and the operation, as rustc names drop glue by the type it drops.
auto TypeOperationSymbol(
    std::string_view unit_name, SymbolPart type,
    support::ValueOperation operation) -> std::string;

// The name the class `name` of `unit` is qualified by: the unit's own for a
// class another unit names, which every body of the unit states alike; the
// body's for a class built for one body's own use, which no other body states
// and whose position is its only identity.
auto QualifierOf(const CompilationUnit& unit, const support::ClassName& name)
    -> std::string_view;

// The symbol `type`'s definition is linked under, or nothing where the type
// names no declaration a value is built from. The definition is the compiler's
// own and stands beside the declaration rather than inside it, so it is a
// category over the declaration's parts. A declaration of this unit and one
// another unit declares are composed from the same parts the same way, which is
// what lets every unit naming a class reach the constant its own unit emits
// with no shared table.
auto DefinitionSymbol(const CompilationUnit& unit, TypeId type)
    -> std::optional<std::string>;

// The same for a declaration of this unit, which always has one.
auto DefinitionSymbol(const CompilationUnit& unit, ClassId id) -> std::string;
auto DefinitionSymbol(const CompilationUnit& unit, ClosureId id) -> std::string;

// The bodies that create a value of a declaration, running none of the
// design's code: the base object creation brings into existence, in storage
// it is handed, the part the declaration declares and every part it extends;
// the allocating creation takes the storage, does that, and then gives the
// value its class, once -- the Itanium ABI's base object and complete object
// allocating constructors. A value is created by its own class's allocating
// creation before any constructor runs on it, and a declaration extending one
// of another unit names its base's base object creation this way.
enum class Creation : std::uint8_t { kBaseObject, kAllocating };

auto CreationSymbol(std::string_view definition, Creation which) -> std::string;

// The ends a C++ compiler gives a destructor: the base object destructor ends
// what the declaration declares and then what it extends; the complete object
// destructor ends a whole value; the deleting destructor also gives the
// value's storage back, which is how a class's definition says its objects
// end. A declaration extending one of another unit names its base's base
// object destructor this way.
enum class Destructor : std::uint8_t {
  kBaseObject,
  kCompleteObject,
  kDeleting
};

auto DestructorSymbol(std::string_view definition, Destructor which)
    -> std::string;

// The symbol of one constant a declaration holds, by the position it sits at
// among them.
auto ClassConstantSymbol(std::string_view definition, std::uint32_t ordinal)
    -> std::string;

// The symbols of the table a value of a class dispatches through and of the
// one it gives an interface class it implements, composed over the symbols
// the definitions are linked under.
auto DispatchTableSymbol(std::string_view definition) -> std::string;
auto InterfaceTableSymbol(
    std::string_view definition, std::string_view iface_definition)
    -> std::string;

}  // namespace lyra::lir
