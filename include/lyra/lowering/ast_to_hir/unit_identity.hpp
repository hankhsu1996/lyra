#pragma once

// How what a unit declares is identified and what it is called. A declaration
// is identified by the unit holding it and its path there; a specialization of
// a generic class by its generic and what its parameters were bound to (LRM
// 8.25); a name is derived from an identity where a bounded identifier is
// needed. The two are separate on purpose -- an identity has to distinguish and
// may drop nothing, a name has to fit in an identifier -- so nothing here
// treats one as the other.
//
// Both are computed from the frontend and from a naming every party in one
// stage reads the same, because the unit naming itself and every unit naming
// it must reach the same answer with no shared table.

#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include "lyra/support/def_path.hpp"

namespace slang {
class ConstantValue;
}  // namespace slang

namespace slang::ast {
class ClassType;
class CovergroupType;
class DefinitionSymbol;
class GenerateBlockSymbol;
class InstanceBodySymbol;
class InstanceSymbol;
class ParameterSymbol;
class Scope;
class SubroutineSymbol;
class Symbol;
class Type;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// The instantiation a body was elaborated for. A body is what one application
// of a definition produced and states no bindings apart from that application,
// so it belongs to exactly one and is never asked what it is a specialization
// of on its own.
[[nodiscard]] auto InstantiationOf(const slang::ast::InstanceBodySymbol& body)
    -> const slang::ast::InstanceSymbol&;

// The instance `scope` is the body of or stands in: an instance's body, or a
// scope declared somewhere inside one.
[[nodiscard]] auto InstanceHolding(const slang::ast::Scope& scope)
    -> const slang::ast::InstanceSymbol&;

// The digest a name carries of `bytes`: the first 64 bits of their stable
// fingerprint, so producer and consumer agree on the name across separately
// compiled units and across sessions. Whoever names by it holds the name
// against what it was folded from, which is where two folding to one digest is
// caught.
[[nodiscard]] auto NameDigest(std::string_view bytes) -> std::uint64_t;

// A value as an identity: every bit of it, unknowns included, so two values
// that differ anywhere never answer alike. The front end's own rendering is
// for reading -- it shortens a value wider than it cares to print and folds
// unknowns -- so two different values can render alike through it.
[[nodiscard]] auto ValueIdentity(const slang::ConstantValue& value)
    -> std::string;

// What one parameter of a class specialization was bound to (LRM 8.25): a
// value or a type, the two a parameter takes (LRM 6.20.2, 6.20.3). Each holds
// the identity of what it was bound to -- a constant's value, a data type's
// identity -- because that is what decides whether two specializations are
// one type.
struct BoundValue {
  std::string value;

  auto operator==(const BoundValue&) const -> bool = default;
};

struct BoundType {
  std::string type;

  auto operator==(const BoundType&) const -> bool = default;
};

using ClassKeyInputKind = std::variant<BoundValue, BoundType>;

// One parameter of a class specialization: its name, and what it was bound to.
struct ClassKeyInput {
  std::string name;
  ClassKeyInputKind kind;

  auto operator==(const ClassKeyInput&) const -> bool = default;
};

// What a class specialization is: the class it specializes, and what each of
// its parameters was bound to. Equality is structural, so two keys agree or
// differ on their parts and never on a rendering of them.
struct ClassKey {
  std::string definition;
  std::vector<ClassKeyInput> inputs;

  auto operator==(const ClassKey&) const -> bool = default;
};

// Which design element `inst` is an instance of. A name alone does not say: a
// library cell is found by its library and its name (LRM 33.2.1), and a module
// declared inside another by the definition declaring it and its name (LRM
// 23.4).
[[nodiscard]] auto DefinitionNameOf(const slang::ast::InstanceSymbol& inst)
    -> std::string;

// How one stage of the compile names what it names: the instance a reference
// reaches, and a generate block on the way to a declaration. Declaring a unit
// and lowering its bodies state another instance's class and a block's class
// in the stage's own terms -- what it reaches while classes are found, and the
// class found for it once they have been -- and every other answer is the
// instances' own, the same in every stage.
//
// Every party in one stage reads the same naming, so a parent and the child it
// names agree.
class InstanceNames {
 public:
  InstanceNames() = default;
  InstanceNames(const InstanceNames&) = delete;
  auto operator=(const InstanceNames&) -> InstanceNames& = delete;
  InstanceNames(InstanceNames&&) = delete;
  auto operator=(InstanceNames&&) -> InstanceNames& = delete;
  virtual ~InstanceNames() = default;

  // What `inst` is named by in this stage.
  [[nodiscard]] virtual auto NameOf(
      const slang::ast::InstanceSymbol& inst) const -> std::string = 0;

  // What a type naming the interface `iface` (LRM 25.9) names it by in this
  // stage. Such a type names a unit and holds no instance: the front end stands
  // one instance in for every type naming an interface with the same
  // parameters, and places it under whichever declaration it met first, so
  // where that instance stands says nothing of the type.
  [[nodiscard]] virtual auto InterfaceNameOf(
      const slang::ast::InstanceSymbol& iface) const -> std::string = 0;

  // The step the block instance `block` is in the scope holding it: its label,
  // which one it is among the blocks of that scope sharing the label (LRM
  // 27.5), and what tells it from the other block instances of its construct in
  // this stage.
  [[nodiscard]] virtual auto BlockStepOf(
      const slang::ast::GenerateBlockSymbol& block) const
      -> support::GenerateBlockStep = 0;
};

// The step `block` is in the scope holding it, by where it stands in its
// instance: its label, which one it is among the blocks sharing the label (LRM
// 27.5), and in a loop the value its index stood at (LRM 27.4).
[[nodiscard]] auto PathStepOf(const slang::ast::GenerateBlockSymbol& block)
    -> support::GenerateBlockStep;

// The naming before any unit is found: an instance by its hierarchical path
// (LRM 23.6), and a block by `PathStepOf`. Nothing compiled carries a name of
// this naming; it tells apart what is found before the units are.
class PathNames final : public InstanceNames {
 public:
  [[nodiscard]] auto NameOf(const slang::ast::InstanceSymbol& inst) const
      -> std::string override;
  [[nodiscard]] auto InterfaceNameOf(
      const slang::ast::InstanceSymbol& iface) const -> std::string override;
  [[nodiscard]] auto BlockStepOf(const slang::ast::GenerateBlockSymbol& block)
      const -> support::GenerateBlockStep override;
};

// The key of a SystemVerilog class specialization (LRM 8.25). Two
// specializations of one generic class denote the same type iff every value
// binding is equal and every type binding is a matching type (LRM 8.25
// uniqueness rule); slang deduplicates on that rule, so distinct bindings
// arrive as distinct ClassType instances and key apart. Bare `C` and
// empty-override `C #()` resolve to the same slang ClassType and key alike.
auto ClassKeyOf(const slang::ast::ClassType& cls, const InstanceNames& names)
    -> ClassKey;

// The digest of everything a key says was fixed, or nothing for a key under
// which nothing was. It is computed by folding only bytes, so the producer (the
// unit naming itself) and every consumer (a parent naming a child) reach the
// same answer across separate compilations with no shared table.
auto ArgumentsDigest(const ClassKey& key) -> std::optional<std::uint64_t>;

// The name a key is known by. The definition's name when nothing was fixed, and
// otherwise that name plus the digest of what was -- bounded, so it serves as
// an identifier.
auto UnitName(const ClassKey& key) -> std::string;

// The name the class specialization `cls` is, for a caller that wants the name
// and not the key it comes from.
auto UnitName(const slang::ast::ClassType& cls, const InstanceNames& names)
    -> std::string;

// What a virtual interface type naming `iface` names (LRM 25.9): the
// interface's definition and the value or type each of its parameters was
// fixed to. Nothing else shapes an instance a virtual interface may hold, since
// the front end refuses one whose body reaches upward or takes an interface
// port.
[[nodiscard]] auto InterfaceIdentity(
    const slang::ast::InstanceSymbol& iface, const InstanceNames& names)
    -> std::string;

// The port or the instance a module's definition of an interface's subroutine
// is written through (LRM 25.7), the `b` of `b.Get`, or nothing for any other
// subroutine.
auto ExternDefinitionPrefix(const slang::ast::SubroutineSymbol& subroutine)
    -> std::optional<std::string_view>;

// The name a subroutine is declared under in the scope holding it: its own,
// or, for one a module defines for an interface it reaches (LRM 25.7), both
// parts of the name it was written as, with a space between them, which no
// identifier holds (LRM 5.6.1). A scope may define one interface subroutine
// through two ports, declare one of that name itself and one whose escaped name
// spells `b.Get`, and each is a subroutine of the scope apart from the others.
auto SubroutineNameOf(const slang::ast::SubroutineSymbol& subroutine)
    -> std::string;

// The step a subroutine or a block of statements is in the scope holding it:
// under its name (LRM 13, 9.3.4), or for a block the source gave no label by
// where it sits. The unit publishing what such a scope holds and a unit
// reaching into it by a hierarchical name (LRM 23.9) both ask here.
auto ProceduralStepOf(const slang::ast::Symbol& scope) -> support::DefPathData;

// Which declaration of its compilation unit `declaration` is: one step for each
// scope the source nests it in, from the unit's own scope down, and one for the
// declaration itself, which is a scope too -- a generate block as the
// application of its definition it is (LRM 27.3), a class under its name and
// what its parameters were bound to (LRM 8.3, 8.25), a subroutine or a labelled
// block under its name (LRM 13, 9.3.4), an enumeration, a structure or a union
// under the name it answers to (LRM 6.22.1), and a block or type with no name
// by where it sits. The unit's own scope is the path of no steps, so the block
// instances of one application are one declaration, as the instances of one
// unit are.
//
// The source alone decides the answer, so the unit declaring something and
// every unit naming it compute the same path here, and a type SystemVerilog
// identifies by its declaration (LRM 8.3, 6.22.1) is identified from anywhere
// by the name of the unit holding it and this path.
auto DefPathOf(
    const slang::ast::Symbol& declaration, const InstanceNames& names)
    -> support::DefPath;

// The name a covergroup goes by (LRM 19.3): the one it was declared with, or
// for one a class embeds, which declares an anonymous type and a property of it
// (LRM 19.4), the property's.
auto CovergroupNameOf(const slang::ast::CovergroupType& covergroup)
    -> std::string_view;

// The package (LRM 26.2) or `$unit` scope (LRM 3.12.1) `decl` is declared in,
// or null where it is declared in any other scope. Neither has an instance, so
// what one declares is reached by the unit's name rather than routed to through
// an object. An anonymous program declares its items in the package or `$unit`
// scope around it without opening a scope of its own (LRM 24.6), so one of its
// items is declared in that unit.
[[nodiscard]] auto DeclaringNamespaceUnit(const slang::ast::Symbol& decl)
    -> const slang::ast::Symbol*;

// The symbol whose compilation unit owns `decl`, found by climbing its parent
// scopes to the first that is one: a package (LRM 26), a design element's body
// (LRM 23.2, 25), or the `$unit` file-set scope a declaration outside every
// design element belongs to (LRM 3.12.1). A declaration nested deeper -- in a
// class, a generate block, a subroutine -- still hits one of those first, which
// is the ownership boundary. Every declaration lies in some compilation unit,
// so one is always found, and a chain that ends without one is a declaration
// the frontend placed nowhere.
auto DeclaringCompilationUnit(const slang::ast::Symbol& decl)
    -> const slang::ast::Symbol&;

// Whether `decl` is written in a text of Lyra's own -- the built-in classes
// the language supplies (LRM 9.7, 15.3, 15.4), the coverage library -- rather
// than in the design. A body of such a text reports a failure of the design
// at the call that reached it, since its own lines mean nothing to whoever
// wrote the call.
[[nodiscard]] auto DeclaredInLibraryText(const slang::ast::Symbol& decl)
    -> bool;

// The structural scope whose instances each have their own copy of `decl`, a
// declaration or a type (LRM 6.22): a type declared inside an instance is a
// type of that instance. A specialization of a generic class is a type of its
// own for each set of parameters (LRM 8.25), so one specialized on a type of an
// instance is a type of that instance too, wherever the generic was declared.
// So the answer is the innermost instance body or generate block among the one
// enclosing `decl` and those replicating each type argument of a specialization
// `decl` is or lies in; where none is, the package or `$unit` scope enclosing
// `decl`, which no instance replicates. A class nested inside another class is
// a type of the same instance the outer one is, since SystemVerilog gives it no
// reach into the outer object.
[[nodiscard]] auto ReplicatingScope(const slang::ast::Symbol& decl)
    -> const slang::ast::Scope&;

// Whether `scope` is a scope of the design hierarchy: a design element's body
// or a generate block, each of which every instance has its own of.
[[nodiscard]] auto IsInstanceScope(const slang::ast::Scope& scope) -> bool;

// The symbol whose compilation unit holds `decl`: the one whose own source
// fixes everything `decl` is, so no unit's content follows from how another
// unit uses its declarations. A declaration an instance replicates belongs to
// the design element's body that scope lies in. Otherwise, one that is or lies
// in a specialization of a generic class belongs to that specialization, which
// is a unit of its own, since its parameters are written wherever it is named
// and the unit declaring the generic fixes none of them (LRM 8.25). Every other
// declaration belongs to its declaring compilation unit.
[[nodiscard]] auto UnitHomeOf(const slang::ast::Symbol& decl)
    -> const slang::ast::Symbol&;

// Whether `unit` is a design element (LRM 23.2.1) rather than a namespace one.
// A design element is instantiated into the hierarchy, so a type it declares
// inside is a type of each instance rather than one type of the unit (LRM
// 6.22), and what it publishes is the object each instance is. A package and
// the file-set scope are named once and declare once.
[[nodiscard]] auto IsDesignElement(const slang::ast::Symbol& unit) -> bool;

// Whether `type`, a class or a covergroup, belongs to an instance (LRM 6.22):
// an instance replicates it, so each instance of that scope has a type of its
// own. One a package or the `$unit` scope declares belongs to none, whichever
// unit reaches it, unless it is a specialization on a type that does.
[[nodiscard]] auto BelongsToAnInstance(const slang::ast::Symbol& type) -> bool;

// The name a compilation unit publishes for itself, so a consumer reaching one
// of its declarations and the unit emitting that declaration agree with no
// shared table (LRM 26.3). A package publishes its declared name; a module body
// its unit name. An anonymous compilation-unit scope (the LRM 3.12.1 `$unit`
// file-set scope, modeled as a namespace unit with no source name) publishes a
// name derived from its own source-input identity: the only property
// distinguishing two such scopes is which compilation-unit input they belong
// to, which the LRM uses to define the scope boundary itself. A specialization
// that is a unit of its own publishes the name of the unit declaring its
// generic joined to its own the way a unit's name joins its definition to the
// rest, so the name is an identifier as a module's is. Both the producer and
// every consumer compute this from the same slang unit symbol.
auto CompilationUnitName(
    const slang::ast::Symbol& unit, const InstanceNames& names) -> std::string;

}  // namespace lyra::lowering::ast_to_hir
