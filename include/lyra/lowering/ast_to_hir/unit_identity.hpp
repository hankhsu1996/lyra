#pragma once

// How a compilation unit is identified and what it is called. A unit's identity
// is its definition together with everything the design fixed that changes what
// gets compiled; a name is derived from that identity where a bounded
// identifier is needed. The two are separate on purpose -- an identity has to
// distinguish and may drop nothing, a name has to fit in an identifier -- so
// nothing here treats one as the other.
//
// Both are computed from the frontend and from a policy every party reads the
// same, because the unit naming itself and every unit naming it must reach the
// same answer with no shared table.

#include <cstddef>
#include <cstdint>
#include <map>
#include <optional>
#include <span>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/lowering/ast_to_hir/instance_context.hpp"
#include "lyra/support/bodies_read.hpp"
#include "lyra/support/def_path.hpp"

namespace slang {
class ConstantValue;
}  // namespace slang

namespace slang::ast {
class ClassType;
class DefinitionSymbol;
class GenerateBlockSymbol;
class InstanceBodySymbol;
class InstanceSymbol;
class ParameterSymbol;
class Scope;
class Symbol;
class Type;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// Where a value parameter's value comes from, for the objects built from one
// compiled scope. Fixed by the specialization: it is part of what is compiled,
// so every object of the scope holds the one value. Supplied at construction:
// whoever builds the object passes it -- an instance's overridden parameter
// (LRM 23.10.2), or the index a loop's block was built at (LRM 27.4). Computed
// at construction: its own declaration writes it from a supplied value,
// directly or through another such, so each object computes it when built.
enum class ParameterValueSource {
  kFixedBySpecialization,
  kSuppliedAtConstruction,
  kComputedAtConstruction,
};

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

// A value as an identity: every bit of it, unknowns included, so two values
// that differ anywhere never answer alike. The front end's own rendering is
// for reading -- it shortens a value wider than it cares to print and folds
// unknowns -- so two different values can render alike through it.
[[nodiscard]] auto ValueIdentity(const slang::ConstantValue& value)
    -> std::string;

// What one input to a specialization was fixed to. A value and a type are the
// two a parameter takes (LRM 6.20.2, 6.20.3); an interface is what an interface
// port carries (LRM 25.3), which no parameter declares and the language gives
// no way to write, so the connection is where it is fixed. Each holds the
// identity of what it was fixed to -- a constant's value, a data type's
// identity, a unit's name -- because that is what decides whether two
// instantiations compile alike.
struct FixedValue {
  std::string value;

  auto operator==(const FixedValue&) const -> bool = default;
};

struct FixedType {
  std::string type;

  auto operator==(const FixedType&) const -> bool = default;
};

// The interface instances an interface port is bound to, as the units they
// are: the distinct ones, in the order the positions first meet them, and
// which of them each position takes, in row-major order (LRM 25.3). A port
// standing for one instance is the set of one.
struct FixedInterface {
  std::vector<std::string> units;
  std::vector<std::uint32_t> taken;
  // The modport the port is restricted to (LRM 25.5), which narrows the members
  // it reaches and their directions. Empty when the port reaches the whole
  // interface, which the LRM spells as the absence of one.
  std::string modport;

  auto operator==(const FixedInterface&) const -> bool = default;
};

// A parameter the parent overrode with a value the instance is handed when it
// is built. The value is not part of what is compiled, but that the parent
// supplies one is: an instance that is handed a value and one that works its
// default out from the declaration are built by different constructions.
struct SuppliedAtConstruction {
  auto operator==(const SuppliedAtConstruction&) const -> bool = default;
};

// The scope a hierarchical name lands in once it leaves the instance, named
// the way the class of that scope is: by the unit holding it and the class's
// path there. The name resolves per instance (LRM 23.8), and what it reaches
// from there are that scope's declarations, so two instances whose names land
// in different scopes compile differently.
struct LandsIn {
  std::string unit;
  support::DefPath scope;

  auto operator==(const LandsIn&) const -> bool = default;
};

// The same, where the name lands in an instance whose own name is still being
// worked out -- the instance writing it, or one whose name asked for this
// one's. `levels` counts how far out on that chain it is, the instance itself
// being none, and `blocks` names the generate blocks below it the name lands
// in, each as the application of its definition it is. Stated so, the scope is
// named by where it stands relative to the instance being named, which is all
// two instances could differ in there.
struct LandsBack {
  std::uint32_t levels = 0;
  support::DefPath blocks;

  auto operator==(const LandsBack&) const -> bool = default;
};

// An instantiation a bind directive inserted into the instance (LRM 23.11),
// named by the directive. The bound instantiation's connections are text of
// the directive, read from the target's point of view, so two targets bound by
// one directive build alike and two directives may connect one name apart.
struct BindInstantiation {
  std::string directive;

  auto operator==(const BindInstantiation&) const -> bool = default;
};

// The cell a configuration bound the instance to (LRM 33.4), as its library
// and its name. Under a configuration the same instantiation is bound to
// different cells depending on where its instance stands, so the text holding
// it does not say which.
struct BoundToCell {
  std::string cell;

  auto operator==(const BoundToCell&) const -> bool = default;
};

using SpecializationInputKind = std::variant<
    FixedValue, FixedType, FixedInterface, SuppliedAtConstruction, LandsIn,
    LandsBack, BindInstantiation, BoundToCell>;

// One thing the design fixed for an instance: what it named, and what it fixed
// that to. Every input is named -- a parameter by its own name, an interface
// port by the port's, what reaches an instance below by that instance's path
// from this one -- so the name sits here and the arms carry only what differs
// between them.
struct SpecializationInput {
  std::string name;
  SpecializationInputKind kind;

  auto operator==(const SpecializationInput&) const -> bool = default;
};

// Which compiled artifact an instance belongs to: the definition it is built
// from, and everything the design fixed that changes what gets compiled. Two
// instances with equal keys compile alike and share one artifact; instance
// count never affects how many keys exist. Equality is structural, so two keys
// agree or differ on their parts and never on a rendering of them.
struct SpecializationKey {
  std::string definition;
  std::vector<SpecializationInput> inputs;

  auto operator==(const SpecializationKey&) const -> bool = default;
};

// Which value parameters of each instance reach its unit when the instance is
// built rather than as part of what is compiled. A parameter's value is fixed
// before the run (LRM 23.10), and nothing requires the compiled unit to hold
// it: one whose value decides what is compiled -- a type, which blocks exist,
// which child is built -- makes two values two units; one only ever read as a
// value makes every value one unit, and the instance is handed it when it is
// built.
//
// A definition kept whole supplies nothing, and every index of its loops
// decides by its own value: an earlier lowering found two of its instances
// handed different values lowered apart, or two of its block instances taken
// for one application publishing different classes. Every party naming a unit
// reads the same policy, so a parent and the child it names still agree.
//
// The answer is worked out from the source once per instance and kept, which
// is why the lookup is const and the table is not: naming asks it many times
// per instance. Working out one instance's answer asks for its children's,
// since whether what a child is handed is only read as a value is the child's
// answer.
class SpecializationPolicy {
 public:
  explicit SpecializationPolicy(
      std::unordered_set<const slang::ast::DefinitionSymbol*> kept_whole = {},
      support::BodiesRead bodies_read = support::BodiesRead::kElaborated)
      : kept_whole_(std::move(kept_whole)), bodies_read_(bodies_read) {
  }

  // Whether `inst` is read through a body of its own: one the front end
  // elaborated for it, or one this compile decided to read.
  [[nodiscard]] auto HasBodyOfItsOwn(
      const slang::ast::InstanceSymbol& inst) const -> bool;

  // Has `inst` read through its own body from here on, which makes the front
  // end elaborate it. Whatever was worked out of it from where it is written
  // is dropped, so a name asked for afterwards is the one its body gives it.
  void ReadBodyOf(const slang::ast::InstanceSymbol& inst) const;

  // The parameters `inst` is handed when it is built, in the order its body
  // declares them -- which is the order a construction supplies their values
  // in, for the parent writing the construction and the unit reading it alike.
  [[nodiscard]] auto SuppliedParametersOf(
      const slang::ast::InstanceSymbol& inst) const
      -> std::span<const slang::ast::ParameterSymbol* const>;

  // Where the value of `param`, declared anywhere in `inst`'s body, comes from
  // for the objects built from the compiled scope declaring it. A generate
  // block is built once per object of its scope, so every parameter it declares
  // differs between them: its index is supplied, and each other one computed.
  [[nodiscard]] auto ValueSourceOf(
      const slang::ast::InstanceSymbol& inst,
      const slang::ast::ParameterSymbol& param) const -> ParameterValueSource;

  // What tells two values of `index` apart in what is compiled, for the index
  // a loop's block inside `inst` declares (LRM 27.4): the constants the front
  // end settled from it wherever its value decides something, in the order the
  // body states them, or its own value where one of those places kept none.
  // Nothing where it is only ever read as a value. So two blocks whose indices
  // fold alike everywhere are told apart by nothing here.
  [[nodiscard]] auto WhatItDecides(
      const slang::ast::InstanceSymbol& inst,
      const slang::ast::ParameterSymbol& index) const
      -> std::span<const std::string>;

  // What follows for `inst` from where it stands, worked out once and kept.
  [[nodiscard]] auto ContextOf(const slang::ast::InstanceSymbol& inst) const
      -> const InstanceContext&;

  // The step the block instance `block` is in the scope holding it: its label,
  // which one it is among the blocks of that scope sharing the label (LRM
  // 27.5), and which application of its definition it is. A generate block is
  // a definition nested in the scope holding it, and each block
  // instance built from it applies it to arguments, the way an instance applies
  // a design element (LRM 27.3): the index a loop built it at (LRM 27.4), and
  // what is written elsewhere about an instance it holds
  // (LRM 23.10.1, 23.11, 33.4). An index enters only through what its value
  // decides about what is compiled; one only read as a value is handed over
  // when the block is built, so the blocks of a loop that differ in nothing
  // else are one application. A block declaring a class is the exception,
  // entering with the index itself: a class's bodies are compiled against the
  // scope its block lowered to, and which blocks lower to one scope is not
  // known where a name is.
  //
  // The definition is named by the label the source gave it, and what was
  // fixed is stated beside the label as a digest, absent where nothing was.
  [[nodiscard]] auto BlockStepOf(const slang::ast::GenerateBlockSymbol& block)
      const -> support::GenerateBlockStep;

  // The name of the specialization `inst` is an application of, folded from
  // what the design fixed for it: its parameters (LRM 6.20, 23.10), the
  // interface each of its interface ports is connected to (LRM 25.3), the scope
  // each name written in it or below it lands in once it leaves the instance
  // (LRM 23.8), and everything written elsewhere that reaches an instance below
  // it -- a parameter a defparam or a configuration sets (LRM 23.10.1, 33.4.3),
  // an instantiation a bind inserts (LRM 23.11), a cell a configuration binds
  // (LRM 33.4.1.6) -- each under its path from `inst`. A parameter fixed by the
  // specialization enters with its value; one supplied at construction enters
  // only as being supplied, and one computed at construction not at all, so
  // instances supplied different values are one unit.
  //
  // Every part is read off `inst` and what elaborated below it, which is where
  // the parent naming a child already stands. What the frontend chose to
  // elaborate once serves a question about its own work and settles nothing
  // here.
  //
  // The name is worked out once and kept. What is fixed for an instance states
  // everything below it, and every scope the instance holds and every unit
  // naming it asks, so working it out per asking costs the instance each time.
  // A name asked while another is being worked out can differ from the one the
  // instance has alone, where a name it writes lands in an instance still being
  // named. So a name is kept only when nothing it met lay outside itself,
  // together with the instances it looked for among those being named and did
  // not find, and it answers a later asking only while none of those is being
  // named. An asking made on its own passes both, so no answer depends on who
  // asked first.
  [[nodiscard]] auto NameOf(const slang::ast::InstanceSymbol& inst) const
      -> std::string;

  // Naming one instance can ask for the name of another, where a name it writes
  // lands there, and that one may land back in the first: a path from the top
  // can name any instance (LRM 23.6), the one writing it included. These record
  // the chain of instances whose names are being worked out, so a name landing
  // on one of them is told by how far out on the chain it is rather than by a
  // name nobody has yet.
  void EnterNaming(const slang::ast::InstanceBodySymbol& body) const;
  void LeaveNaming() const;
  [[nodiscard]] auto LevelsOutTo(const slang::ast::InstanceBodySymbol& body)
      const -> std::optional<std::uint32_t>;

 private:
  struct PerInstance {
    std::vector<const slang::ast::ParameterSymbol*> supplied;
    std::unordered_set<const slang::ast::ParameterSymbol*> varying;
    std::unordered_map<
        const slang::ast::ParameterSymbol*, std::vector<std::string>>
        folded_to;
  };

  using InstanceBodies =
      std::unordered_set<const slang::ast::InstanceBodySymbol*>;

  struct KeptName {
    std::string name;
    // The instances it looked for among those being named and did not find.
    InstanceBodies depends_on;
  };

  struct NameInProgress {
    // How long the chain was when this name was asked for, so a landing on an
    // earlier entry is one outside it.
    std::size_t chain_at_start = 0;
    bool met_nothing_outside = true;
    InstanceBodies depends_on;
  };

  using ParameterSet = std::unordered_set<const slang::ast::ParameterSymbol*>;

  // What a body's own text says of its value parameters, whichever instance
  // it is read for: the ones whose value decides what is compiled, what tells
  // two values of a loop's index apart, and which parameters each one's
  // declaration is written into.
  struct PerBody {
    ParameterSet own;
    ParameterSet deciding;
    std::unordered_map<const slang::ast::ParameterSymbol*, ParameterSet>
        written_into;
    std::unordered_map<
        const slang::ast::ParameterSymbol*, std::vector<std::string>>
        folded_to;
  };

  auto Of(const slang::ast::InstanceSymbol& inst) const -> const PerInstance&;
  auto Classify(const slang::ast::InstanceSymbol& inst) const -> PerInstance;
  auto OfBody(const slang::ast::InstanceBodySymbol& body) const
      -> const PerBody&;
  auto ReadThroughItsOwnBody() const -> HasBodyOfItsOwnFn;
  auto AnswersNow(const KeptName& kept) const -> bool;

  std::unordered_set<const slang::ast::DefinitionSymbol*> kept_whole_;
  support::BodiesRead bodies_read_;
  mutable std::unordered_set<const slang::ast::InstanceSymbol*> read_anyway_;
  mutable std::unordered_map<const slang::ast::InstanceBodySymbol*, PerBody>
      per_body_;
  mutable std::unordered_map<const slang::ast::InstanceSymbol*, PerInstance>
      per_instance_;
  mutable InstanceContexts context_;
  mutable std::vector<const slang::ast::InstanceBodySymbol*> naming_;
  mutable std::unordered_map<const slang::ast::InstanceSymbol*, KeptName>
      names_;
  mutable std::vector<NameInProgress> names_in_progress_;
  // Two keys reaching one name would silently make two units into one, so a
  // name kept is held against the key it was folded from.
  mutable std::unordered_map<std::string, SpecializationKey> folded_;
  mutable std::unordered_map<
      const slang::ast::GenerateBlockSymbol*, support::GenerateBlockStep>
      block_steps_;
  mutable std::map<support::GenerateBlockStep, SpecializationKey>
      folded_blocks_;
};

// The key of a SystemVerilog class specialization (LRM 8.25). Two
// specializations of one generic class denote the same type iff every value
// binding is equal and every type binding is a matching type (LRM 8.25
// uniqueness rule); slang deduplicates on that rule, so distinct bindings
// arrive as distinct ClassType instances and key apart. Bare `C` and
// empty-override `C #()` resolve to the same slang ClassType and key alike.
auto SpecializationKeyOf(
    const slang::ast::ClassType& cls, const SpecializationPolicy& policy)
    -> SpecializationKey;

// The digest of everything a key says was fixed, or nothing for a key under
// which nothing was. It is computed by folding only bytes, so the producer (the
// unit naming itself) and every consumer (a parent naming a child) reach the
// same answer across separate compilations with no shared table.
auto ArgumentsDigest(const SpecializationKey& key)
    -> std::optional<std::uint64_t>;

// The name a key is known by. The definition's name when nothing was fixed, and
// otherwise that name plus the digest of what was -- bounded, so it serves as
// an identifier.
auto SpecializationName(const SpecializationKey& key) -> std::string;

// The name the class specialization `cls` is, for a caller that wants the name
// and not the key it comes from.
auto SpecializationName(
    const slang::ast::ClassType& cls, const SpecializationPolicy& policy)
    -> std::string;

// The step a subroutine or a block of statements is in the scope holding it:
// under its name (LRM 13, 9.3.4), or for a block the source gave no label by
// where it sits. The unit publishing what such a scope holds and a unit
// reaching into it by a hierarchical name (LRM 23.9) both ask here.
auto ProceduralStepOf(const slang::ast::Symbol& scope) -> support::DefPathData;

// Which declaration of its compilation unit `declaration` is: one step for
// each scope the source nests it in, from the unit's own scope down, and one
// for the declaration itself, which is a scope too -- a generate block as the
// application of its definition it is (LRM 27.3), a class under its name and
// what its parameters were bound to (LRM 8.3, 8.25), a subroutine or a labelled
// block under its name (LRM 13, 9.3.4), an enumeration, a structure or a union
// under the name it answers to (LRM 6.22.1), and a block or type with no name
// by where it sits.
// The unit's own scope is the path of no steps, so the block instances of one
// application are one declaration, as the instances of one specialization are.
//
// The source alone decides the answer, so the unit declaring something and
// every unit naming it compute the same path here, and a type SystemVerilog
// identifies by its declaration (LRM 8.3, 6.22.1) is identified from anywhere
// by the name of the unit holding it and this path.
auto DefPathOf(
    const slang::ast::Symbol& declaration, const SpecializationPolicy& policy)
    -> support::DefPath;

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

// Whether `cls` belongs to an instance (LRM 6.22): an instance replicates it,
// so each instance of that scope has a type of its own. A class a package or
// the `$unit` scope declares belongs to none, whichever unit reaches it, unless
// it is a specialization on a type that does.
[[nodiscard]] auto BelongsToAnInstance(const slang::ast::ClassType& cls)
    -> bool;

// The name a compilation unit publishes for itself, so a consumer reaching one
// of its declarations and the unit emitting that declaration agree with no
// shared table (LRM 26.3). A package publishes its declared name; a module body
// its specialization name. An anonymous compilation-unit scope (the LRM 3.12.1
// `$unit` file-set scope, modeled as a namespace unit with no source name)
// publishes a name derived from its own source-input identity: the only
// property distinguishing two such scopes is which compilation-unit input they
// belong to, which the LRM uses to define the scope boundary itself. A
// specialization that is a unit of its own publishes the name of the unit
// declaring its generic joined to its own the way a specialization's name joins
// its definition to the rest, so the name is an identifier as a module's is.
// Both the producer and every consumer compute this from the same slang unit
// symbol.
auto CompilationUnitName(
    const slang::ast::Symbol& unit, const SpecializationPolicy& policy)
    -> std::string;

}  // namespace lyra::lowering::ast_to_hir
