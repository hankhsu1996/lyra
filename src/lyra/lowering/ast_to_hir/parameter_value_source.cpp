// Which of an instance's value parameters are arguments of what is compiled,
// which are supplied at construction, and which its declarations compute from
// those.
//
// A parameter read where the source wrote an expression is lowered as that
// expression, and the object reads the value it was built with: it is a
// constructor argument. One whose value something compiled relies on -- a
// range, a width, a count, a type, a value a class specialization is handed --
// is an argument of the specialization instead, as a template argument is in
// C++, and enters the name by its value: the unit's where the signature, what
// the scope declares, relies on it, and the body's where only the statements
// of that body do. Which parameters something compiled relies on is the front
// end's own record, kept as it evaluates a constant the language requires and
// as it reads a type as a fact, so no position of the language is listed here
// and none can be missed. Two kinds of reads are passed over: what a
// construction evaluates to decide what it builds -- a generate's selection, a
// loop's count (LRM 27.4, 27.5) -- which the front end marks; and the few
// positions this compiler lowers as the source wrote them, which it lists
// itself.

#include <algorithm>
#include <cstddef>
#include <span>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#include <slang/ast/ASTVisitor.h>
#include <slang/ast/Compilation.h>
#include <slang/ast/Expression.h>
#include <slang/ast/Scope.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/expressions/SelectExpressions.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/MemberSymbols.h>
#include <slang/ast/symbols/ParameterSymbols.h>
#include <slang/ast/symbols/PortSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/types/DeclaredType.h>
#include <slang/syntax/AllSyntax.h>

#include "lyra/lowering/ast_to_hir/hierarchy_override.hpp"
#include "lyra/lowering/ast_to_hir/library_cell.hpp"
#include "lyra/lowering/ast_to_hir/unit_identity.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

using ParameterSet = std::unordered_set<const slang::ast::ParameterSymbol*>;

// The value parameters `scope` declares anywhere inside it -- its own, header
// ones included, and those its blocks and subroutines declare -- which are the
// ones a reference inside it can name. A child instance is no scope of this
// one, so its parameters, which are its own, are not reached.
auto ValueParametersOf(const slang::ast::Scope& scope) -> ParameterSet {
  ParameterSet out;
  for (const auto& member : scope.members()) {
    if (const auto* value = member.as_if<slang::ast::ParameterSymbol>()) {
      out.insert(value);
    } else if (const auto* inner = member.as_if<slang::ast::Scope>()) {
      out.merge(ValueParametersOf(*inner));
    }
  }
  return out;
}

// What the body builds, and the parameters of it that decide what is compiled
// without the front end's record of what it evaluated saying so: those a
// design element declared inside this one reads. A child instance's body names
// its own parameters, so what is collected of a child is what it is handed,
// which is an expression of this body.
struct EveryReference
    : slang::ast::ASTVisitor<EveryReference, slang::ast::VisitFlags::AllGood> {
  explicit EveryReference(const ParameterSet& own) : own(&own) {
  }

  const ParameterSet* own;
  ParameterSet read_in_a_nested_element;
  // The instances this body builds, whose parameters are written from it.
  std::vector<const slang::ast::InstanceSymbol*> children;
  // The bodies of the design elements declared inside this one, wherever they
  // are built, whose evaluations read this body's parameters by name.
  std::vector<const slang::ast::InstanceBodySymbol*> nested_bodies;
  bool in_a_nested_element = false;

  void Note(const slang::ast::ValueExpressionBase& e) {
    const auto* param = e.symbol.as_if<slang::ast::ParameterSymbol>();
    if (param == nullptr || !own->contains(param)) return;
    if (in_a_nested_element) read_in_a_nested_element.insert(param);
  }

  void handle(const slang::ast::NamedValueExpression& e) {
    Note(e);
    visitDefault(e);
  }

  void handle(const slang::ast::HierarchicalValueExpression& e) {
    Note(e);
    visitDefault(e);
  }

  // A design element declared inside another reads the enclosing one's
  // parameters by name (LRM 23.4, 23.9), and its instance is handed none of
  // them when it is built, so what its body reads of them is the value this
  // elaboration gave.
  void handle(const slang::ast::InstanceSymbol& child) {
    child.visitExprs(*this);
    if (!in_a_nested_element) children.push_back(&child);
    if (!IsLibraryCell(child.getDefinition())) {
      nested_bodies.push_back(&child.body);
      const bool outer = in_a_nested_element;
      in_a_nested_element = true;
      child.body.visit(*this);
      in_a_nested_element = outer;
    }
  }
};

// Every expression a visited piece of code holds.
struct EveryExpression
    : slang::ast::ASTVisitor<EveryExpression, slang::ast::VisitFlags::AllGood> {
  std::unordered_set<const slang::ast::Expression*> held;

  template <typename T>
    requires std::is_base_of_v<slang::ast::Expression, T>
  void handle(const T& e) {
    held.insert(&e);
    visitDefault(e);
  }
};

// Collects into `code` every expression a statement holds in the code of a
// process, a continuous assignment or a subroutine `scope` holds: what that
// code runs, which is no part of what the scope declares. A subroutine's
// signature and a process's static variables are declarations, the storage of
// the object and what a caller sees (LRM 6.21, 13.4), so only statements are
// reached. A class's methods are part of the class, which the scope declares,
// and a child instance's body is none of this body's code; neither is reached.
void CollectCodeOf(const slang::ast::Scope& scope, EveryExpression& code) {
  for (const auto& member : scope.members()) {
    if (const auto* process =
            member.as_if<slang::ast::ProceduralBlockSymbol>()) {
      process->getBody().visit(code);
    } else if (
        const auto* assign =
            member.as_if<slang::ast::ContinuousAssignSymbol>()) {
      assign->getAssignment().visit(code);
    } else if (
        const auto* subroutine = member.as_if<slang::ast::SubroutineSymbol>()) {
      subroutine->getBody().visit(code);
    } else if (
        member.kind == slang::ast::SymbolKind::GenerateBlock ||
        member.kind == slang::ast::SymbolKind::GenerateBlockArray) {
      CollectCodeOf(member.as<slang::ast::Scope>(), code);
    }
  }
}

// The expressions the lowering states as they are written although the
// language requires each to be a constant: the index of an element select and
// the base of an indexed part select, which name a position rather than a
// shape (LRM 11.5.1, 11.5.3), and an input port's default, which each object
// it is left unconnected in evaluates (LRM 23.2.2.4). What the front end
// settled any of them to is held by nothing compiled. A child instance's body
// is the child's, so only what this body writes for it is reached.
//
// A select the front end found out of bounds is the exception: it reaches
// nothing (LRM 11.5.1), which is what is compiled, so its position is no
// position stated as written.
struct StatedAsWritten
    : slang::ast::ASTVisitor<StatedAsWritten, slang::ast::VisitFlags::AllGood> {
  std::unordered_set<const slang::ast::Expression*> held;

  void handle(const slang::ast::ElementSelectExpression& e) {
    if (!e.isOutOfBounds()) held.insert(&e.selector());
    visitDefault(e);
  }

  void handle(const slang::ast::RangeSelectExpression& e) {
    if (e.getSelectionKind() != slang::ast::RangeSelectionKind::Simple &&
        !e.isOutOfBounds()) {
      held.insert(&e.left());
    }
    visitDefault(e);
  }

  void handle(const slang::ast::PortSymbol& port) {
    held.insert(port.getInitializer());
    visitDefault(port);
  }

  void handle(const slang::ast::InstanceSymbol& child) {
    child.visitExprs(*this);
  }
};

// The parameters of `own` that `param`'s own value was evaluated from.
auto DependenciesOf(
    const slang::ast::ParameterSymbol& param, const ParameterSet& own)
    -> ParameterSet {
  ParameterSet out;
  if (param.getParentScope()->isUninstantiated()) return out;
  param.getValue();
  const slang::ast::Compilation& compilation =
      param.getParentScope()->getCompilation();
  for (const slang::ast::ParameterSymbol* source :
       compilation.getParameterValueSources(param)) {
    if (own.contains(source)) out.insert(source);
  }
  return out;
}

// A parameter written with neither a type nor a range takes the type of the
// value it is given (LRM 6.20.2). One an instantiation may override can be
// given a value of any type, so its value decides a type wherever it is read.
// A local one is given only its own initializer, whose type follows from the
// places that expression reads, and those are read where it stands.
auto TypeFollowsValue(const slang::ast::ParameterSymbol& param) -> bool {
  if (param.isLocalParam()) {
    return false;
  }
  const slang::syntax::DataTypeSyntax* syntax =
      param.getDeclaredType()->getTypeSyntax();
  if (syntax == nullptr ||
      syntax->kind != slang::syntax::SyntaxKind::ImplicitType) {
    return false;
  }
  return syntax->as<slang::syntax::ImplicitTypeSyntax>().dimensions.empty();
}

// Each of a unit's parameters mapped to others of its parameters: the ones its
// declaration is written from, or the other way round. Every parameter of the
// unit has an entry, so a walk along it never meets a missing one.
using ParameterEdges =
    std::unordered_map<const slang::ast::ParameterSymbol*, ParameterSet>;

// `from`, and every parameter reached from it along `edges` through ones
// `admits` accepts.
template <typename Admits>
auto Reached(ParameterSet from, const ParameterEdges& edges, Admits admits)
    -> ParameterSet {
  std::vector<const slang::ast::ParameterSymbol*> pending(
      from.begin(), from.end());
  while (!pending.empty()) {
    const slang::ast::ParameterSymbol* param = pending.back();
    pending.pop_back();
    for (const slang::ast::ParameterSymbol* next : edges.at(param)) {
      if (admits(*next) && from.insert(next).second) pending.push_back(next);
    }
  }
  return from;
}

auto DeclaredByAGenerateBlock(const slang::ast::ParameterSymbol& param)
    -> bool {
  const slang::ast::Scope* declaring = param.getParentScope();
  return declaring != nullptr &&
         declaring->asSymbol().kind == slang::ast::SymbolKind::GenerateBlock;
}

// Whether `param` is declared in a class or in one of its methods. A class's
// code is no body of the unit's objects, so it holds the parameter's value as
// the constant this elaboration settled rather than one an object is built
// with.
auto DeclaredInAClass(const slang::ast::ParameterSymbol& param) -> bool {
  for (const slang::ast::Scope* level = param.getParentScope();
       level != nullptr; level = level->asSymbol().getParentScope()) {
    const slang::ast::SymbolKind kind = level->asSymbol().kind;
    if (kind == slang::ast::SymbolKind::ClassType) return true;
    if (kind == slang::ast::SymbolKind::InstanceBody) return false;
  }
  return false;
}

// Whether an object built from the scope declaring `param` may be handed its
// value: a parameter its instantiation may assign (LRM 23.10), or the index a
// loop's block is built at (LRM 27.4). Every other parameter's value follows
// from these and its own declaration.
auto CanBeSupplied(const slang::ast::ParameterSymbol& param) -> bool {
  if (param.isFromGenvar()) return true;
  return !param.isLocalParam() && !DeclaredByAGenerateBlock(param) &&
         !DeclaredInAClass(param);
}

// The parameters what a body compiles to uses, each entering a name by its own
// value as a template argument does in C++ -- what rustc's polymorphization
// called a definition's used generic parameters: those its signature, what the
// body declares, uses, and apart, those only its body uses -- the statements of
// its processes, continuous assignments and subroutines, which are how it is
// compiled and no part of the signature.
struct UsedParameters {
  ParameterSet in_signature;
  ParameterSet in_body;
};

// Whether `scope` declares a class, itself or in a scope inside it. A child
// instance's body is the child's, so it is not reached.
auto DeclaresAClass(const slang::ast::Scope& scope) -> bool {
  return std::ranges::any_of(
      scope.members(), [](const slang::ast::Symbol& member) {
        if (member.kind == slang::ast::SymbolKind::ClassType ||
            member.kind == slang::ast::SymbolKind::GenericClassDef) {
          return true;
        }
        if (member.kind == slang::ast::SymbolKind::Instance) return false;
        const auto* inner = member.as_if<slang::ast::Scope>();
        return inner != nullptr && DeclaresAClass(*inner);
      });
}

// The parameters an object could be handed that `param` was computed from,
// `param` among them where it is one.
auto SuppliedParametersItDependsOn(
    const slang::ast::ParameterSymbol& param,
    const ParameterEdges& written_from) -> ParameterSet {
  ParameterSet sources;
  for (const slang::ast::ParameterSymbol* source : Reached(
           ParameterSet{&param}, written_from,
           [](const auto&) { return true; })) {
    if (CanBeSupplied(*source)) sources.insert(source);
  }
  return sources;
}

// Marks what `param` was computed from as used in the signature.
void MarkUsedInSignature(
    UsedParameters& used, const slang::ast::ParameterSymbol& param,
    const ParameterEdges& written_from) {
  used.in_signature.merge(SuppliedParametersItDependsOn(param, written_from));
}

// Marks what `param` was computed from as used in the body -- each in the body
// of the scope it is supplied to, unless that scope declares a class. A class
// reaches the object of the scope declaring it as that scope's body compiled
// it, and is one class however many bodies there were, so such a scope has one
// body and the parameter is used in its signature instead.
void MarkUsedInBody(
    UsedParameters& used, const slang::ast::ParameterSymbol& param,
    const ParameterEdges& written_from) {
  for (const slang::ast::ParameterSymbol* source :
       SuppliedParametersItDependsOn(param, written_from)) {
    if (DeclaresAClass(*source->getParentScope())) {
      used.in_signature.insert(source);
    } else {
      used.in_body.insert(source);
    }
  }
}

// Marks the parameters the front end recorded as read by a constant it
// evaluated where the language requires one, or by a type it read as a fact. A
// design element declared inside this one is handed none of this body's
// parameters, so what its own evaluations read of them counts here too. Passed
// over are what only decides which blocks a construction builds and what the
// lowering states as written; a read in what a process, a continuous
// assignment or a subroutine runs is a use in the body, any other one in the
// signature.
void MarkUsedInConstantsAndTypes(
    UsedParameters& used, const slang::ast::InstanceBodySymbol& body,
    const ParameterSet& own, const ParameterEdges& written_from,
    std::span<const slang::ast::InstanceBodySymbol* const> nested_bodies) {
  EveryExpression code;
  CollectCodeOf(body, code);
  StatedAsWritten as_written;
  body.visit(as_written);

  std::vector<const slang::ast::InstanceBodySymbol*> evaluated_in{&body};
  evaluated_in.insert(
      evaluated_in.end(), nested_bodies.begin(), nested_bodies.end());
  for (const slang::ast::InstanceBodySymbol* in : evaluated_in) {
    for (const slang::ast::ParameterRead& read :
         body.getCompilation().getParameterReads(*in)) {
      if (!own.contains(read.parameter) || read.decidesGenerateBlocks ||
          as_written.held.contains(read.root)) {
        continue;
      }
      if (code.held.contains(read.root)) {
        MarkUsedInBody(used, *read.parameter, written_from);
      } else {
        MarkUsedInSignature(used, *read.parameter, written_from);
      }
    }
  }
}

// Marks the parameters of this body read into the value of a parameter that is
// not this body's own, as rustc's `visit_child_body` marks the arguments a
// nested body uses. A parameter of a child this body builds is handed when the
// child is built, so where the child does not use it, this body does not
// either; where the child does not take it at construction, which is where the
// child's signature uses it, this body uses it in its signature, which names
// the child's unit as a member's type, and where the child uses it in its body
// only, this
// body uses it in its body, since which body is built there is how this body
// is built. Any other parameter it is read into -- a class specialization's,
// which picks a type, a virtual interface's -- is a use in the signature.
// Asking for a child's value is what has the front end evaluate it, which is
// what records what it was evaluated from.
void MarkUsedByChildArgs(
    UsedParameters& used, const ParameterSet& own,
    const ParameterEdges& written_from,
    std::span<const slang::ast::InstanceSymbol* const> children,
    const SpecializationPolicy& policy) {
  std::unordered_map<
      const slang::ast::ParameterSymbol*, const slang::ast::InstanceSymbol*>
      child_of;
  for (const slang::ast::InstanceSymbol* child : children) {
    for (const auto* base : child->body.getParameters()) {
      if (const auto* param =
              base->symbol.as_if<slang::ast::ParameterSymbol>()) {
        param->getValue();
        child_of.emplace(param, child);
      }
    }
  }

  for (const slang::ast::ParameterSymbol* source : own) {
    for (const slang::ast::ParameterSymbol* reader :
         source->getParentScope()->getCompilation().getParameterValueReaders(
             *source)) {
      if (own.contains(reader)) continue;
      const auto child = child_of.find(reader);
      if (child == child_of.end()) {
        MarkUsedInSignature(used, *source, written_from);
        continue;
      }
      const slang::ast::InstanceSymbol& built = *child->second;
      if (!std::ranges::contains(policy.SuppliedParametersOf(built), reader)) {
        MarkUsedInSignature(used, *source, written_from);
      } else if (policy.IsUsedInBody(built, *reader)) {
        MarkUsedInBody(used, *source, written_from);
      }
    }
  }
}

// Marks the parameters used whatever the body does, as rustc's
// `mark_used_by_default_parameters` does: one a design element declared inside
// this one reads, since its instance is handed none of them; one whose type is
// its value's (LRM 6.20.2); one a class declares, since a class's code is no
// body of the unit's objects.
void MarkUsedByDefaultParameters(
    UsedParameters& used, const ParameterSet& own,
    const ParameterEdges& written_from,
    const ParameterSet& read_in_a_nested_element) {
  for (const slang::ast::ParameterSymbol* param : own) {
    if (read_in_a_nested_element.contains(param) || TypeFollowsValue(*param) ||
        DeclaredInAClass(*param)) {
      MarkUsedInSignature(used, *param, written_from);
    }
  }
}

// The parameters what the specialization `body` is read through compiles to
// uses: every one an object could be handed whose value something compiled
// relies on, together with every such parameter that one was computed from.
//
// So what selects an alternative of a conditional generate decides nothing:
// the construction chooses it (LRM 27.5), and what is compiled holds every
// alternative any object selected. Nor does what counts a loop generate's
// blocks out: the construction runs the loop (LRM 27.4), and how many blocks an
// object holds is no part of what it is an object of.
auto UsedParametersOf(
    const slang::ast::InstanceBodySymbol& body, const ParameterSet& own,
    const ParameterEdges& written_from, const SpecializationPolicy& policy)
    -> UsedParameters {
  EveryReference every(own);
  body.visit(every);

  UsedParameters used;
  MarkUsedInConstantsAndTypes(
      used, body, own, written_from, every.nested_bodies);
  MarkUsedByChildArgs(used, own, written_from, every.children, policy);
  MarkUsedByDefaultParameters(
      used, own, written_from, every.read_in_a_nested_element);
  return used;
}

}  // namespace

auto SpecializationPolicy::SuppliedParametersOf(
    const slang::ast::InstanceSymbol& inst) const
    -> std::span<const slang::ast::ParameterSymbol* const> {
  return Of(inst).supplied;
}

auto SpecializationPolicy::Of(const slang::ast::InstanceSymbol& inst) const
    -> const PerInstance& {
  auto cached = per_instance_.find(&inst);
  if (cached == per_instance_.end()) {
    cached = per_instance_.emplace(&inst, Classify(inst)).first;
  }
  return cached->second;
}

auto SpecializationPolicy::OfBody(
    const slang::ast::InstanceBodySymbol& body) const -> const PerBody& {
  if (const auto kept = per_body_.find(&body); kept != per_body_.end()) {
    return kept->second;
  }
  PerBody out{
      .own = ValueParametersOf(body),
      .written_into = {},
      .used_in_signature = {},
      .used_in_body = {}};
  ParameterEdges written_from;
  for (const slang::ast::ParameterSymbol* param : out.own) {
    written_from.emplace(param, DependenciesOf(*param, out.own));
    out.written_into.emplace(param, ParameterSet{});
  }
  for (const auto& [param, sources] : written_from) {
    for (const slang::ast::ParameterSymbol* source : sources) {
      out.written_into.at(source).insert(param);
    }
  }
  UsedParameters used = UsedParametersOf(body, out.own, written_from, *this);
  out.used_in_signature = std::move(used.in_signature);
  out.used_in_body = std::move(used.in_body);
  return per_body_.emplace(&body, std::move(out)).first->second;
}

// What a body's text says of its parameters is one answer for every instance
// that body serves; what an instance adds is which of them it was given a
// value for, and whether anything builds it. So the text is asked of the body
// the instance is read through, and the rest of the instance itself, a
// parameter of the one standing for the parameter of the other declared at the
// same place.
auto SpecializationPolicy::Classify(
    const slang::ast::InstanceSymbol& inst) const -> PerInstance {
  const slang::ast::InstanceBodySymbol& read_through =
      HasBodyOfItsOwn(inst) ? inst.body : *inst.getCanonicalBody();
  const PerBody& text = OfBody(read_through);

  const auto declared = read_through.getParameters();
  const auto handed = inst.body.getParameters();
  std::unordered_map<
      const slang::ast::ParameterSymbol*, const slang::ast::ParameterSymbol*>
      of_instance;
  for (std::size_t at = 0; at < declared.size(); ++at) {
    const auto* value =
        declared[at]->symbol.as_if<slang::ast::ParameterSymbol>();
    if (value == nullptr) continue;
    of_instance.emplace(
        value, &handed[at]->symbol.as<slang::ast::ParameterSymbol>());
  }
  // A value given anywhere -- by the instantiation, a defparam, or a
  // configuration (LRM 23.10) -- is one value the instance is handed. A
  // parameter a block or a subroutine declares is the read body's own.
  const auto given_a_value = [&](const slang::ast::ParameterSymbol& param) {
    const auto its = of_instance.find(&param);
    if (its == of_instance.end()) {
      return param.isOverridden() || ValueSetElsewhere(read_through, param);
    }
    return its->second->isOverridden() ||
           ValueSetElsewhere(inst.body, *its->second);
  };

  // A header parameter of the read body stands for the instance's own.
  const auto as_handed = [&](const slang::ast::ParameterSymbol* param) {
    const auto its = of_instance.find(param);
    return its == of_instance.end() ? param : its->second;
  };
  PerInstance out;
  for (const slang::ast::ParameterSymbol* param : text.used_in_signature) {
    out.used_in_signature.insert(as_handed(param));
  }
  for (const slang::ast::ParameterSymbol* param : text.used_in_body) {
    out.used_in_body.insert(as_handed(param));
  }
  // A top-level instance is built by the design root, which hands nothing, and
  // is the only instance of its unit, so a value it is given is compiled in.
  // Every other instance is handed, in the order its body declares them, each
  // parameter an instantiation may assign that its signature does not use --
  // assigned or left to its default, which the instantiation says and the unit
  // evaluates, so whether a value was written does not tell instances apart
  // (LRM 6.20.1, 23.10). One its body uses names the body as well, and a read
  // of it is of the value the object was built with.
  ParameterSet roots;
  if (!inst.isTopLevel()) {
    for (const slang::ast::ParameterSymbolBase* base : declared) {
      const auto* param = base->symbol.as_if<slang::ast::ParameterSymbol>();
      if (param == nullptr || param->isLocalParam() ||
          text.used_in_signature.contains(param)) {
        continue;
      }
      out.supplied.push_back(of_instance.at(param));
      roots.insert(param);
    }
  }

  // What varies to begin with is what the unit is handed and what a generate
  // block declares, since a block is built with its own. What a declaration
  // writes from a value that varies varies with it, and so on; one whose value
  // comes from elsewhere is not written by its declaration.
  for (const slang::ast::ParameterSymbol* param : text.own) {
    if (DeclaredByAGenerateBlock(*param)) roots.insert(param);
  }
  for (const slang::ast::ParameterSymbol* param : Reached(
           std::move(roots), text.written_into,
           [&](const slang::ast::ParameterSymbol& param) {
             return !given_a_value(param);
           })) {
    const auto its = of_instance.find(param);
    out.varying.insert(its == of_instance.end() ? param : its->second);
  }
  return out;
}

auto SpecializationPolicy::ValueSourceOf(
    const slang::ast::InstanceSymbol& inst,
    const slang::ast::ParameterSymbol& param) const -> ParameterValueSource {
  const PerInstance& known = Of(inst);
  if (!known.varying.contains(&param)) {
    return ParameterValueSource::kFixedBySpecialization;
  }
  if (param.isFromGenvar() || std::ranges::contains(known.supplied, &param)) {
    return ParameterValueSource::kSuppliedAtConstruction;
  }
  return ParameterValueSource::kComputedAtConstruction;
}

auto SpecializationPolicy::IsUsedInSignature(
    const slang::ast::InstanceSymbol& inst,
    const slang::ast::ParameterSymbol& param) const -> bool {
  return Of(inst).used_in_signature.contains(&param);
}

auto SpecializationPolicy::IsUsedInBody(
    const slang::ast::InstanceSymbol& inst,
    const slang::ast::ParameterSymbol& param) const -> bool {
  return Of(inst).used_in_body.contains(&param);
}

}  // namespace lyra::lowering::ast_to_hir
