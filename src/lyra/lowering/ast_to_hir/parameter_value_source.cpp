// Which of an instance's value parameters are supplied at construction, which
// its declarations compute from those, and which decide what is compiled.
//
// A parameter read where the source wrote an expression is lowered as that
// expression, and the object reads the value it was built with. So a parameter
// is supplied unless what is compiled holds a constant settled from it: the
// language requires a constant there -- a range, a width, a count, a value a
// class specialization is handed -- and the front end evaluated it. Which
// parameters each such evaluation read is the front end's own record, kept as
// it evaluates, so no position is listed here and none can be missed. What a
// construction evaluates to decide what it builds -- a generate's selection,
// a loop's count (LRM 27.4, 27.5) -- holds nothing compiled; the front end
// marks such a read, and it is passed over.

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <string>
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
#include <slang/numeric/ConstantValue.h>
#include <slang/syntax/AllSyntax.h>
#include <slang/text/SourceLocation.h>

#include "lyra/lowering/ast_to_hir/hierarchy_override.hpp"
#include "lyra/lowering/ast_to_hir/library_cell.hpp"
#include "lyra/lowering/ast_to_hir/unit_identity.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

using ParameterSet = std::unordered_set<const slang::ast::ParameterSymbol*>;
using References = std::unordered_map<
    const slang::ast::ParameterSymbol*,
    std::unordered_set<const slang::ast::Expression*>>;

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

// Every reference to the body's own parameters, and the ones that decide what
// is compiled without the front end's record of what it evaluated saying so:
// those a design element declared inside this one writes. A child instance's
// body names its own parameters, so what is collected of a child is what it is
// handed, which is an expression of this body.
struct EveryReference
    : slang::ast::ASTVisitor<EveryReference, slang::ast::VisitFlags::AllGood> {
  explicit EveryReference(const ParameterSet& own) : own(&own) {
  }

  const ParameterSet* own;
  References found;
  std::unordered_set<const slang::ast::Expression*> deciding;
  // The instances this body builds, whose parameters are written from it.
  std::vector<const slang::ast::InstanceSymbol*> children;
  // The bodies of the design elements declared inside this one, wherever they
  // are built, whose evaluations read this body's parameters by name.
  std::vector<const slang::ast::InstanceBodySymbol*> nested_bodies;
  bool in_a_nested_element = false;

  void Note(const slang::ast::ValueExpressionBase& e) {
    const auto* param = e.symbol.as_if<slang::ast::ParameterSymbol>();
    if (param == nullptr || !own->contains(param)) return;
    found[param].insert(&e);
    if (in_a_nested_element) deciding.insert(&e);
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

// Collects into `code` every expression in the code of a process, a
// continuous assignment or a subroutine `scope` holds, wherever in it the
// expression sits. A child instance is no scope of this one, so its body,
// which is none of this body's code, is not reached.
void CollectCodeOf(const slang::ast::Scope& scope, EveryExpression& code) {
  for (const auto& member : scope.members()) {
    if (const auto* process =
            member.as_if<slang::ast::ProceduralBlockSymbol>()) {
      process->getBody().visit(code);
    } else if (
        const auto* assign =
            member.as_if<slang::ast::ContinuousAssignSymbol>()) {
      assign->getAssignment().visit(code);
    }
    if (const auto* subroutine = member.as_if<slang::ast::SubroutineSymbol>()) {
      subroutine->getBody().visit(code);
    }
    if (const auto* inner = member.as_if<slang::ast::Scope>()) {
      CollectCodeOf(*inner, code);
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
struct StatedAsWritten
    : slang::ast::ASTVisitor<StatedAsWritten, slang::ast::VisitFlags::AllGood> {
  std::unordered_set<const slang::ast::Expression*> held;

  void handle(const slang::ast::ElementSelectExpression& e) {
    held.insert(&e.selector());
    visitDefault(e);
  }

  void handle(const slang::ast::RangeSelectExpression& e) {
    if (e.getSelectionKind() != slang::ast::RangeSelectionKind::Simple) {
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

// Whether an evaluation in `scope` evaluated the code of a process or a
// subroutine rather than a declaration of the block: a variable of automatic
// lifetime exists only while its body runs (LRM 6.21), so what it is declared
// as is part of that body's code.
auto IsCodeScope(const slang::ast::Scope& scope) -> bool {
  using slang::ast::SymbolKind;
  for (const slang::ast::Scope* level = &scope; level != nullptr;
       level = level->asSymbol().getParentScope()) {
    const SymbolKind kind = level->asSymbol().kind;
    if (kind == SymbolKind::StatementBlock || kind == SymbolKind::Subroutine) {
      return true;
    }
    if (kind == SymbolKind::GenerateBlock || kind == SymbolKind::InstanceBody) {
      return false;
    }
  }
  return false;
}

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

// The parameters of a body whose value decides what is compiled, and for each
// of them that is the index of a loop's block (LRM 27.4), what tells two of
// its values apart: the constants the front end settled from it wherever it
// decides, in the order the body states them, so two values that fold alike
// everywhere are told apart by nothing. Where one of those places kept no
// constant, or the index decides through another parameter written from it,
// its own value is what is left.
struct Deciding {
  ParameterSet parameters;
  std::unordered_map<
      const slang::ast::ParameterSymbol*, std::vector<std::string>>
      folded_to;
};

// What decides for `body`: a parameter one of whose references sits where the
// value is held by what is compiled, whose type is the type of its value, or
// that a class declares; and what a deciding parameter is written from decides
// with it.
//
// So what selects an alternative of a conditional generate decides nothing:
// the construction chooses it (LRM 27.5), and what is compiled holds every
// alternative any object selected. Nor does what counts a loop generate's
// blocks out: the construction runs the loop (LRM 27.4), and how many blocks an
// object holds is no part of what it is an object of.
//
// A parameter a generate block declares decides less still, because a block is
// built once per object of its scope, each with its own value, and the blocks
// of one text are objects of one class whose code may be compiled once per
// block. So for such a parameter the code of a process, a continuous assignment
// or a subroutine decides nothing wherever in it the parameter is read: it is
// no part of what the class declares, and blocks whose code comes out different
// are that class realized more than once.
auto DecidingParametersOf(
    const slang::ast::InstanceBodySymbol& body, const ParameterSet& own,
    const ParameterEdges& written_from, const SpecializationPolicy& policy)
    -> Deciding {
  EveryReference every(own);
  body.visit(every);

  EveryExpression code;
  CollectCodeOf(body, code);

  // What the front end evaluated where it required a constant -- the place it
  // settled one this body holds -- read off its own record of the evaluation.
  // A read of a block's own parameter in the code of a process or a
  // subroutine is a read of that class's code, which decides nothing about
  // what the class declares.
  StatedAsWritten as_written;
  body.visit(as_written);

  // A design element declared inside this one is handed none of this body's
  // parameters, so what its own evaluations read of them is held there too.
  const slang::ast::Compilation& compilation = body.getCompilation();
  std::vector<slang::ast::ParameterRead> held;
  std::vector<const slang::ast::InstanceBodySymbol*> evaluated_in{&body};
  evaluated_in.insert(
      evaluated_in.end(), every.nested_bodies.begin(),
      every.nested_bodies.end());
  for (const slang::ast::InstanceBodySymbol* in : evaluated_in) {
    for (const slang::ast::ParameterRead& read :
         compilation.getParameterReads(*in)) {
      if (!own.contains(read.parameter) || read.decidesGenerateBlocks ||
          as_written.held.contains(read.root)) {
        continue;
      }
      if (DeclaredByAGenerateBlock(*read.parameter) &&
          (IsCodeScope(*read.scope) || code.held.contains(read.root))) {
        continue;
      }
      held.push_back(read);
    }
  }

  const auto decides_there = [&](const slang::ast::Expression* use) {
    return every.deciding.contains(use);
  };
  const auto decides_somewhere = [&](const slang::ast::ParameterSymbol* param) {
    const auto all = every.found.find(param);
    if (all == every.found.end()) return false;
    return std::ranges::any_of(all->second, decides_there);
  };
  ParameterSet roots;
  for (const slang::ast::ParameterRead& read : held) {
    roots.insert(read.parameter);
  }
  for (const slang::ast::ParameterSymbol* param : own) {
    if (decides_somewhere(param) || TypeFollowsValue(*param) ||
        DeclaredInAClass(*param)) {
      roots.insert(param);
    }
  }
  // Where a loop's index settled a constant: where the body states it, and the
  // constant. The front end evaluates when something first asks, so the record
  // is in no order of the body's; each constant is taken where the body states
  // it, and once however often it was evaluated.
  using Settled = std::pair<std::pair<std::uint32_t, std::size_t>, std::string>;
  std::unordered_map<const slang::ast::ParameterSymbol*, std::vector<Settled>>
      settled_at;
  const auto settled_where = [&](const slang::ast::ParameterSymbol& index,
                                 const slang::ast::Expression& where,
                                 const slang::ConstantValue& constant) {
    const slang::SourceLocation at = where.sourceRange.start();
    settled_at[&index].emplace_back(
        std::pair{at.buffer().getId(), at.offset()}, ValueIdentity(constant));
  };

  // What each parameter of a child this body builds is handed, and whether the
  // child takes it at construction. Asking for the value is what has the front
  // end evaluate it, which is what records what it was evaluated from.
  std::unordered_map<const slang::ast::ParameterSymbol*, bool>
      taken_at_construction;
  for (const slang::ast::InstanceSymbol* child : every.children) {
    const auto supplied = policy.SuppliedParametersOf(*child);
    for (const auto* base : child->body.getParameters()) {
      const auto* param = base->symbol.as_if<slang::ast::ParameterSymbol>();
      if (param == nullptr) continue;
      param->getValue();
      taken_at_construction.emplace(
          param, std::ranges::contains(supplied, param));
    }
  }

  // A parameter read into another's value decides with it. Where that other
  // is this body's own, the edge is followed below; where it is a child's that
  // the child takes at construction, this body hands the value when it builds
  // the child. Into anything else -- a child's parameter that decides there,
  // and so which unit this body names; a class specialization's, which picks a
  // type; a virtual interface's -- the value is held by what is compiled. An
  // index of a loop's block so read is told apart by the value it gave.
  for (const slang::ast::ParameterSymbol* source : own) {
    for (const slang::ast::ParameterSymbol* reader :
         compilation.getParameterValueReaders(*source)) {
      if (own.contains(reader)) continue;
      const auto child = taken_at_construction.find(reader);
      if (child != taken_at_construction.end() && child->second) continue;
      roots.insert(source);
      if (source->isFromGenvar() && reader->getInitializer() != nullptr) {
        settled_where(*source, *reader->getInitializer(), reader->getValue());
      }
    }
  }
  Deciding deciding{
      .parameters = Reached(
          std::move(roots), written_from, [](const auto&) { return true; }),
      .folded_to = {}};

  // An index of a loop's block is told apart by the constant each read of it
  // settled, and by its own value where it decides through another parameter
  // or where nothing settled.
  ParameterSet by_own_value;
  for (const slang::ast::ParameterSymbol* param : deciding.parameters) {
    for (const slang::ast::ParameterSymbol* source : written_from.at(param)) {
      if (source->isFromGenvar()) by_own_value.insert(source);
    }
  }
  for (const auto& [param, uses] : every.found) {
    if (param->isFromGenvar() && deciding.parameters.contains(param) &&
        std::ranges::any_of(uses, decides_there)) {
      by_own_value.insert(param);
    }
  }
  for (const slang::ast::ParameterRead& read : held) {
    if (!read.parameter->isFromGenvar()) continue;
    if (const slang::ConstantValue* settled = read.root->getConstant()) {
      settled_where(*read.parameter, *read.root, *settled);
    } else {
      by_own_value.insert(read.parameter);
    }
  }
  for (auto& [index, constants] : settled_at) {
    std::ranges::sort(constants);
    const auto repeated = std::ranges::unique(constants, {}, &Settled::first);
    constants.erase(repeated.begin(), repeated.end());
    for (Settled& constant : constants) {
      deciding.folded_to[index].push_back(std::move(constant.second));
    }
  }
  for (const slang::ast::ParameterSymbol* index : by_own_value) {
    deciding.folded_to[index] = {ValueIdentity(index->getValue())};
  }
  return deciding;
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
      .deciding = {},
      .written_into = {},
      .folded_to = {}};
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
  Deciding deciding = DecidingParametersOf(body, out.own, written_from, *this);
  out.deciding = std::move(deciding.parameters);
  out.folded_to = std::move(deciding.folded_to);
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

  PerInstance out;
  out.folded_to = text.folded_to;
  // A top-level instance is built by the design root, which hands nothing, and
  // is the only instance of its unit, so a value it is given is compiled in.
  // Every other instance is handed, in the order its body declares them, each
  // parameter an instantiation may assign that decides nothing about what is
  // compiled -- assigned or left to its default, which the instantiation says
  // and the unit evaluates, so whether a value was written does not tell
  // instances apart (LRM 6.20.1, 23.10).
  ParameterSet roots;
  if (!inst.isTopLevel()) {
    for (const slang::ast::ParameterSymbolBase* base : declared) {
      const auto* param = base->symbol.as_if<slang::ast::ParameterSymbol>();
      if (param == nullptr || param->isLocalParam() ||
          text.deciding.contains(param)) {
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

auto SpecializationPolicy::WhatItDecides(
    const slang::ast::InstanceSymbol& inst,
    const slang::ast::ParameterSymbol& index) const
    -> std::span<const std::string> {
  const PerInstance& known = Of(inst);
  const auto folded = known.folded_to.find(&index);
  if (folded == known.folded_to.end()) return {};
  return folded->second;
}

}  // namespace lyra::lowering::ast_to_hir
