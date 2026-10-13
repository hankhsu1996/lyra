// Which of an instance's value parameters are supplied at construction, which
// its declarations compute from those, and which decide what is compiled.
//
// A parameter read where the source wrote an expression is lowered as that
// expression, and the object reads the value it was built with. So a parameter
// is supplied unless what is compiled holds a constant settled from it: the
// language requires a constant there -- a range, a width, a count, a value a
// class specialization is handed -- and the front end evaluated it. Which
// parameters each such evaluation read is the front end's own record, kept as
// it evaluates, so no position of the language is listed here and none can be
// missed. Two kinds of evaluation hold nothing compiled and are passed over:
// what a construction evaluates to decide what it builds -- a generate's
// selection, a loop's count (LRM 27.4, 27.5) -- which the front end marks; and
// the few positions this compiler lowers as the source wrote them, which it
// lists itself.

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

// What the values an object may be supplied fixed about what is compiled: for
// each, the constants the front end settled from it, each taken where the body
// states it and once however often it was evaluated, so two values that fold
// alike everywhere are told apart by nothing; and its own value where one of
// those places kept no constant. The front end evaluates when something first
// asks, so its record is in no order of the body's.
class WhatSuppliedValuesFixed {
 public:
  void Settled(
      const slang::ast::ParameterSymbol& supplied,
      const slang::ast::Expression& where,
      const slang::ConstantValue& constant) {
    const slang::SourceLocation at = where.sourceRange.start();
    settled_at_[&supplied].emplace_back(
        std::pair{at.buffer().getId(), at.offset()}, ValueIdentity(constant));
  }

  void ByItsOwnValue(const slang::ast::ParameterSymbol& supplied) {
    by_own_value_.insert(&supplied);
  }

  [[nodiscard]] auto Folded() && -> std::unordered_map<
      const slang::ast::ParameterSymbol*, std::vector<std::string>> {
    std::unordered_map<
        const slang::ast::ParameterSymbol*, std::vector<std::string>>
        out;
    for (auto& [supplied, constants] : settled_at_) {
      std::ranges::sort(constants);
      const auto repeated = std::ranges::unique(constants, {}, &Place::first);
      constants.erase(repeated.begin(), repeated.end());
      for (Place& constant : constants) {
        out[supplied].push_back(std::move(constant.second));
      }
    }
    for (const slang::ast::ParameterSymbol* supplied : by_own_value_) {
      out[supplied] = {ValueIdentity(supplied->getValue())};
    }
    return out;
  }

 private:
  using Place = std::pair<std::pair<std::uint32_t, std::size_t>, std::string>;
  std::unordered_map<const slang::ast::ParameterSymbol*, std::vector<Place>>
      settled_at_;
  ParameterSet by_own_value_;
};

// What the values an object of a body may be supplied fixed in what the body
// declares, and apart, in the code of its processes, continuous assignments
// and subroutines, which is how that code is compiled and no part of what the
// body declares.
struct WhatTheyFix {
  std::unordered_map<
      const slang::ast::ParameterSymbol*, std::vector<std::string>>
      in_declarations;
  std::unordered_map<
      const slang::ast::ParameterSymbol*, std::vector<std::string>>
      in_code;
};

// What the values an object of `body` may be supplied fix in what is compiled.
// Each place a constant is held is one the front end evaluated where it
// required one, a parameter written into a value the object holds as compiled,
// or a parameter whose value decides by its own: one a design element declared
// inside this one reads, whose type is the type of its value, or that a class
// declares. What the place fixes is fixed by every supplied value the
// parameter it read was computed from, at the constant the place settled.
//
// So what selects an alternative of a conditional generate fixes nothing: the
// construction chooses it (LRM 27.5), and what is compiled holds every
// alternative any object selected. Nor does what counts a loop generate's
// blocks out: the construction runs the loop (LRM 27.4), and how many blocks an
// object holds is no part of what it is an object of.
auto WhatSuppliedValuesFix(
    const slang::ast::InstanceBodySymbol& body, const ParameterSet& own,
    const ParameterEdges& written_from, const SpecializationPolicy& policy)
    -> WhatTheyFix {
  EveryReference every(own);
  body.visit(every);
  EveryExpression code;
  CollectCodeOf(body, code);
  StatedAsWritten as_written;
  body.visit(as_written);

  WhatSuppliedValuesFixed in_declarations;
  WhatSuppliedValuesFixed in_code;
  // Fixes, in `layer`, every supplied value `param` was computed from, the way
  // `settle` says.
  const auto fix = [&](WhatSuppliedValuesFixed& layer,
                       const slang::ast::ParameterSymbol& param,
                       const auto& settle) {
    for (const slang::ast::ParameterSymbol* source : Reached(
             ParameterSet{&param}, written_from,
             [](const auto&) { return true; })) {
      if (CanBeSupplied(*source)) settle(layer, *source);
    }
  };

  // A design element declared inside this one is handed none of this body's
  // parameters, so what its own evaluations read of them is held there too.
  const slang::ast::Compilation& compilation = body.getCompilation();
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
      fix(code.held.contains(read.root) ? in_code : in_declarations,
          *read.parameter, [&](auto& layer, const auto& supplied) {
            if (const slang::ConstantValue* settled =
                    read.root->getConstant()) {
              layer.Settled(supplied, *read.root, *settled);
            } else {
              layer.ByItsOwnValue(supplied);
            }
          });
    }
  }

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

  // A parameter read into the value of one that is not this body's is held as
  // the value it gave, unless that is a child's the child takes at
  // construction, which this body hands when it builds the child. So a child's
  // parameter that decides there, and so which unit this body names, a class
  // specialization's, which picks a type, and a virtual interface's are each
  // what is compiled.
  for (const slang::ast::ParameterSymbol* source : own) {
    for (const slang::ast::ParameterSymbol* reader :
         compilation.getParameterValueReaders(*source)) {
      if (own.contains(reader)) continue;
      const auto child = taken_at_construction.find(reader);
      if (child != taken_at_construction.end() && child->second) continue;
      fix(in_declarations, *source, [&](auto& layer, const auto& supplied) {
        if (reader->getInitializer() != nullptr) {
          layer.Settled(
              supplied, *reader->getInitializer(), reader->getValue());
        } else {
          layer.ByItsOwnValue(supplied);
        }
      });
    }
  }

  for (const slang::ast::ParameterSymbol* param : own) {
    if (every.read_in_a_nested_element.contains(param) ||
        TypeFollowsValue(*param) || DeclaredInAClass(*param)) {
      fix(in_declarations, *param, [](auto& layer, const auto& supplied) {
        layer.ByItsOwnValue(supplied);
      });
    }
  }
  return WhatTheyFix{
      .in_declarations = std::move(in_declarations).Folded(),
      .in_code = std::move(in_code).Folded()};
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
      .fixed_in_declarations = {},
      .fixed_in_code = {}};
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
  WhatTheyFix fixed = WhatSuppliedValuesFix(body, out.own, written_from, *this);
  out.fixed_in_declarations = std::move(fixed.in_declarations);
  out.fixed_in_code = std::move(fixed.in_code);
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
  out.fixed_in_declarations = text.fixed_in_declarations;
  out.fixed_in_code = text.fixed_in_code;
  // A top-level instance is built by the design root, which hands nothing, and
  // is the only instance of its unit, so a value it is given is compiled in.
  // Every other instance is handed, in the order its body declares them, each
  // parameter an instantiation may assign that fixes nothing about what is
  // compiled -- assigned or left to its default, which the instantiation says
  // and the unit evaluates, so whether a value was written does not tell
  // instances apart (LRM 6.20.1, 23.10). A unit is compiled to one body, so
  // what a parameter fixes in its code names it as what it fixes in its
  // declarations does.
  ParameterSet roots;
  if (!inst.isTopLevel()) {
    for (const slang::ast::ParameterSymbolBase* base : declared) {
      const auto* param = base->symbol.as_if<slang::ast::ParameterSymbol>();
      if (param == nullptr || param->isLocalParam() ||
          text.fixed_in_declarations.contains(param) ||
          text.fixed_in_code.contains(param)) {
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

auto SpecializationPolicy::WhatItFixesInDeclarations(
    const slang::ast::InstanceSymbol& inst,
    const slang::ast::ParameterSymbol& index) const
    -> std::span<const std::string> {
  const PerInstance& known = Of(inst);
  const auto folded = known.fixed_in_declarations.find(&index);
  if (folded == known.fixed_in_declarations.end()) return {};
  return folded->second;
}

auto SpecializationPolicy::WhatItFixesInCode(
    const slang::ast::InstanceSymbol& inst,
    const slang::ast::ParameterSymbol& index) const
    -> std::span<const std::string> {
  const PerInstance& known = Of(inst);
  const auto fixed = known.fixed_in_code.find(&index);
  if (fixed == known.fixed_in_code.end()) return {};
  return fixed->second;
}

}  // namespace lyra::lowering::ast_to_hir
