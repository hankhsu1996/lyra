// Where each value parameter of an instance's body takes its value from for
// the objects built from it.
//
// A parameter the code reads as a value is a value the object is built with,
// so specializations handed different values can come out as the same code. A
// parameter the code never reads holds nothing at run time: where the language
// requires a constant -- a range, a width, a type -- the front end has already
// settled it, and that is what every declaration reads. So which parameters an
// object is handed is read off what the body's text reads, as it was bound.

#include <cstddef>
#include <span>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#include <slang/ast/ASTVisitor.h>
#include <slang/ast/Expression.h>
#include <slang/ast/Scope.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/ParameterSymbols.h>

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

// The parameters of `own` the body's text reads as values: every name of one
// an expression of the body holds, and every one a generate construct's
// scheme reads, since the construction runs the scheme as written (LRM 27.4,
// 27.5). A parameter's own initializer is read only where that parameter is,
// so it is left to the walk along initializers. A design element declared
// inside this one reads this body's parameters by name and is handed none of
// them (LRM 23.4, 23.9), so what it reads is collected apart.
struct ReadsOfParameters
    : slang::ast::ASTVisitor<
          ReadsOfParameters, slang::ast::VisitFlags::AllGood> {
  explicit ReadsOfParameters(const ParameterSet& own) : own(&own) {
  }

  const ParameterSet* own;
  ParameterSet read;
  ParameterSet read_in_a_nested_element;
  bool in_a_nested_element = false;
  bool in_an_initializer = false;

  void Note(const slang::ast::ValueExpressionBase& e) {
    const auto* param = e.symbol.as_if<slang::ast::ParameterSymbol>();
    if (param == nullptr || !own->contains(param)) return;
    if (in_an_initializer && !in_a_nested_element) return;
    if (in_a_nested_element) {
      read_in_a_nested_element.insert(param);
      return;
    }
    read.insert(param);
  }

  void handle(const slang::ast::NamedValueExpression& e) {
    Note(e);
    visitDefault(e);
  }

  void handle(const slang::ast::HierarchicalValueExpression& e) {
    Note(e);
    visitDefault(e);
  }

  // A parameter a class declares is the class's code, so what its initializer
  // reads is read there.
  void handle(const slang::ast::ParameterSymbol& param) {
    const bool outer = in_an_initializer;
    in_an_initializer = !DeclaredInAClass(param);
    visitDefault(param);
    in_an_initializer = outer;
  }

  void handle(const slang::ast::GenerateBlockArraySymbol& loop) {
    for (const slang::ast::Expression* scheme :
         {loop.initialExpression, loop.stopExpression, loop.iterExpression}) {
      if (scheme != nullptr) scheme->visit(*this);
    }
    visitDefault(loop);
  }

  void handle(const slang::ast::GenerateBlockSymbol& block) {
    for (const slang::ast::GenerateSelection& selection : block.selectionPath) {
      if (selection.condition != nullptr) selection.condition->visit(*this);
      for (const slang::ast::Expression* item : selection.caseItems) {
        item->visit(*this);
      }
    }
    visitDefault(block);
  }

  // What the instantiation writes for a child is this body's expression, the
  // values it assigns the child's parameters included (LRM 23.10.2). The
  // child's body is the child's own, unless its definition is declared inside
  // this one, whose names it then reads.
  void handle(const slang::ast::InstanceSymbol& child) {
    child.visitExprs(*this);
    for (const slang::ast::ParameterSymbolBase* base :
         child.body.getParameters()) {
      const auto* param = base->symbol.as_if<slang::ast::ParameterSymbol>();
      if (param == nullptr) continue;
      if (const slang::ast::Expression* written =
              ValueWrittenAtInstantiation(child, *param)) {
        written->visit(*this);
      }
    }
    if (!DeclaredInside(child.getDefinition())) return;
    const bool outer = in_a_nested_element;
    in_a_nested_element = true;
    child.body.visit(*this);
    in_a_nested_element = outer;
  }

  // Whether `definition` is declared inside a design element (LRM 23.4).
  static auto DeclaredInside(const slang::ast::DefinitionSymbol& definition)
      -> bool {
    if (IsLibraryCell(definition)) return false;
    const slang::ast::Scope* declaring = definition.getParentScope();
    return declaring != nullptr &&
           declaring->asSymbol().kind == slang::ast::SymbolKind::InstanceBody;
  }
};

// The parameters of `own` the initializer of `param` names.
auto InitializerReads(
    const slang::ast::ParameterSymbol& param, const ParameterSet& own)
    -> ParameterSet {
  ReadsOfParameters reads(own);
  if (const slang::ast::Expression* initializer = param.getInitializer()) {
    initializer->visit(reads);
  }
  return std::move(reads.read);
}

auto DeclaredByAGenerateBlock(const slang::ast::ParameterSymbol& param)
    -> bool {
  const slang::ast::Scope* declaring = param.getParentScope();
  return declaring != nullptr &&
         declaring->asSymbol().kind == slang::ast::SymbolKind::GenerateBlock;
}

// Whether an object built from the scope declaring `param` may be handed its
// value: a parameter its instantiation may assign (LRM 23.10). The index a
// loop's block is built at is handed by the loop instead (LRM 27.4).
auto CanBeSupplied(const slang::ast::ParameterSymbol& param) -> bool {
  return !param.isLocalParam() && !DeclaredByAGenerateBlock(param) &&
         !DeclaredInAClass(param);
}

// Whether `param` must hold the value this elaboration settled, whatever the
// code reads: a class's own, and one a design element declared inside this one
// reads, since that one is handed none.
auto HeldAsSettled(
    const slang::ast::ParameterSymbol& param, const ReadsOfParameters& reads)
    -> bool {
  return DeclaredInAClass(param) ||
         reads.read_in_a_nested_element.contains(&param);
}

}  // namespace

// What a body's text says of its parameters is one answer for every instance
// that body serves; what an instance adds is which of them it was given a
// value for, and whether anything builds it. So the text is asked of the body
// the instance is read through, and the rest of the instance itself, a
// parameter of the one standing for the parameter of the other declared at the
// same place.
auto Specializations::ParametersOf(const slang::ast::InstanceSymbol& inst) const
    -> const ParametersOfInstance& {
  if (const auto kept = per_instance_.find(&inst);
      kept != per_instance_.end()) {
    return kept->second;
  }
  const slang::ast::InstanceBodySymbol& read_through =
      HasBodyOfItsOwn(inst) ? inst.body : *inst.getCanonicalBody();
  const ParameterSet own = ValueParametersOf(read_through);
  ReadsOfParameters reads(own);
  read_through.visit(reads);

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
  const auto as_handed = [&](const slang::ast::ParameterSymbol& param) {
    const auto its = of_instance.find(&param);
    return its == of_instance.end() ? &param : its->second;
  };
  // A value given anywhere is one value the instance is handed rather than one
  // its declaration writes.
  const auto given_a_value = [&](const slang::ast::ParameterSymbol& param) {
    const auto its = of_instance.find(&param);
    return its == of_instance.end() ? GivenAValue(read_through, param)
                                    : GivenAValue(inst.body, *its->second);
  };

  // What the code reads, and what each such parameter's own declaration reads
  // in turn where the object works its value out.
  ParameterSet needed;
  std::vector<const slang::ast::ParameterSymbol*> pending(
      reads.read.begin(), reads.read.end());
  while (!pending.empty()) {
    const slang::ast::ParameterSymbol* param = pending.back();
    pending.pop_back();
    if (!needed.insert(param).second) continue;
    if (CanBeSupplied(*param) || param->isFromGenvar() ||
        given_a_value(*param)) {
      continue;
    }
    for (const slang::ast::ParameterSymbol* read :
         InitializerReads(*param, own)) {
      pending.push_back(read);
    }
  }

  // A top-level instance is built by the design root, which hands nothing, and
  // is the only instance of its specialization, so a value it is given is
  // compiled in.
  ParametersOfInstance out;
  const auto supplied_here = [&](const slang::ast::ParameterSymbol& param) {
    return param.isFromGenvar() ||
           (CanBeSupplied(param) && !inst.isTopLevel() &&
            needed.contains(&param) && !HeldAsSettled(param, reads));
  };
  for (const slang::ast::ParameterSymbolBase* base : declared) {
    const auto* param = base->symbol.as_if<slang::ast::ParameterSymbol>();
    if (param != nullptr && !param->isFromGenvar() && supplied_here(*param)) {
      out.supplied.push_back(as_handed(*param));
    }
  }
  // A parameter whose declaration writes it from a value an object is handed
  // is worked out by each object, and one written from no such value is the
  // constant it was settled to. Found by spreading from each handed value to
  // the declarations reading it, and from those to the ones reading them.
  std::unordered_map<
      const slang::ast::ParameterSymbol*,
      std::vector<const slang::ast::ParameterSymbol*>>
      read_by;
  std::vector<const slang::ast::ParameterSymbol*> spreading;
  for (const slang::ast::ParameterSymbol* param : own) {
    if (supplied_here(*param)) spreading.push_back(param);
    if (CanBeSupplied(*param) || given_a_value(*param) ||
        HeldAsSettled(*param, reads)) {
      continue;
    }
    for (const slang::ast::ParameterSymbol* read :
         InitializerReads(*param, own)) {
      read_by[read].push_back(param);
    }
  }
  ParameterSet varying;
  while (!spreading.empty()) {
    const slang::ast::ParameterSymbol* param = spreading.back();
    spreading.pop_back();
    if (!varying.insert(param).second) continue;
    if (const auto readers = read_by.find(param); readers != read_by.end()) {
      spreading.insert(
          spreading.end(), readers->second.begin(), readers->second.end());
    }
  }
  for (const slang::ast::ParameterSymbol* param : own) {
    ParameterValueSource source = ParameterValueSource::kSettledAtElaboration;
    if (supplied_here(*param)) {
      source = ParameterValueSource::kSuppliedAtConstruction;
    } else if (needed.contains(param) && varying.contains(param)) {
      source = ParameterValueSource::kComputedAtConstruction;
    }
    out.sources.emplace(as_handed(*param), source);
  }

  return per_instance_.emplace(&inst, std::move(out)).first->second;
}

auto Specializations::SuppliedParametersOf(
    const slang::ast::InstanceSymbol& inst) const
    -> std::span<const slang::ast::ParameterSymbol* const> {
  return ParametersOf(inst).supplied;
}

auto Specializations::ValueSourceOf(
    const slang::ast::InstanceSymbol& inst,
    const slang::ast::ParameterSymbol& param) const -> ParameterValueSource {
  const ParametersOfInstance& known = ParametersOf(inst);
  const auto it = known.sources.find(&param);
  return it == known.sources.end() ? ParameterValueSource::kSettledAtElaboration
                                   : it->second;
}

}  // namespace lyra::lowering::ast_to_hir
