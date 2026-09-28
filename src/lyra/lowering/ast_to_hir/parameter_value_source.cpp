// Which of an instance's value parameters are supplied at construction, which
// its declarations compute from those, and which decide what is compiled.
//
// The answer is read off the source and never guessed from a list of what
// decides a class: a list of that kind belongs to SystemVerilog and has no end.
// Instead every reference to a parameter is found, and a parameter is supplied
// only where every one of them sits somewhere this compiler lowers as an
// expression the run evaluates -- a statement, a continuous assignment, a
// variable's initializer -- and never as one whose value fixes a type. Anything
// else denies, including a position nobody thought of, which costs sharing and
// nothing else.
//
// Some positions hold a constant the front end settled while binding, so a
// parameter's value sits there with no reference left in the expression: a
// declared range, including one on a named type, a struct or union member, an
// enum's base, or a type written as an operand; an instance or port array's
// range; a size cast's width, a hierarchical name's index or range, a stream's
// slice size, a sequence's delay or repetition, a queue's bound; the values a
// class specialization or a virtual interface is handed, which pick one shared
// with every other site handing the same; the operand a type is taken from
// (LRM 6.23); the type a cast or an assignment pattern writes, and a pattern's
// type key; the path a defparam names its target through.
// Beside each the front end keeps the expression it settled it from, and the
// walk reads that like any other reference. Anything still unseen is caught
// where it matters: the lowering compares every instance handed a different
// value against the unit it shares, and a difference keeps the definition
// whole. So this answers how much is shared and never whether the program is
// right.

#include <algorithm>
#include <span>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include <slang/ast/ASTVisitor.h>
#include <slang/ast/EvaluatedDimension.h>
#include <slang/ast/Expression.h>
#include <slang/ast/HierarchicalReference.h>
#include <slang/ast/Scope.h>
#include <slang/ast/expressions/AssertionExpr.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/CallExpression.h>
#include <slang/ast/expressions/ConversionExpression.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/expressions/OperatorExpressions.h>
#include <slang/ast/expressions/SelectExpressions.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/ClassSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/MemberSymbols.h>
#include <slang/ast/symbols/ParameterSymbols.h>
#include <slang/ast/symbols/PortSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>
#include <slang/ast/types/DeclaredType.h>
#include <slang/syntax/AllSyntax.h>

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

// Records a reference to one of `own` parameters.
void Record(
    const slang::ast::ValueExpressionBase& e, const ParameterSet& own,
    References& into) {
  const auto* param = e.symbol.as_if<slang::ast::ParameterSymbol>();
  if (param != nullptr && own.contains(param)) {
    into[param].insert(&e);
  }
}

// Every reference the body makes to its own parameters, wherever it is --
// except inside a child instance's body, whose references are to its own
// parameters. What a child is handed is an expression of this body and is
// collected here.
struct EveryReference
    : slang::ast::ASTVisitor<EveryReference, slang::ast::VisitFlags::AllGood> {
  explicit EveryReference(const ParameterSet& own) : own(&own) {
  }

  const ParameterSet* own;
  References found;

  void handle(const slang::ast::NamedValueExpression& e) {
    Record(e, *own, found);
    VisitSpecializationParameters(e.specializationParameters);
    visitDefault(e);
  }

  // Its declared type and its initializer are both places a value may reach.
  void handle(const slang::ast::ParameterSymbol& param) {
    VisitFoldedDimensions(param);
    VisitSettledFrom(param.getInitializer());
  }

  // What the child is handed, never what its body reads.
  void handle(const slang::ast::InstanceSymbol& child) {
    child.visitExprs(*this);
    for (const auto* param : child.body.getParameters()) {
      const auto* value = param->symbol.as_if<slang::ast::ParameterSymbol>();
      if (value != nullptr && value->isOverridden()) {
        if (const slang::ast::Expression* given = value->getInitializer()) {
          given->visit(*this);
        }
      }
    }
  }

  void handle(const slang::ast::GenerateBlockSymbol& block) {
    for (const slang::ast::GenerateSelection& level : block.selectionPath) {
      if (level.condition != nullptr) level.condition->visit(*this);
      for (const slang::ast::Expression* item : level.caseItems) {
        if (item != nullptr) item->visit(*this);
      }
    }
    visitDefault(block);
  }

  void handle(const slang::ast::GenerateBlockArraySymbol& array) {
    for (const slang::ast::Expression* header :
         {array.initialExpression, array.stopExpression,
          array.iterExpression}) {
      if (header != nullptr) header->visit(*this);
    }
    visitDefault(array);
  }

  template <typename T>
    requires std::is_base_of_v<slang::ast::Symbol, T>
  void handle(const T& symbol) {
    VisitFoldedDimensions(symbol);
    visitDefault(symbol);
  }

  // The constants the front end settled while binding, each read through the
  // expression it was settled from.
  void handle(const slang::ast::ConversionExpression& e) {
    VisitSettledFrom(e.targetExpr);
    visitDefault(e);
  }

  void handle(const slang::ast::SimpleAssignmentPatternExpression& e) {
    VisitSettledFrom(e.typeExpr);
    visitDefault(e);
  }

  void handle(const slang::ast::StructuredAssignmentPatternExpression& e) {
    VisitSettledFrom(e.typeExpr);
    for (const auto& setter : e.typeSetters) {
      VisitSettledFrom(setter.key);
    }
    visitDefault(e);
  }

  void handle(const slang::ast::ReplicatedAssignmentPatternExpression& e) {
    VisitSettledFrom(e.typeExpr);
    visitDefault(e);
  }

  void handle(const slang::ast::TypeReferenceExpression& e) {
    e.operand.visit(*this);
  }

  // Which instance a defparam reaches, and the value it gives there, both
  // decide what that instance is (LRM 23.10.1).
  void handle(const slang::ast::DefParamSymbol& defparam) {
    VisitPath(defparam.getTargetPath());
    VisitSettledFrom(&defparam.getInitializer());
  }

  void handle(const slang::ast::StreamingConcatenationExpression& e) {
    VisitSettledFrom(e.sliceSizeExpr);
    visitDefault(e);
  }

  // A parameter reached through a hierarchical name is lowered to the value
  // one elaboration gave it rather than to what its instance is handed, so
  // such a reference is one no evaluated place accepts.
  void handle(const slang::ast::HierarchicalValueExpression& e) {
    Record(e, *own, found);
    VisitSelectors(e.ref);
    VisitSpecializationParameters(e.specializationParameters);
    visitDefault(e);
  }

  void handle(const slang::ast::ArbitrarySymbolExpression& e) {
    VisitSelectors(e.hierRef);
    visitDefault(e);
  }

  void handle(const slang::ast::CallExpression& e) {
    VisitSelectors(e.lookupInfo.hierRef);
    VisitSpecializationParameters(e.lookupInfo.specializationParameters);
    visitDefault(e);
  }

  void handle(const slang::ast::NewClassExpression& e) {
    VisitSpecializationParameters(e.specializationParameters);
    visitDefault(e);
  }

  // A class declared here names its base and interfaces in its header.
  void handle(const slang::ast::ClassType& c) {
    VisitSpecializationParameters(c.getHeaderSpecializationParameters());
    visitDefault(c);
  }

  void handle(const slang::ast::SimpleAssertionExpr& e) {
    if (e.repetition) VisitRange(e.repetition->range);
    visitDefault(e);
  }

  void handle(const slang::ast::SequenceWithMatchExpr& e) {
    if (e.repetition) VisitRange(e.repetition->range);
    visitDefault(e);
  }

  void handle(const slang::ast::SequenceConcatExpr& e) {
    for (const slang::ast::SequenceConcatExpr::Element& element : e.elements) {
      VisitRange(element.delay);
    }
    visitDefault(e);
  }

  void handle(const slang::ast::UnaryAssertionExpr& e) {
    if (e.range) VisitRange(*e.range);
    visitDefault(e);
  }

  // What an instance array or an interface-port array is sized by.
  void handle(const slang::ast::InstanceArraySymbol& array) {
    VisitSettledFrom(array.leftExpr);
    VisitSettledFrom(array.rightExpr);
    visitDefault(array);
  }

  void handle(const slang::ast::InterfacePortSymbol& port) {
    if (const auto dims = port.getDeclaredDimensions()) {
      VisitDimensions(*dims);
    }
    visitDefault(port);
  }

  void handle(const slang::ast::DataTypeExpression& e) {
    VisitDimensions(e.dimensions);
    VisitSpecializationParameters(e.specializationParameters);
    VisitTypeReferences(e.typeReferences);
    VisitWrittenType(*e.type);
  }

  // A type the front end folded holds a parameter's value with no reference
  // left in it, so the bound expressions it was folded from are asked for.
  void VisitFoldedDimensions(const slang::ast::Symbol& symbol) {
    const slang::ast::DeclaredType* declared = symbol.getDeclaredType();
    if (declared == nullptr) return;
    VisitDimensions(declared->getResolvedDimensions());
    VisitSpecializationParameters(
        declared->getResolvedSpecializationParameters());
    VisitTypeReferences(declared->getResolvedTypeReferences());
    VisitWrittenType(declared->getType());
  }

  // A type taken from an expression keeps only the expression's type, so
  // whatever decides that type is read from the operand (LRM 6.23).
  void VisitTypeReferences(
      std::span<const slang::ast::Expression* const> operands) {
    for (const slang::ast::Expression* operand : operands) {
      VisitSettledFrom(operand);
    }
  }

  // A class specialization or a virtual interface is shared by every site
  // handing it the same values, so what decides it is read from the
  // parameters this site wrote: a value's expression, a type's declaration.
  void VisitSpecializationParameters(
      std::span<const slang::ast::Symbol* const> params) {
    for (const slang::ast::Symbol* param : params) {
      if (const auto* value = param->as_if<slang::ast::ParameterSymbol>()) {
        if (value->isOverridden()) VisitSettledFrom(value->getInitializer());
      } else if (
          const auto* type = param->as_if<slang::ast::TypeParameterSymbol>()) {
        if (type->isOverridden()) VisitFoldedDimensions(*type);
      }
    }
  }

  void VisitDimensions(std::span<const slang::ast::EvaluatedDimension> dims) {
    for (const slang::ast::EvaluatedDimension& dim : dims) {
      VisitSettledFrom(dim.leftExpr);
      VisitSettledFrom(dim.rightExpr);
      VisitSettledFrom(dim.queueMaxSizeExpr);
      VisitSettledFrom(dim.associativeTypeExpr);
    }
  }

  // A struct, union or enum written out where a type is declared states its
  // members' ranges and its base range there as well, and a virtual interface
  // the values it hands its interface, so those are read with it. One named
  // through an alias belongs to the declaration of that alias, which is read
  // where it stands.
  void VisitWrittenType(const slang::ast::Type& type) {
    const slang::ast::Type* written = &type;
    while (!written->isAlias() && written->getArrayElementType() != nullptr) {
      written = written->getArrayElementType();
    }
    const auto fields = [&](const slang::ast::Scope& members) {
      for (const auto& field :
           members.membersOfType<slang::ast::FieldSymbol>()) {
        VisitFoldedDimensions(field);
      }
    };
    if (const auto* e = written->as_if<slang::ast::EnumType>()) {
      VisitDimensions(e->baseDimensions);
    } else if (
        const auto* vif = written->as_if<slang::ast::VirtualInterfaceType>()) {
      VisitSpecializationParameters(vif->specializationParameters);
    } else if (const auto* s = written->as_if<slang::ast::PackedStructType>()) {
      fields(*s);
    } else if (const auto* u = written->as_if<slang::ast::PackedUnionType>()) {
      fields(*u);
    } else if (
        const auto* us = written->as_if<slang::ast::UnpackedStructType>()) {
      fields(*us);
    } else if (
        const auto* uu = written->as_if<slang::ast::UnpackedUnionType>()) {
      fields(*uu);
    }
  }

  // Which element each step of a hierarchical name reached was settled from
  // the index or range the source wrote there (LRM 23.6).
  void VisitSelectors(const slang::ast::HierarchicalReference& ref) {
    VisitPath(ref.path);
  }

  void VisitPath(
      std::span<const slang::ast::HierarchicalReference::Element> path) {
    for (const slang::ast::HierarchicalReference::Element& step : path) {
      VisitSettledFrom(step.leftExpr);
      VisitSettledFrom(step.rightExpr);
    }
  }

  void VisitRange(const slang::ast::SequenceRange& range) {
    VisitSettledFrom(range.minExpr);
    VisitSettledFrom(range.maxExpr);
  }

  void VisitSettledFrom(const slang::ast::Expression* expr) {
    if (expr != nullptr) expr->visit(*this);
  }
};

// The references that sit where this compiler lowers the expression the source
// wrote and the run evaluates it, and where no type depends on the value: an
// operand of a statement, of a continuous assignment, of a variable's, a net's
// or a parameter's initializer, of a port's default, or of what a child is
// handed. Inside those, an operand whose value fixes the width of what its
// expression produces is left out, because the type the front end gave the
// expression already holds that value.
struct ValueReference
    : slang::ast::ASTVisitor<ValueReference, slang::ast::VisitFlags::AllGood> {
  explicit ValueReference(const ParameterSet& own) : own(&own) {
  }

  const ParameterSet* own;
  References found;

  void handle(const slang::ast::NamedValueExpression& e) {
    Record(e, *own, found);
  }

  void handle(const slang::ast::RangeSelectExpression& e) {
    e.value().visit(*this);
    if (e.getSelectionKind() != slang::ast::RangeSelectionKind::Simple) {
      e.left().visit(*this);
    }
  }

  void handle(const slang::ast::ReplicationExpression& e) {
    e.concat().visit(*this);
  }
};

// Collects the value references in the places a run evaluates, walking the
// body's members and skipping everything that is not one of those places.
struct EvaluatedPlaces
    : slang::ast::ASTVisitor<EvaluatedPlaces, slang::ast::VisitFlags::Symbols> {
  EvaluatedPlaces(ValueReference& values, const SpecializationPolicy& policy)
      : values(&values), policy(&policy) {
  }

  ValueReference* values;
  const SpecializationPolicy* policy;

  void handle(const slang::ast::ProceduralBlockSymbol& b) const {
    b.getBody().visit(*values);
  }

  void handle(const slang::ast::ContinuousAssignSymbol& a) const {
    a.getAssignment().visit(*values);
  }

  void handle(const slang::ast::SubroutineSymbol& s) {
    s.getBody().visit(*values);
    visitDefault(s);
  }

  // A parameter written from a value that varies is computed when the object
  // holding it is built, wherever it is declared; one that does not vary is
  // folded, and then nothing it reads is supplied to the instance.
  void handle(const slang::ast::ParameterSymbol& p) const {
    if (const slang::ast::Expression* init = p.getInitializer()) {
      init->visit(*values);
    }
  }

  void handle(const slang::ast::VariableSymbol& v) const {
    if (const slang::ast::Expression* init = v.getInitializer()) {
      init->visit(*values);
    }
  }

  void handle(const slang::ast::NetSymbol& n) const {
    if (const slang::ast::Expression* init = n.getInitializer()) {
      init->visit(*values);
    }
  }

  // A port's default is evaluated in this unit when an instance leaves the
  // port unconnected (LRM 23.2.2.4).
  void handle(const slang::ast::PortSymbol& p) const {
    if (const slang::ast::Expression* init = p.getInitializer()) {
      init->visit(*values);
    }
  }

  // What a child is handed is a value this body states, and whether that is a
  // place the run evaluates is the child's own answer about that parameter.
  // The child's body reads its own parameters and is not walked.
  void handle(const slang::ast::InstanceSymbol& child) const {
    for (const slang::ast::ParameterSymbol* param :
         policy->SuppliedParametersOf(child)) {
      if (const slang::ast::Expression* given = param->getInitializer()) {
        given->visit(*values);
      }
    }
  }
};

// The parameters `param`'s own initializer names.
auto DependenciesOf(
    const slang::ast::ParameterSymbol& param, const ParameterSet& own)
    -> ParameterSet {
  EveryReference refs(own);
  if (const slang::ast::Expression* init = param.getInitializer()) {
    init->visit(refs);
  }
  ParameterSet out;
  for (const auto& [found, uses] : refs.found) {
    out.insert(found);
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

// Whether a hierarchical override (LRM 23.10.1) or a command-line one reaches
// `param`. Either takes precedence over what the instantiation wrote, so the
// value the parent would hand over is not the value the parameter holds.
auto OverriddenFromElsewhere(
    const slang::ast::InstanceBodySymbol& body,
    const slang::ast::ParameterSymbol& param) -> bool {
  if (body.hierarchyOverrideNode == nullptr) {
    return false;
  }
  const slang::syntax::SyntaxNode* syntax = param.getSyntax();
  return syntax != nullptr &&
         body.hierarchyOverrideNode->paramOverrides.contains(syntax);
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

// The parameters of `body` its instance is handed when it is built, in the
// order the body declares them: every one the instantiation overrides that
// decides nothing about what is compiled.
auto SuppliedOf(
    const slang::ast::InstanceBodySymbol& body, const ParameterSet& own,
    const ParameterEdges& written_from, const SpecializationPolicy& policy)
    -> std::vector<const slang::ast::ParameterSymbol*> {
  EveryReference every(own);
  body.visit(every);

  ValueReference values(own);
  EvaluatedPlaces places(values, policy);
  body.visit(places);

  // Whether some reference to `param` sits outside the places the run
  // evaluates.
  const auto read_elsewhere = [&](const slang::ast::ParameterSymbol* param) {
    const auto all = every.found.find(param);
    if (all == every.found.end()) return false;
    const auto evaluated = values.found.find(param);
    return std::ranges::any_of(all->second, [&](const auto* use) {
      return evaluated == values.found.end() ||
             !evaluated->second.contains(use);
    });
  };
  // A parameter decides what is compiled where it is read outside those
  // places, where its type is the type of its value, or where an override from
  // elsewhere replaces what the instantiation hands it; and what a deciding
  // parameter is written from decides with it.
  ParameterSet roots;
  for (const slang::ast::ParameterSymbol* param : own) {
    if (read_elsewhere(param) || TypeFollowsValue(*param) ||
        OverriddenFromElsewhere(body, *param)) {
      roots.insert(param);
    }
  }
  const ParameterSet decides =
      Reached(std::move(roots), written_from, [](const auto&) { return true; });

  std::vector<const slang::ast::ParameterSymbol*> supplied;
  for (const auto& member : body.members()) {
    const auto* param = member.as_if<slang::ast::ParameterSymbol>();
    if (param == nullptr || param->isLocalParam() || !param->isOverridden() ||
        decides.contains(param)) {
      continue;
    }
    supplied.push_back(param);
  }
  return supplied;
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

auto SpecializationPolicy::Classify(
    const slang::ast::InstanceSymbol& inst) const -> PerInstance {
  PerInstance out;
  const slang::ast::InstanceBodySymbol& body = inst.body;
  const ParameterSet own = ValueParametersOf(body);
  ParameterEdges written_from;
  ParameterEdges written_into;
  for (const slang::ast::ParameterSymbol* param : own) {
    written_from.emplace(param, DependenciesOf(*param, own));
    written_into.emplace(param, ParameterSet{});
  }
  for (const auto& [param, sources] : written_from) {
    for (const slang::ast::ParameterSymbol* source : sources) {
      written_into.at(source).insert(param);
    }
  }
  if (!kept_whole_.contains(&inst.getDefinition())) {
    out.supplied = SuppliedOf(body, own, written_from, *this);
  }

  // What varies to begin with is what the unit is handed and what a generate
  // block declares, since a block is built with its own. What a declaration
  // writes from a value that varies varies with it, and so on; one whose value
  // comes from elsewhere is not written by its declaration.
  ParameterSet roots(out.supplied.begin(), out.supplied.end());
  for (const slang::ast::ParameterSymbol* param : own) {
    const slang::ast::Scope* declaring = param->getParentScope();
    if (declaring != nullptr &&
        declaring->asSymbol().kind == slang::ast::SymbolKind::GenerateBlock) {
      roots.insert(param);
    }
  }
  out.varying = Reached(
      std::move(roots), written_into,
      [&](const slang::ast::ParameterSymbol& param) {
        return !param.isOverridden() && !OverriddenFromElsewhere(body, param);
      });
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

}  // namespace lyra::lowering::ast_to_hir
