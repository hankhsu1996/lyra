// Which of an instance's value parameters are supplied at construction, which
// its declarations compute from those, and which decide what is compiled.
//
// A parameter read where the source wrote an expression is lowered as that
// expression, and the object reads the value it was built with. So a parameter
// is supplied unless one of its references sits where the front end settled a
// constant while binding, since what is compiled then holds the constant and
// not the expression: a declared range, including one on a named type, a
// struct or union member, an enum's base, or a type written as an operand; an
// instance or port array's range; the bounds a part select's width comes from
// and a replication's count; a size cast's width, a hierarchical name's index
// or range, a stream's slice size, a sequence's delay or repetition, a queue's
// bound; the values a class specialization or a virtual interface is handed,
// which pick one shared with every other site handing the same; the operand a
// type is taken from (LRM 6.23); the type a cast or an assignment pattern
// writes, and a pattern's type key; the path a defparam names its target
// through; and what a child is handed for a parameter that decides there.
// Beside each the front end keeps the expression it settled the constant from,
// and the walk reads the parameters that expression names.
//
// What is listed is where the front end folds, which is a fact about the front
// end and moves only with it. A place it folds that the walk does not know is
// caught where it matters: the lowering holds every instance handed a
// different value against the unit it shares, and the classes the block
// instances of one generate block published against each other, and a
// difference keeps the definition whole. So this answers how much is shared
// and never whether the program is right.

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

#include "lyra/base/internal_error.hpp"
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

// Every reference to the body's own parameters, wherever it is, and which of
// them sit where the value decides what is compiled. A child instance's body
// names its own parameters, so what is collected of a child is what it is
// handed, which is an expression of this body.
struct EveryReference
    : slang::ast::ASTVisitor<EveryReference, slang::ast::VisitFlags::AllGood> {
  EveryReference(const ParameterSet& own, const SpecializationPolicy& policy)
      : own(&own), policy(&policy) {
  }

  const ParameterSet* own;
  const SpecializationPolicy* policy;
  References found;
  // The references whose value what is compiled holds: one inside an
  // expression the front end settled a constant from, one a design element
  // declared inside this one writes, and one written through a hierarchical
  // name.
  std::unordered_set<const slang::ast::Expression*> deciding;
  // Each reference in the order the walk met it, with the expression the front
  // end settled a constant from that it sits in, or nothing where it sits in
  // none. That expression is the whole of what the reference decides there:
  // two values it folds alike leave the same constant behind.
  struct Use {
    const slang::ast::ParameterSymbol* param;
    const slang::ast::Expression* reference;
    const slang::ast::Expression* settled_from;
  };
  std::vector<Use> uses;
  // The references in a generate construct's header, which a construction
  // evaluates to decide what it builds: what selects an alternative of a
  // conditional generate, and what counts a loop generate's blocks out.
  std::unordered_set<const slang::ast::Expression*> in_generate_headers;
  const slang::ast::Expression* settling = nullptr;
  bool in_a_nested_element = false;
  bool in_a_generate_header = false;

  void Note(const slang::ast::ValueExpressionBase& e, bool through_a_path) {
    const auto* param = e.symbol.as_if<slang::ast::ParameterSymbol>();
    if (param == nullptr || !own->contains(param)) return;
    if (in_a_generate_header) in_generate_headers.insert(&e);
    const bool decides =
        settling != nullptr || in_a_nested_element || through_a_path;
    const bool first_met = found[param].insert(&e).second;
    const bool first_deciding = decides && deciding.insert(&e).second;
    if (first_met || first_deciding) {
      uses.push_back(
          Use{.param = param, .reference = &e, .settled_from = settling});
    }
  }

  void handle(const slang::ast::NamedValueExpression& e) {
    Note(e, false);
    VisitSpecializationParameters(e.specializationParameters);
    visitDefault(e);
  }

  // Its declared type is settled; its initializer is an expression the object
  // computes when it is built, where what it is written from varies.
  void handle(const slang::ast::ParameterSymbol& param) {
    VisitFoldedDimensions(param);
    if (const slang::ast::Expression* init = param.getInitializer()) {
      init->visit(*this);
    }
  }

  // What this body hands the child, and not what a child's body reads of its
  // own parameters. A value handed for a parameter the child is supplied is an
  // expression this body evaluates when it builds the child, and one handed
  // for any other is settled into which unit the child is. A design element
  // declared inside another reads the enclosing one's parameters by name (LRM
  // 23.4, 23.9), and its instance is handed none of them when it is built, so
  // what its body reads of them is the value this elaboration gave.
  void handle(const slang::ast::InstanceSymbol& child) {
    child.visitExprs(*this);
    const auto supplied = policy->SuppliedParametersOf(child);
    for (const auto* param : child.body.getParameters()) {
      const auto* value = param->symbol.as_if<slang::ast::ParameterSymbol>();
      if (value == nullptr) continue;
      const slang::ast::Expression* given =
          ValueWrittenAtInstantiation(child, *value);
      if (given == nullptr) continue;
      if (std::ranges::contains(supplied, value)) {
        given->visit(*this);
      } else {
        VisitSettledFrom(given);
      }
    }
    if (!IsLibraryCell(child.getDefinition())) {
      const bool outer = in_a_nested_element;
      in_a_nested_element = true;
      child.body.visit(*this);
      in_a_nested_element = outer;
    }
  }

  // A part select's width is the type of what it yields: both bounds of a
  // constant range, and the width of an indexed one, whose base is a position
  // the run computes (LRM 11.5.1).
  void handle(const slang::ast::RangeSelectExpression& e) {
    e.value().visit(*this);
    if (e.getSelectionKind() == slang::ast::RangeSelectionKind::Simple) {
      VisitSettledFrom(&e.left());
    } else {
      e.left().visit(*this);
    }
    VisitSettledFrom(&e.right());
  }

  // A replication's count is the width of what it yields (LRM 11.4.12.1).
  void handle(const slang::ast::ReplicationExpression& e) {
    VisitSettledFrom(&e.count());
    e.concat().visit(*this);
  }

  // The condition or the case items an alternative stands under (LRM 27.5).
  void handle(const slang::ast::GenerateBlockSymbol& block) {
    for (const slang::ast::GenerateSelection& level : block.selectionPath) {
      VisitGenerateHeader(level.condition);
      for (const slang::ast::Expression* item : level.caseItems) {
        VisitGenerateHeader(item);
      }
    }
    visitDefault(block);
  }

  // Where a loop's index starts, whether a block stands at it, and how it
  // reaches the next one (LRM 27.4).
  void handle(const slang::ast::GenerateBlockArraySymbol& array) {
    for (const slang::ast::Expression* header :
         {array.initialExpression, array.stopExpression,
          array.iterExpression}) {
      VisitGenerateHeader(header);
    }
    visitDefault(array);
  }

  void VisitGenerateHeader(const slang::ast::Expression* expr) {
    if (expr == nullptr) return;
    const bool outer = in_a_generate_header;
    in_a_generate_header = true;
    expr->visit(*this);
    in_a_generate_header = outer;
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
  // one elaboration gave it rather than to what its instance is handed.
  void handle(const slang::ast::HierarchicalValueExpression& e) {
    Note(e, true);
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
    if (expr == nullptr) return;
    const slang::ast::Expression* outer = settling;
    if (outer == nullptr) settling = expr;
    expr->visit(*this);
    settling = outer;
  }
};

// Collects into `code` every reference in the code of a process, a continuous
// assignment or a subroutine `scope` holds, wherever in it the reference sits.
// A variable of automatic lifetime exists only while its body runs (LRM 6.21),
// so what it is declared as is part of that body's code. A child instance is
// no scope of this one, so its body, which is none of this body's code, is not
// reached.
void CollectCodeOf(const slang::ast::Scope& scope, EveryReference& code) {
  for (const auto& member : scope.members()) {
    if (const auto* process =
            member.as_if<slang::ast::ProceduralBlockSymbol>()) {
      process->getBody().visit(code);
    } else if (
        const auto* assign =
            member.as_if<slang::ast::ContinuousAssignSymbol>()) {
      assign->getAssignment().visit(code);
    } else if (const auto* variable =
                   member.as_if<slang::ast::VariableSymbol>();
               variable != nullptr &&
               variable->kind == slang::ast::SymbolKind::Variable &&
               variable->lifetime == slang::ast::VariableLifetime::Automatic) {
      variable->visit(code);
    }
    if (const auto* subroutine = member.as_if<slang::ast::SubroutineSymbol>()) {
      subroutine->getBody().visit(code);
    }
    if (const auto* inner = member.as_if<slang::ast::Scope>()) {
      CollectCodeOf(*inner, code);
    }
  }
}

// The parameters `param`'s own initializer names.
auto DependenciesOf(
    const slang::ast::ParameterSymbol& param, const ParameterSet& own,
    const SpecializationPolicy& policy) -> ParameterSet {
  EveryReference refs(own, policy);
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

// Which of a body's parameters are asked what they decide. Where only those a
// generate block declares are, nothing the unit declares decides, which is what
// a unit handed every value is lowered under.
enum class AskedWhatTheyDecide : std::uint8_t {
  kEveryParameter,
  kOnlyWhatBlocksDeclare,
};

// What a unit holds of the blocks its instances build. One lowered from every
// instance that builds something of it holds what each built. One that could
// only be lowered from a single instance -- a class a design element holds is
// declared once per unit, from one body -- holds what that instance built.
enum class UnitHolds : std::uint8_t {
  kWhatEveryInstanceBuilt,
  kWhatOneInstanceBuilt,
};

// What decides for `body`: a parameter one of whose references sits where the
// value is held by what is compiled, or whose type is the type of its value;
// and what a deciding parameter is written from decides with it.
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
    const ParameterEdges& written_from, const SpecializationPolicy& policy,
    AskedWhatTheyDecide asked, UnitHolds holds) -> Deciding {
  EveryReference every(own, policy);
  body.visit(every);

  EveryReference code(own, policy);
  CollectCodeOf(body, code);

  const auto in_code = [&](const slang::ast::ParameterSymbol* param,
                           const slang::ast::Expression* use) {
    const auto uses = code.found.find(param);
    return uses != code.found.end() && uses->second.contains(use);
  };
  // A unit holding what one instance built and no other is named by what that
  // instance's construction read to decide it.
  const auto decides_there = [&](const slang::ast::ParameterSymbol* param,
                                 const slang::ast::Expression* use) {
    if (DeclaredByAGenerateBlock(*param)) {
      return every.deciding.contains(use) && !in_code(param, use);
    }
    switch (holds) {
      case UnitHolds::kWhatEveryInstanceBuilt:
        return every.deciding.contains(use);
      case UnitHolds::kWhatOneInstanceBuilt:
        return every.deciding.contains(use) ||
               every.in_generate_headers.contains(use);
    }
    throw InternalError("DecidingParametersOf: unknown UnitHolds");
  };
  const auto decides_somewhere = [&](const slang::ast::ParameterSymbol* param) {
    const auto all = every.found.find(param);
    if (all == every.found.end()) return false;
    return std::ranges::any_of(all->second, [&](const auto* use) {
      return decides_there(param, use);
    });
  };
  const auto is_asked = [&](const slang::ast::ParameterSymbol& param) {
    switch (asked) {
      case AskedWhatTheyDecide::kEveryParameter:
        return true;
      case AskedWhatTheyDecide::kOnlyWhatBlocksDeclare:
        return DeclaredByAGenerateBlock(param);
    }
    throw InternalError("DecidingParametersOf: unknown AskedWhatTheyDecide");
  };
  ParameterSet roots;
  for (const slang::ast::ParameterSymbol* param : own) {
    if (!is_asked(*param)) continue;
    if (decides_somewhere(param) || TypeFollowsValue(*param)) {
      roots.insert(param);
    }
  }
  Deciding deciding{
      .parameters = Reached(
          std::move(roots), written_from, [](const auto&) { return true; }),
      .folded_to = {}};

  ParameterSet by_own_value;
  for (const slang::ast::ParameterSymbol* param : deciding.parameters) {
    for (const slang::ast::ParameterSymbol* source : written_from.at(param)) {
      if (source->isFromGenvar()) by_own_value.insert(source);
    }
  }
  for (const EveryReference::Use& use : every.uses) {
    if (!use.param->isFromGenvar() ||
        !deciding.parameters.contains(use.param) ||
        !decides_there(use.param, use.reference)) {
      continue;
    }
    const slang::ConstantValue* settled =
        use.settled_from == nullptr ? nullptr : use.settled_from->getConstant();
    if (settled == nullptr) {
      by_own_value.insert(use.param);
    } else {
      deciding.folded_to[use.param].push_back(ValueIdentity(*settled));
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
    written_from.emplace(param, DependenciesOf(*param, out.own, *this));
    out.written_into.emplace(param, ParameterSet{});
  }
  for (const auto& [param, sources] : written_from) {
    for (const slang::ast::ParameterSymbol* source : sources) {
      out.written_into.at(source).insert(param);
    }
  }
  // A definition kept whole shares nothing, so every value decides, and every
  // value of a loop's index is told apart from every other.
  if (kept_whole_.contains(&body.getDefinition())) {
    out.deciding = out.own;
    for (const slang::ast::ParameterSymbol* param : out.own) {
      if (param->isFromGenvar()) {
        out.folded_to[param] = {ValueIdentity(param->getValue())};
      }
    }
  } else {
    Deciding deciding = DecidingParametersOf(
        body, out.own, written_from, *this,
        handed_every_value_.contains(&body.getDefinition())
            ? AskedWhatTheyDecide::kOnlyWhatBlocksDeclare
            : AskedWhatTheyDecide::kEveryParameter,
        built_by_one_instance_.contains(&body.getDefinition())
            ? UnitHolds::kWhatOneInstanceBuilt
            : UnitHolds::kWhatEveryInstanceBuilt);
    out.deciding = std::move(deciding.parameters);
    out.folded_to = std::move(deciding.folded_to);
  }
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
  // value it was given that decides nothing about what is compiled.
  ParameterSet roots;
  if (!inst.isTopLevel()) {
    for (const slang::ast::ParameterSymbolBase* base : declared) {
      const auto* param = base->symbol.as_if<slang::ast::ParameterSymbol>();
      if (param == nullptr || param->isLocalParam() || !given_a_value(*param) ||
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
