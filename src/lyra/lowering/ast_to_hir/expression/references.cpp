#include "lyra/lowering/ast_to_hir/expression/references.hpp"

#include <expected>
#include <optional>
#include <string>
#include <utility>
#include <variant>

#include <slang/ast/Expression.h>
#include <slang/ast/HierarchicalReference.h>
#include <slang/ast/Scope.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/symbols/ClassSymbols.h>
#include <slang/ast/symbols/MemberSymbols.h>
#include <slang/ast/symbols/ParameterSymbols.h>
#include <slang/ast/symbols/PortSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>
#include <slang/numeric/ConstantValue.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/hir/expr_builders.hpp"
#include "lyra/hir/external_unit_object.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/hir/published_modport.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/ast_to_hir/constant_value.hpp"
#include "lyra/lowering/ast_to_hir/expression/selects.hpp"
#include "lyra/lowering/ast_to_hir/integral_constant.hpp"
#include "lyra/lowering/ast_to_hir/published_projection.hpp"
#include "lyra/lowering/ast_to_hir/subroutine_decl.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// What a value reference to a symbol lowers to, independent of whether the
// reference is written by simple name or by a hierarchical path (LRM 6, 8.4,
// 8.11, 23.6). A parameter or enum value is a compile-time constant whose value
// does not depend on the path used to reach it; a variable or net binds to a
// runtime storage cell; a class property reaches the invoking object's field;
// `this` is that object itself, which is no cell at all. One classification
// serves every consumer of a name -- including the reads a process is sensitive
// to, which build no expression at all -- so a symbol cannot be read as a
// constant by one of them and turned away by another.
// True when the symbol is the `this` handle (LRM 8.11) of the scope that
// declares it. The front end synthesizes one such variable per scope that can
// name the current instance -- a non-static method, a constraint block, and
// the class itself, whose property initializers may name it -- and which
// variable it is, is that scope's own answer.
auto IsCurrentInstanceHandle(const slang::ast::Symbol& sym) -> bool {
  const auto* variable = sym.as_if<slang::ast::VariableSymbol>();
  if (variable == nullptr) return false;
  const slang::ast::Scope* scope = sym.getParentScope();
  if (scope == nullptr) return false;
  const slang::ast::Symbol& declaring = scope->asSymbol();
  if (const auto* sub = declaring.as_if<slang::ast::SubroutineSymbol>()) {
    return sub->thisVar == variable;
  }
  if (const auto* cls = declaring.as_if<slang::ast::ClassType>()) {
    return cls->thisVar == variable;
  }
  if (const auto* con = declaring.as_if<slang::ast::ConstraintBlockSymbol>()) {
    return con->thisVar == variable;
  }
  return false;
}

}  // namespace

// Total over slang's symbol kinds with no `default`: a kind that ought to lower
// to a real referent must not hide in a catch-all and surface as a spurious
// "unsupported" -- the failure mode that let a hierarchically reached parameter
// read as an unsupported reference. Listing every kind forces a deliberate
// classification of each (a plausible referent like a specparam is a conscious
// entry, not a silent omission), and a kind added by a future slang release
// fails to compile until it is classified here.
auto ResolveReferent(
    const slang::ast::ValueSymbol& value, diag::SourceSpan span)
    -> diag::Result<NamedReferent> {
  using slang::ast::SymbolKind;
  const auto declared = [&](Referent kind) -> diag::Result<NamedReferent> {
    return NamedReferent{.symbol = &value, .kind = kind};
  };
  switch (value.kind) {
    // A modport gives its port identifiers a name space of their own (LRM
    // 25.5.4), in which a name either stands for the interface item the view
    // named it after or is one the view computed and the interface declares
    // nowhere. Which of the two a name is, is a property of the declaration.
    case SymbolKind::ModportPort: {
      const auto& port = value.as<slang::ast::ModportPortSymbol>();
      if (ViewDefinesTheName(port)) {
        return declared(Referent::kViewDefinedName);
      }
      const auto* item =
          port.internalSymbol == nullptr
              ? nullptr
              : port.internalSymbol->as_if<slang::ast::ValueSymbol>();
      if (item == nullptr) {
        return diag::Fail(
            span, diag::DiagCode::kUnsupportedExpressionForm,
            "a modport port connected to nothing inside its interface is not "
            "yet supported");
      }
      return ResolveReferent(*item, span);
    }

    case SymbolKind::Parameter:
      return declared(Referent::kParameterConstant);
    case SymbolKind::EnumValue:
      return declared(Referent::kEnumConstant);
    case SymbolKind::ClassProperty:
      return declared(Referent::kClassProperty);
    // One front-end kind, two referents: a variable a body declares, and the
    // handle to the object a method was invoked on (LRM 8.11), which declares
    // no storage and reaches no cell. Being total over the front end's kinds
    // cannot tell these apart, because the front end does not separate them.
    case SymbolKind::Variable:
      return declared(
          IsCurrentInstanceHandle(value) ? Referent::kThisHandle
                                         : Referent::kVariableStorage);
    case SymbolKind::FormalArgument:
    case SymbolKind::Iterator:
      return declared(Referent::kVariableStorage);
    case SymbolKind::PatternVar:
      return declared(Referent::kPatternBinding);
    case SymbolKind::Net:
      return declared(Referent::kNetStorage);

    // A specparam is a constant (LRM 6.20.4) and belongs with the two above by
    // what it is; it stands here because the front end declares it apart.
    case SymbolKind::Specparam:
      return declared(Referent::kSpecparam);

    // The rest of what a value reference can denote, each with its own
    // vocabulary in the standard.
    case SymbolKind::PrimitivePort:
      return declared(Referent::kPrimitivePort);
    case SymbolKind::ClockVar:
      return declared(Referent::kClockingSignal);
    case SymbolKind::LocalAssertionVar:
      return declared(Referent::kAssertionLocal);
    case SymbolKind::Field:
      return declared(Referent::kStructureMember);

    case SymbolKind::Unknown:
    case SymbolKind::Root:
    case SymbolKind::Definition:
    case SymbolKind::CompilationUnit:
    case SymbolKind::DeferredMember:
    case SymbolKind::TransparentMember:
    case SymbolKind::EmptyMember:
    case SymbolKind::PredefinedIntegerType:
    case SymbolKind::ScalarType:
    case SymbolKind::FloatingType:
    case SymbolKind::EnumType:
    case SymbolKind::PackedArrayType:
    case SymbolKind::FixedSizeUnpackedArrayType:
    case SymbolKind::DynamicArrayType:
    case SymbolKind::DPIOpenArrayType:
    case SymbolKind::AssociativeArrayType:
    case SymbolKind::QueueType:
    case SymbolKind::PackedStructType:
    case SymbolKind::UnpackedStructType:
    case SymbolKind::PackedUnionType:
    case SymbolKind::UnpackedUnionType:
    case SymbolKind::ClassType:
    case SymbolKind::CovergroupType:
    case SymbolKind::VoidType:
    case SymbolKind::NullType:
    case SymbolKind::CHandleType:
    case SymbolKind::StringType:
    case SymbolKind::EventType:
    case SymbolKind::UnboundedType:
    case SymbolKind::TypeRefType:
    case SymbolKind::UntypedType:
    case SymbolKind::SequenceType:
    case SymbolKind::PropertyType:
    case SymbolKind::VirtualInterfaceType:
    case SymbolKind::TypeAlias:
    case SymbolKind::ErrorType:
    case SymbolKind::ForwardingTypedef:
    case SymbolKind::NetType:
    case SymbolKind::TypeParameter:
    case SymbolKind::Port:
    case SymbolKind::MultiPort:
    case SymbolKind::InterfacePort:
    case SymbolKind::Modport:
    case SymbolKind::ModportClocking:
    case SymbolKind::Instance:
    case SymbolKind::InstanceBody:
    case SymbolKind::InstanceArray:
    case SymbolKind::Package:
    case SymbolKind::ExplicitImport:
    case SymbolKind::WildcardImport:
    case SymbolKind::Attribute:
    case SymbolKind::Genvar:
    case SymbolKind::GenerateBlock:
    case SymbolKind::GenerateBlockArray:
    case SymbolKind::ProceduralBlock:
    case SymbolKind::StatementBlock:
    case SymbolKind::Subroutine:
    case SymbolKind::ContinuousAssign:
    case SymbolKind::ElabSystemTask:
    case SymbolKind::GenericClassDef:
    case SymbolKind::MethodPrototype:
    case SymbolKind::UninstantiatedDef:
    case SymbolKind::ConstraintBlock:
    case SymbolKind::DefParam:
    case SymbolKind::Primitive:
    case SymbolKind::PrimitiveInstance:
    case SymbolKind::SpecifyBlock:
    case SymbolKind::Sequence:
    case SymbolKind::Property:
    case SymbolKind::AssertionPort:
    case SymbolKind::ClockingBlock:
    case SymbolKind::LetDecl:
    case SymbolKind::Checker:
    case SymbolKind::CheckerInstance:
    case SymbolKind::CheckerInstanceBody:
    case SymbolKind::RandSeqProduction:
    case SymbolKind::CovergroupBody:
    case SymbolKind::Coverpoint:
    case SymbolKind::CoverCross:
    case SymbolKind::CoverCrossBody:
    case SymbolKind::CoverageBin:
    case SymbolKind::TimingPath:
    case SymbolKind::PulseStyle:
    case SymbolKind::SystemTimingCheck:
    case SymbolKind::AnonymousProgram:
    case SymbolKind::NetAlias:
    case SymbolKind::ConfigBlock:
      return declared(Referent::kNotAValue);
  }
  throw InternalError("ResolveReferent: unknown slang SymbolKind");
}

namespace {

// The refusal for a declaration a value reference may denote and that nothing
// here lowers. Every kind the classification sends here is a construct, so
// every one names itself and cites the clause that defines it -- a reader who
// meets one can search for the construct and ask for it, which a shared
// sentence gives nobody. The set is closed by the classification above; a kind
// arriving from outside it has been misclassified there.
auto UnsupportedReferentMessage(Referent referent) -> std::string_view {
  switch (referent) {
    case Referent::kSpecparam:
      return "a specparam is not yet supported (LRM 6.20.4)";
    case Referent::kPrimitivePort:
      return "a port of a user-defined primitive is not yet supported (LRM 29)";
    case Referent::kClockingSignal:
      return "a signal of a clocking block is not yet supported (LRM 14.3)";
    case Referent::kAssertionLocal:
      return "a local variable of an assertion is not yet supported (LRM "
             "16.10)";
    case Referent::kStructureMember:
      return "a structure or union member named on its own, rather than "
             "through "
             "the value that holds it, is not yet supported (LRM 7.2)";
    case Referent::kPatternBinding:
    case Referent::kParameterConstant:
    case Referent::kEnumConstant:
    case Referent::kClassProperty:
    case Referent::kThisHandle:
    case Referent::kVariableStorage:
    case Referent::kNetStorage:
    case Referent::kViewDefinedName:
    case Referent::kNotAValue:
      break;
  }
  throw InternalError(
      "UnsupportedReferentMessage: a classification that is not a refusal was "
      "asked for the construct it refuses");
}

}  // namespace

auto FailOnUnsupportedReferent(Referent referent, diag::SourceSpan span)
    -> std::unexpected<diag::Diagnostic> {
  return diag::Fail(
      span, diag::DiagCode::kUnsupportedNonVariableNamedReference,
      std::string{UnsupportedReferentMessage(referent)});
}

namespace {

// A pattern-bound identifier (LRM 12.6) resolves to the `VariablePattern` node
// that declares it. That node is reached the same way from every context --
// the pattern lowering registered it on the unit before any body naming it was
// walked -- so one resolution serves the procedural, structural, and
// hierarchical entries alike.
auto MakePatternVarRefExpr(
    UnitLowerer& unit_lowerer, const slang::ast::PatternVarSymbol& sym,
    const slang::ast::Type& type, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  const auto pattern = unit_lowerer.LookupPatternVar(sym);
  if (!pattern.has_value()) {
    throw InternalError(
        "MakePatternVarRefExpr: pattern-bound identifier has no registered "
        "declaring pattern; the pattern lowering runs before any body that "
        "names it");
  }
  auto type_id = unit_lowerer.InternType(type, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return hir::MakeRefExpr(
      hir::PatternVarRef{.pattern = *pattern}, *type_id, span);
}

auto MakeEnumValueExpr(
    const slang::ast::EnumValueSymbol& sym, hir::TypeId type,
    diag::SourceSpan span) -> hir::Expr {
  const auto& cv = sym.getValue();
  if (!cv.isInteger()) {
    throw InternalError("MakeEnumValueExpr: enum value is not integral");
  }
  return MakeIntegralLiteralExpr(cv.integer(), type, span);
}

auto MakeParameterConstantExpr(
    UnitLowerer& unit_lowerer, WalkFrame frame, const slang::ast::Symbol& sym,
    const slang::ast::Type& type, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto type_id = unit_lowerer.InternType(type, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return MakeConstantValueExpr(
      unit_lowerer.Unit(), frame,
      sym.as<slang::ast::ParameterSymbol>().getValue(), *type_id, span);
}

auto MakeEnumConstantExpr(
    UnitLowerer& unit_lowerer, const slang::ast::Symbol& sym,
    const slang::ast::Type& type, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto type_id = unit_lowerer.InternType(type, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return MakeEnumValueExpr(
      sym.as<slang::ast::EnumValueSymbol>(), *type_id, span);
}

// LRM 7.12.4: a reference to an array-method `with`-clause iteration element
// (`item`) lowers to an `IterationBindingRef` naming `clause` and the element
// role, typed by its own reference type. The element is one of the clause's two
// iteration parameters, not a variable of the enclosing scope, so neither pass
// class's variable storage is consulted.
auto MakeIterationElementRefExpr(
    UnitLowerer& unit_lowerer, const slang::ast::NamedValueExpression& named,
    hir::WithClauseId clause, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto type_id = unit_lowerer.InternType(*named.type, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return MakeRefExpr(
      hir::IterationBindingRef{
          .clause = clause, .role = hir::IterationBindingRole::kElement},
      *type_id, span);
}

auto MakeClassPropertyRefExpr(
    UnitLowerer& unit_lowerer, const WalkFrame& frame,
    const slang::ast::Symbol& sym, const slang::ast::Type& type,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  auto type_id = unit_lowerer.InternType(type, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  const auto& prop = sym.as<slang::ast::ClassPropertySymbol>();
  // LRM 8.9: a static-lifetime property belongs to the type rather than to any
  // object of it, so its reference form carries neither the enclosing method's
  // receiver nor a fabricated stand-in. Instance properties and static
  // properties take structurally disjoint reference primaries.
  if (prop.lifetime == slang::ast::VariableLifetime::Static) {
    auto property = unit_lowerer.ResolveStaticPropertyTarget(frame, prop, span);
    if (!property) return std::unexpected(std::move(property.error()));
    return hir::MakeRefExpr(*property, *type_id, span);
  }
  const auto& owner_class =
      sym.getParentScope()->asSymbol().as<slang::ast::ClassType>();
  auto target =
      unit_lowerer.MakeClassPropertyTarget(frame, owner_class, prop, span);
  if (!target) {
    return std::unexpected(std::move(target.error()));
  }
  return hir::MakeRefExpr(
      hir::ClassPropertyRef{.target = *std::move(target)}, *type_id, span);
}

// LRM 8.11 `this` standing on its own: the source asks for the object itself
// rather than for something reached through it. Every qualifying use is
// answered where the qualification is lowered, so what arrives here is the
// handle, which is a primary of the expression grammar exactly as `null` is.
auto MakeCurrentInstanceHandleExpr(
    UnitLowerer& unit_lowerer, const slang::ast::Type& type,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  auto type_id = unit_lowerer.InternType(type, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return hir::MakeRefExpr(hir::ThisHandle{}, *type_id, span);
}

// The same keyword where no object is running. A structural expression and a
// hierarchical path both reach a scope rather than an invocation, so there is
// no receiver for the handle to refer to.
auto FailOnCurrentInstanceHandle(diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  return diag::Fail(
      span, diag::DiagCode::kUnsupportedExpressionForm,
      "`this` names the object a method was invoked on, which this context "
      "has none of (LRM 8.11)");
}

// Wraps a resolved value target as a reference Expr. Every way of reaching a
// cell -- a direct member of the reader's own scope, a routed reference sealed
// to a per-instance endpoint, a namespace unit's cell named across the boundary
// -- is a reference primary, so one wrap serves them all.
auto ValueTargetRefExpr(
    const hir::ValueTarget& target, hir::TypeId type, diag::SourceSpan span)
    -> hir::Expr {
  const auto wrap = [&](const auto& primary) -> hir::Expr {
    return hir::MakeRefExpr(primary, type, span);
  };
  return std::visit(
      Overloaded{
          [&](const hir::ReferenceRoute& route) -> hir::Expr {
            return std::visit(wrap, route);
          },
          [&](const hir::ExternalUnitValueRef& external) -> hir::Expr {
            return wrap(external);
          },
          [&](const hir::StaticPropertyRef& property) -> hir::Expr {
            return wrap(property);
          },
      },
      target);
}

// Lowers a reference to a value that has a cell -- a variable or a net --
// wherever that cell lives, through the one resolver. Shared by every
// named-value entry once each has classified what the name denotes, which is
// what lets the resolver answer about storage alone.
auto LowerValueRef(
    UnitLowerer& unit_lowerer, WalkFrame frame,
    const slang::ast::ValueSymbol& value, const slang::ast::Type& type,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  auto type_id = unit_lowerer.InternType(type, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  auto target = unit_lowerer.ResolveValueTarget(frame, value, span);
  if (!target) return std::unexpected(std::move(target.error()));
  return ValueTargetRefExpr(*target, *type_id, span);
}

// LRM 25.3: a name reached through an interface port, which is the port's own
// reach plus the descent the name spells out from there. What each step of that
// descent is, and what the route ends at, follow the way they do for a step
// onto an instance the reader can see -- the port decides where the descent
// starts and nothing else about it.
auto LowerInterfacePortValue(
    UnitLowerer& unit_lowerer, WalkFrame frame,
    const slang::ast::HierarchicalValueExpression& hve,
    const slang::ast::ValueSymbol& declaration, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto through = unit_lowerer.ReachOneThroughInterfacePort(frame, hve.ref);
  if (!through.has_value()) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "a name reached through an interface port by a path of this shape is "
        "not yet supported");
  }
  auto type_id = unit_lowerer.InternType(*hve.type, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  auto route = unit_lowerer.MakeRoutedRef(
      declaration, frame.Current(), *std::move(through));
  if (!route) return std::unexpected(std::move(route.error()));
  return ValueTargetRefExpr(hir::ValueTarget{*route}, *type_id, span);
}

// LRM 25.5: a name the modport an interface port selected defined for itself.
// The identifier belongs to the view rather than to the interface's
// declarations (LRM 25.5.4), so what it means is read out of the view the
// interface published -- it names no declaration and could not be found among
// the members.
//
// The route is kept beside the meaning because the two answers need different
// things from it: a place is reached over it member by member, while a call is
// made on the object it ends at. The meaning is copied rather than pointed at,
// since reaching the object may grow the arena it lives in.
struct OfferedName {
  ScopeRoute route;
  hir::ExternalUnitObjectId object;
  hir::ViewDefinedName meaning;
};

auto ResolveOfferedName(
    UnitLowerer& unit_lowerer, WalkFrame frame,
    const slang::ast::HierarchicalValueExpression& hve, diag::SourceSpan span)
    -> diag::Result<OfferedName> {
  // The view offering the name is the scope the port identifier is declared in
  // (LRM 25.5.4).
  const auto& selected =
      hve.symbol.getParentScope()->asSymbol().as<slang::ast::ModportSymbol>();
  // The object the view sits on is reached the way this unit reaches that
  // interface instance -- through the port a connection bound it to, or by a
  // route down to an instance the design declares inside this unit. Which of
  // the two is a fact about the object and not about the name, so it is the
  // same question a name reaching an ordinary member of that instance asks.
  auto through =
      hve.ref.isViaIfacePort()
          ? unit_lowerer.ReachOneThroughInterfacePort(frame, hve.ref)
          : unit_lowerer.RouteToScope(frame, *selected.getParentScope());
  if (!through.has_value() || !through->unit_name.has_value()) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "a name a view offers, reached past what the interface promised, is "
        "not yet supported");
  }
  const hir::ExternalUnitObjectId object =
      unit_lowerer.ExternalUnitObjectOf(*through->unit_name);
  // The record is an arena entry, so what this reference needs comes out of it
  // before anything else can grow the arena.
  const hir::PublishedModport* view = hir::FindModport(
      unit_lowerer.Unit().external_unit_objects.Get(object).modports,
      selected.name);
  const hir::PublishedModportPort* offered =
      view == nullptr ? nullptr : view->Find(hve.symbol.name);
  if (offered == nullptr) {
    throw InternalError(
        "ResolveOfferedName: an interface publishes every name each of its "
        "views defines, and the front end has refused any other");
  }
  return OfferedName{
      .route = *std::move(through),
      .object = object,
      .meaning = offered->meaning};
}

// The storage a name a view defines designates, as a place this unit reaches:
// each part routed to the member the interface published and descended by the
// path it stated, then joined. A concatenation of places is itself a place (LRM
// 11.4.12), so what comes back is written, read, driven and taken over exactly
// as any other place is.
auto ViewDefinedPlaceExpr(
    UnitLowerer& unit_lowerer, WalkFrame frame, const OfferedName& offered,
    const hir::ViewDefinedPlace& place, diag::SourceSpan span) -> hir::Expr {
  std::vector<hir::ExprId> parts;
  parts.reserve(place.parts.size());
  for (const hir::MemberProjection& part : place.parts) {
    const hir::PublishedMember member =
        unit_lowerer.Unit()
            .external_unit_objects.Get(offered.object)
            .members.Get(part.member);
    hir::Expr base = unit_lowerer.MakeRoutedMemberRef(
        frame.Current(),
        hir::RoutedRefDecl{
            .recipe =
                hir::RoutedPathRecipe{
                    .head = offered.route.head,
                    .steps = offered.route.steps,
                    .leaf =
                        hir::SignatureMemberLeaf{
                            .object = offered.object,
                            .member = part.member,
                            .storage = member.storage,
                            .type = member.type}}},
        span);
    parts.push_back(frame.Exprs().Add(ProjectPublishedPath(
        unit_lowerer, frame, part.path, std::move(base), span)));
  }
  // Joining is what gives the name a type its parts do not have, so a single
  // part already carrying the name's type is the name -- and one that does not
  // was joined by the view and is joined here too.
  if (parts.size() == 1 &&
      frame.Exprs().Get(parts.front()).type == place.type) {
    return frame.Exprs().Get(parts.front());
  }
  return hir::Expr{
      .type = place.type,
      .data = hir::ConcatExpr{.operands = std::move(parts)},
      .span = span};
}

// LRM 25.5.4: a name a view defines is either the storage its expression
// designates or, where the view offers it only for reading, a value this
// interface computes. The first is reached as a place, which is what makes
// every form of write to it an ordinary write; the second is a call on the
// instance the port carries, because the expression names declarations this
// unit never sees.
auto LowerViewDefinedName(
    UnitLowerer& unit_lowerer, WalkFrame frame,
    const slang::ast::HierarchicalValueExpression& hve, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto resolved = ResolveOfferedName(unit_lowerer, frame, hve, span);
  if (!resolved) return std::unexpected(std::move(resolved.error()));
  return std::visit(
      Overloaded{
          [&](const hir::ViewDefinedPlace& place) -> hir::Expr {
            return ViewDefinedPlaceExpr(
                unit_lowerer, frame, *resolved, place, span);
          },
          [&](const hir::ViewComputedValue& computed) -> hir::Expr {
            const hir::ExternalUnitObject& promised =
                unit_lowerer.Unit().external_unit_objects.Get(resolved->object);
            const hir::TypeId result_type =
                promised.callables.Get(computed.evaluate).result_type;
            const hir::TypeId object_type = unit_lowerer.Unit().types.Intern(
                hir::Type{
                    hir::UnitObjectType{.unit_name = promised.unit_name}});
            return hir::Expr{
                .type = result_type,
                .data =
                    hir::CallExpr{
                        .callee =
                            hir::ExternalUnitMethodRef{
                                .receiver = unit_lowerer.MakeRoutedObjectRef(
                                    frame.Current(), resolved->route,
                                    object_type),
                                .object = resolved->object,
                                .callable = computed.evaluate},
                        .arguments = {}},
                .span = span};
          }},
      resolved->meaning);
}

}  // namespace

auto NamesCurrentInstance(const slang::ast::Expression& expr) -> bool {
  const auto* named = expr.as_if<slang::ast::NamedValueExpression>();
  return named != nullptr && IsCurrentInstanceHandle(named->symbol);
}

auto LowerCurrentInstanceMember(
    UnitLowerer& unit_lowerer, WalkFrame frame,
    const slang::ast::Symbol& member, const slang::ast::Type& type,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  if (member.kind == slang::ast::SymbolKind::ClassProperty) {
    return MakeClassPropertyRefExpr(unit_lowerer, frame, member, type, span);
  }
  // A value parameter of a parameterized class (LRM 8.25) is fixed by the
  // specialization the enclosing method belongs to, so the qualification names
  // a value already known and folds exactly as the bare name does.
  if (member.kind == slang::ast::SymbolKind::Parameter) {
    return MakeParameterConstantExpr(unit_lowerer, frame, member, type, span);
  }
  return diag::Fail(
      span, diag::DiagCode::kUnsupportedExpressionForm,
      "`this` qualifies a property, a value parameter, or a method of the "
      "current instance (LRM 8.11), and this member is none of them");
}

auto LowerNamedValueProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::NamedValueExpression& named) -> diag::Result<hir::Expr> {
  auto& unit_lowerer = proc.Owner();
  const auto& mapper = unit_lowerer.SourceMapper();
  const auto span = mapper.SpanOf(named.sourceRange);
  const auto& sym = named.symbol;

  if (auto clause = frame.FindIterationClause(sym)) {
    return MakeIterationElementRefExpr(unit_lowerer, named, *clause, span);
  }

  auto resolved = ResolveReferent(sym, span);
  if (!resolved) return std::unexpected(std::move(resolved.error()));
  const slang::ast::ValueSymbol& target = *resolved->symbol;
  const Referent referent = resolved->kind;
  switch (referent) {
    // A name a view offers lives in the modport's own name space (LRM 25.5.4),
    // and a plain identifier resolves against the scope's own members, so no
    // spelling reaches one here.
    case Referent::kViewDefinedName:
      throw InternalError(
          "LowerNamedValueProc: a plain identifier does not reach a name a "
          "view offers");
    case Referent::kParameterConstant:
      return MakeParameterConstantExpr(
          unit_lowerer, frame, target, *named.type, span);
    case Referent::kEnumConstant:
      return MakeEnumConstantExpr(unit_lowerer, target, *named.type, span);
    // Inside an instance method, a class property named without an explicit
    // handle (LRM 8.4) reaches the invoking object through the method's
    // receiver, so it lowers to a receiver-relative property reference.
    case Referent::kClassProperty:
      return MakeClassPropertyRefExpr(
          unit_lowerer, frame, target, *named.type, span);
    case Referent::kThisHandle:
      return MakeCurrentInstanceHandleExpr(unit_lowerer, *named.type, span);
    case Referent::kPatternBinding:
      return MakePatternVarRefExpr(
          unit_lowerer, target.as<slang::ast::PatternVarSymbol>(), *named.type,
          span);
    // Subroutine formals (LRM 13.5) and foreach iterators (LRM 12.7.3) are
    // variable-family symbols too, so this arm covers a name bound to the
    // enclosing body's own storage as well as one naming a cell elsewhere. The
    // lexical binding wins: only a name the body does not declare is a value
    // reached through the object graph.
    case Referent::kVariableStorage: {
      const auto& var = target.as<slang::ast::VariableSymbol>();
      if (auto local = proc.LookupProceduralVar(var)) {
        const hir::TypeId type =
            frame.current_procedural_body->procedural_vars.Get(*local).type;
        return hir::MakeRefExpr(
            hir::ProceduralVarRef{.var = *local}, type, span);
      }
      return LowerValueRef(unit_lowerer, frame, var, *named.type, span);
    }
    // A net (LRM 6.5) is always a structural signal, never a procedural local.
    case Referent::kNetStorage:
      return LowerValueRef(unit_lowerer, frame, target, *named.type, span);
    case Referent::kSpecparam:
    case Referent::kPrimitivePort:
    case Referent::kClockingSignal:
    case Referent::kAssertionLocal:
    case Referent::kStructureMember:
      return FailOnUnsupportedReferent(referent, span);
    case Referent::kNotAValue:
      throw InternalError(
          "LowerNamedValueProc: a named value resolved to a declaration that "
          "denotes no value");
  }
  throw InternalError("LowerNamedValueProc: unknown Referent");
}

// LRM 23.6 hierarchical reference. A reached constant folds to its value; a
// reached cell is located from the reader's elaborated position and the target
// symbol, the same way a simple name's is -- the path a reference was written
// with is provenance, not a routing authority. A name that reaches storage
// through an interface port (LRM 25.3) is the one exception: that storage lives
// in a unit the reader reaches no other way, so which port it came through is
// the route and not merely how it was spelled. A constant reached through one
// still folds to its value, because what the port changes is how the target is
// reached and not what it is.
auto LowerHierarchicalValue(
    UnitLowerer& unit_lowerer, WalkFrame frame,
    const slang::ast::HierarchicalValueExpression& hve)
    -> diag::Result<hir::Expr> {
  const auto span = unit_lowerer.SourceMapper().SpanOf(hve.sourceRange);

  auto resolved = ResolveReferent(hve.symbol, span);
  if (!resolved) return std::unexpected(std::move(resolved.error()));
  const slang::ast::ValueSymbol& target = *resolved->symbol;
  const Referent referent = resolved->kind;
  switch (referent) {
    // A name the view defined for itself is on no member list (LRM 25.5.4), so
    // it resolves against the view rather than against the members. An item the
    // view named without an expression is the interface's own item, and is
    // reached the way every other name on that instance is.
    case Referent::kViewDefinedName:
      return LowerViewDefinedName(unit_lowerer, frame, hve, span);
    // A hierarchically reached constant folds to its value; the path is not
    // navigated because the value is fixed at elaboration.
    case Referent::kParameterConstant:
      return MakeParameterConstantExpr(
          unit_lowerer, frame, target, *hve.type, span);
    case Referent::kEnumConstant:
      return MakeEnumConstantExpr(unit_lowerer, target, *hve.type, span);
    // A path through the design hierarchy ends at an object, and a property is
    // reached from there by the member access that names it (LRM 8.4), so no
    // spelling makes a property the end of the path: the class scope resolution
    // operator is refused after a dotted path, and a package-scoped class is
    // resolved as a name rather than as a hierarchy walk.
    case Referent::kClassProperty:
      throw InternalError(
          "LowerHierarchicalValue: a hierarchical path ends at an object, not "
          "at a property of one");
    case Referent::kThisHandle:
      return FailOnCurrentInstanceHandle(span);
    case Referent::kSpecparam:
    case Referent::kPrimitivePort:
    case Referent::kClockingSignal:
    case Referent::kAssertionLocal:
    case Referent::kStructureMember:
      return FailOnUnsupportedReferent(referent, span);
    case Referent::kNotAValue:
      throw InternalError(
          "hierarchical value lowering: a path resolved to a declaration that "
          "denotes no value");
    case Referent::kPatternBinding:
      return MakePatternVarRefExpr(
          unit_lowerer, target.as<slang::ast::PatternVarSymbol>(), *hve.type,
          span);
    case Referent::kVariableStorage:
    case Referent::kNetStorage: {
      if (hve.ref.isViaIfacePort()) {
        return LowerInterfacePortValue(unit_lowerer, frame, hve, target, span);
      }
      auto type_id = unit_lowerer.InternType(*hve.type, span);
      if (!type_id) return std::unexpected(std::move(type_id.error()));
      auto reached = unit_lowerer.ResolveValueTarget(frame, target, span);
      if (!reached) return std::unexpected(std::move(reached.error()));
      return ValueTargetRefExpr(*reached, *type_id, span);
    }
  }
  throw InternalError("LowerHierarchicalValue: unknown Referent");
}

auto LowerNamedValueStructural(
    UnitLowerer& unit_lowerer, WalkFrame frame,
    const slang::ast::NamedValueExpression& named) -> diag::Result<hir::Expr> {
  const auto& mapper = unit_lowerer.SourceMapper();
  const auto span = mapper.SpanOf(named.sourceRange);
  const auto& sym = named.symbol;
  if (auto clause = frame.FindIterationClause(sym)) {
    return MakeIterationElementRefExpr(unit_lowerer, named, *clause, span);
  }
  auto resolved = ResolveReferent(sym, span);
  if (!resolved) return std::unexpected(std::move(resolved.error()));
  const slang::ast::ValueSymbol& target = *resolved->symbol;
  const Referent referent = resolved->kind;
  switch (referent) {
    // As in a process: a plain identifier resolves against the scope's own
    // members, and a name a view offers lives in the modport's name space.
    case Referent::kViewDefinedName:
      throw InternalError(
          "LowerNamedValueStructural: a plain identifier does not reach a name "
          "a view offers");
    case Referent::kParameterConstant:
      return MakeParameterConstantExpr(
          unit_lowerer, frame, target, *named.type, span);
    case Referent::kEnumConstant:
      return MakeEnumConstantExpr(unit_lowerer, target, *named.type, span);
    case Referent::kPatternBinding:
      return MakePatternVarRefExpr(
          unit_lowerer, target.as<slang::ast::PatternVarSymbol>(), *named.type,
          span);
    case Referent::kVariableStorage:
    case Referent::kNetStorage:
      return LowerValueRef(unit_lowerer, frame, target, *named.type, span);
    // A static property (LRM 8.9) belongs to the type rather than to an object
    // of it, so it is reached without a receiver and reads here exactly as it
    // does in a process -- a structural expression stands in the same scope a
    // process of it does, so it counts the same distance to the instance
    // replicating the class. An instance property is reachable only through a
    // receiver, which a structural expression has none of.
    case Referent::kClassProperty: {
      const auto& prop = target.as<slang::ast::ClassPropertySymbol>();
      if (prop.lifetime == slang::ast::VariableLifetime::Static) {
        return MakeClassPropertyRefExpr(
            unit_lowerer, frame, target, *named.type, span);
      }
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedStructuralExpressionForm,
          "an instance class property is reachable only through a receiver, "
          "which a structural expression has none of");
    }
    case Referent::kThisHandle:
      return FailOnCurrentInstanceHandle(span);
    case Referent::kSpecparam:
    case Referent::kPrimitivePort:
    case Referent::kClockingSignal:
    case Referent::kAssertionLocal:
    case Referent::kStructureMember:
      return FailOnUnsupportedReferent(referent, span);
    case Referent::kNotAValue:
      throw InternalError(
          "LowerNamedValueStructural: a named value resolved to a declaration "
          "that denotes no value");
  }
  throw InternalError("LowerNamedValueStructural: unknown Referent");
}

}  // namespace lyra::lowering::ast_to_hir
