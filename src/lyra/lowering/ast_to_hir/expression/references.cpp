#include "lyra/lowering/ast_to_hir/expression/references.hpp"

#include <expected>
#include <optional>
#include <string>
#include <utility>

#include <slang/ast/Expression.h>
#include <slang/ast/HierarchicalReference.h>
#include <slang/ast/Scope.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/symbols/ClassSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/MemberSymbols.h>
#include <slang/ast/symbols/ParameterSymbols.h>
#include <slang/ast/symbols/PortSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>
#include <slang/numeric/ConstantValue.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/hir/expr_builders.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/ast_to_hir/constant_value.hpp"
#include "lyra/lowering/ast_to_hir/expression/selects.hpp"
#include "lyra/lowering/ast_to_hir/expression/view_names.hpp"
#include "lyra/lowering/ast_to_hir/expression/virtual_interface.hpp"
#include "lyra/lowering/ast_to_hir/integral_constant.hpp"
#include "lyra/lowering/ast_to_hir/subroutine_decl.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

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

// An enumeration's members are part of what the type is (LRM 6.19), so this
// value varies with the type and a type is already an artifact's axis. Where a
// block declares the enumeration itself, the blocks declare different types and
// are compiled apart for that reason rather than for this one.
auto MakeEnumValueExpr(
    const slang::ast::EnumValueSymbol& sym, hir::TypeId type,
    diag::SourceSpan span) -> hir::Expr {
  const auto& cv = sym.getValue();
  if (!cv.isInteger()) {
    throw InternalError("MakeEnumValueExpr: enum value is not integral");
  }
  return MakeIntegralLiteralExpr(cv.integer(), type, span);
}

// A parameter read here is folded to the value one elaboration gave it. That
// holds where the value varies only with the parameterization of the unit,
// package or class declaring it, since a parameterization is an artifact of its
// own. Some parameters vary with something else, and a simple name reaching
// one resolves to its declaration instead: one a generate block declares,
// which varies with the index; one a unit's instance is handed when it is built
// (LRM 23.10.2), or works out from such a value, which varies per instance; and
// a constant a subroutine or a procedural block declares from either, which its
// body holds. A hierarchical name still folds them. Inside the unit, a
// parameter read that way is kept in its specialization; from outside it, a
// unit whose instances would read different values here lowers apart and is
// not shared.
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
    return hir::MakeValueTargetRefExpr(*property, *type_id, span);
  }
  const auto& owner_class =
      sym.getParentScope()->asSymbol().as<slang::ast::ClassType>();
  auto target = unit_lowerer.MakeClassPropertyTarget(owner_class, prop, span);
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
  auto target =
      unit_lowerer.ResolveValueTarget(frame, value, FromReader{}, span);
  if (!target) return std::unexpected(std::move(target.error()));
  return hir::MakeValueTargetRefExpr(*target, *type_id, span);
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
  if (const auto* filled = unit_lowerer.NameFilledDuringElaboration(sym)) {
    return LowerValueRef(unit_lowerer, frame, *filled, *named.type, span);
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
    // A constant the body holds because its value differs between the objects
    // built from this unit is read from the body's own cell; any other folds.
    case Referent::kParameterConstant:
      if (auto held = proc.LookupProceduralVar(target)) {
        const hir::TypeId type =
            frame.current_procedural_body->procedural_vars.Get(*held).type;
        return hir::MakeRefExpr(
            hir::ProceduralVarRef{.var = *held}, type, span);
      }
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
// reached cell is reached from where the name starts -- here, through an
// interface port (LRM 25.3), or where its upward search landed (LRM 23.8) --
// since that is what differs between the instances a unit serves. A constant
// reached through a port or a climb still folds to its value, because what
// the start changes is how the target is reached and not what it is.
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
      return LowerRoutedViewName(unit_lowerer, frame, hve, span);
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
      auto type_id = unit_lowerer.InternType(*hve.type, span);
      if (!type_id) return std::unexpected(std::move(type_id.error()));
      auto reached = unit_lowerer.ResolveValueTarget(
          frame, target, unit_lowerer.StartOf(frame, hve.ref), span);
      if (!reached) return std::unexpected(std::move(reached.error()));
      return hir::MakeValueTargetRefExpr(*reached, *type_id, span);
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
  if (const auto* filled = unit_lowerer.NameFilledDuringElaboration(sym)) {
    return LowerValueRef(unit_lowerer, frame, *filled, *named.type, span);
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

auto LowerInterfaceInstanceValue(
    UnitLowerer& unit_lowerer, WalkFrame frame,
    const slang::ast::ArbitrarySymbolExpression& named, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  const auto* handle_type =
      named.type->getCanonicalType().as_if<slang::ast::VirtualInterfaceType>();
  if (handle_type == nullptr) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "a name that denotes no value is not yet supported here");
  }
  // The type names the instance the name reached, whether it was written
  // directly, through a port bound to it, or with a modport selected; a
  // modport narrows what is reached through the value and not which instance
  // it is.
  auto route = unit_lowerer.RouteToScopeOrRefuse(
      frame, handle_type->iface.body,
      unit_lowerer.StartOf(frame, named.hierRef), span);
  if (!route) return std::unexpected(std::move(route.error()));
  auto type_id = unit_lowerer.InternType(*named.type, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  const hir::TypeId object_type = unit_lowerer.ScopeClassTypeOf(
      unit_lowerer.ScopeClassOfInstance(handle_type->iface));
  return hir::MakeRefExpr(
      unit_lowerer.MakeRoutedObjectRef(
          frame.Current(), *std::move(route), object_type),
      *type_id, span);
}

}  // namespace lyra::lowering::ast_to_hir
