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
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>
#include <slang/numeric/ConstantValue.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/hir/expr_builders.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/ast_to_hir/constant_value.hpp"
#include "lyra/lowering/ast_to_hir/expression/selects.hpp"
#include "lyra/lowering/ast_to_hir/integral_constant.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// What a value reference to a symbol lowers to, independent of whether the
// reference is written by simple name or by a hierarchical path (LRM 6, 8.4,
// 23.6). A parameter or enum value is a compile-time constant whose value does
// not depend on the path used to reach it; a variable or net binds to a runtime
// storage cell; a class property reaches the invoking object's field. One
// classification serves every reference-lowering entry so a symbol cannot be
// read as a constant through one syntax and rejected through another.
enum class Referent {
  kPatternBinding,
  kParameterConstant,
  kEnumConstant,
  kClassProperty,
  kVariableStorage,
  kNetStorage,
  kUnsupported,
};

// Total over slang's symbol kinds with no `default`: a kind that ought to lower
// to a real referent must not hide in a catch-all and surface as a spurious
// "unsupported" -- the failure mode that let a hierarchically reached parameter
// read as an unsupported reference. Listing every kind forces a deliberate
// classification of each (a plausible referent like a specparam is a conscious
// entry, not a silent omission), and a kind added by a future slang release
// fails to compile until it is classified here.
auto ClassifyReferent(const slang::ast::Symbol& sym) -> Referent {
  using slang::ast::SymbolKind;
  switch (sym.kind) {
    case SymbolKind::Parameter:
      return Referent::kParameterConstant;
    case SymbolKind::EnumValue:
      return Referent::kEnumConstant;
    case SymbolKind::ClassProperty:
      return Referent::kClassProperty;
    case SymbolKind::Variable:
    case SymbolKind::FormalArgument:
    case SymbolKind::Iterator:
      return Referent::kVariableStorage;
    case SymbolKind::PatternVar:
      return Referent::kPatternBinding;
    case SymbolKind::Net:
      return Referent::kNetStorage;
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
    case SymbolKind::ModportPort:
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
    case SymbolKind::Field:
    case SymbolKind::Subroutine:
    case SymbolKind::ContinuousAssign:
    case SymbolKind::ElabSystemTask:
    case SymbolKind::GenericClassDef:
    case SymbolKind::MethodPrototype:
    case SymbolKind::UninstantiatedDef:
    case SymbolKind::ConstraintBlock:
    case SymbolKind::DefParam:
    case SymbolKind::Specparam:
    case SymbolKind::Primitive:
    case SymbolKind::PrimitivePort:
    case SymbolKind::PrimitiveInstance:
    case SymbolKind::SpecifyBlock:
    case SymbolKind::Sequence:
    case SymbolKind::Property:
    case SymbolKind::AssertionPort:
    case SymbolKind::ClockingBlock:
    case SymbolKind::ClockVar:
    case SymbolKind::LocalAssertionVar:
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
      return Referent::kUnsupported;
  }
  throw InternalError("ClassifyReferent: unknown slang SymbolKind");
}

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
    UnitLowerer& unit_lowerer, const slang::ast::Symbol& sym,
    const slang::ast::Type& type, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto type_id = unit_lowerer.InternType(type, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  const auto& prop = sym.as<slang::ast::ClassPropertySymbol>();
  const auto& owner_class =
      sym.getParentScope()->asSymbol().as<slang::ast::ClassType>();
  auto owner_ref = unit_lowerer.ResolveClassRef(owner_class, span);
  if (!owner_ref) return std::unexpected(std::move(owner_ref.error()));
  // LRM 8.9: a static-lifetime property is one cell owned by the class, so
  // its reference form carries neither the enclosing method's receiver nor
  // a fabricated stand-in -- a type-associated cell has no per-instance
  // context to reach. Instance properties and static properties take
  // structurally disjoint reference primaries.
  if (prop.lifetime == slang::ast::VariableLifetime::Static) {
    return hir::MakeRefExpr(
        hir::StaticPropertyRef{
            .target = unit_lowerer.MakeStaticPropertyTarget(*owner_ref, prop)},
        *type_id, span);
  }
  return hir::MakeRefExpr(
      hir::ClassPropertyRef{
          .target = unit_lowerer.MakeClassPropertyTarget(*owner_ref, prop)},
      *type_id, span);
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
      },
      target);
}

// Lowers a reference to a value that has a cell -- a variable or a net --
// wherever that cell lives, through the one resolver. Shared by every
// named-value entry once each has ruled out the forms its context admits that
// have no cell. A storage referent always resolves here: a simple name is
// lexically enclosing and a hierarchical path is fully elaborated by the
// frontend, so absence is a compiler-bug invariant.
auto LowerValueRef(
    UnitLowerer& unit_lowerer, WalkFrame frame,
    const slang::ast::ValueSymbol& value, const slang::ast::Type& type,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  auto type_id = unit_lowerer.InternType(type, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  auto target = unit_lowerer.ResolveValueTarget(frame, value);
  if (!target) return std::unexpected(std::move(target.error()));
  if (!target->has_value()) {
    throw InternalError("LowerValueRef: storage symbol has no reachable cell");
  }
  return ValueTargetRefExpr(**target, *type_id, span);
}

// LRM 25.3: a name reached through an interface port, which is the port's whole
// route plus the name the interface published, and what the route ends at
// follows the way it does for a step onto an instance.
auto LowerInterfacePortValue(
    UnitLowerer& unit_lowerer, WalkFrame frame,
    const slang::ast::HierarchicalValueExpression& hve,
    const slang::ast::ValueSymbol& declaration, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  const auto path = hve.ref.path;
  // A path ends at what the name reached, so the member has to be the hop below
  // the port. A hop in between descends into what the interface itself owns,
  // past what the port promised about the interface it carries.
  const auto below_the_port = path.subspan(1);
  if (below_the_port.empty() ||
      below_the_port.front().symbol != hve.ref.target) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "a nested name reached through an interface port is not yet supported");
  }
  auto type_id = unit_lowerer.InternType(*hve.type, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  auto through = unit_lowerer.RouteThroughInterfacePort(frame, *path[0].symbol);
  auto route = unit_lowerer.MakeRoutedRef(
      declaration, frame.Current(), std::move(through.head),
      std::move(through.steps));
  if (!route) return std::unexpected(std::move(route.error()));
  return ValueTargetRefExpr(hir::ValueTarget{*route}, *type_id, span);
}

}  // namespace

auto ResolveNamedDeclaration(
    const slang::ast::ValueSymbol& value, diag::SourceSpan span)
    -> diag::Result<const slang::ast::ValueSymbol*> {
  const auto* port = value.as_if<slang::ast::ModportPortSymbol>();
  if (port == nullptr) return &value;
  if (port->explicitConnection != nullptr) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "a modport expression is not yet supported");
  }
  const auto* item =
      port->internalSymbol == nullptr
          ? nullptr
          : port->internalSymbol->as_if<slang::ast::ValueSymbol>();
  if (item == nullptr) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "a modport port connected to nothing inside its interface is not yet "
        "supported");
  }
  return item;
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

  switch (ClassifyReferent(sym)) {
    case Referent::kParameterConstant:
      return MakeParameterConstantExpr(
          unit_lowerer, frame, sym, *named.type, span);
    case Referent::kEnumConstant:
      return MakeEnumConstantExpr(unit_lowerer, sym, *named.type, span);
    // Inside an instance method, a class property named without an explicit
    // handle (LRM 8.4) reaches the invoking object through the method's
    // receiver, so it lowers to a receiver-relative property reference.
    case Referent::kClassProperty:
      return MakeClassPropertyRefExpr(unit_lowerer, sym, *named.type, span);
    case Referent::kPatternBinding:
      return MakePatternVarRefExpr(
          unit_lowerer, sym.as<slang::ast::PatternVarSymbol>(), *named.type,
          span);
    // Subroutine formals (LRM 13.5) and foreach iterators (LRM 12.7.3) are
    // variable-family symbols too, so this arm covers a name bound to the
    // enclosing body's own storage as well as one naming a cell elsewhere. The
    // lexical binding wins: only a name the body does not declare is a value
    // reached through the object graph.
    case Referent::kVariableStorage: {
      const auto& var = sym.as<slang::ast::VariableSymbol>();
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
      return LowerValueRef(
          unit_lowerer, frame, sym.as<slang::ast::ValueSymbol>(), *named.type,
          span);
    case Referent::kUnsupported:
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedNonVariableNamedReference,
          "reference to non-variable declaration is not supported");
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

  auto declaration = ResolveNamedDeclaration(hve.symbol, span);
  if (!declaration) return std::unexpected(std::move(declaration.error()));
  const slang::ast::ValueSymbol& target = **declaration;
  switch (ClassifyReferent(target)) {
    // A hierarchically reached constant folds to its value; the path is not
    // navigated because the value is fixed at elaboration.
    case Referent::kParameterConstant:
      return MakeParameterConstantExpr(
          unit_lowerer, frame, target, *hve.type, span);
    case Referent::kEnumConstant:
      return MakeEnumConstantExpr(unit_lowerer, target, *hve.type, span);
    case Referent::kClassProperty:
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedExpressionForm,
          "hierarchical reference to a class property is not yet supported");
    case Referent::kUnsupported:
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedExpressionForm,
          "hierarchical reference to this declaration kind is not yet "
          "supported");
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
      auto reached = unit_lowerer.ResolveValueTarget(frame, target);
      if (!reached) return std::unexpected(std::move(reached.error()));
      // A path this unit cannot yet express reaches a real cell the user
      // named, so it is a lowering gap rather than a compiler-bug invariant.
      if (!reached->has_value()) {
        return diag::Fail(
            span, diag::DiagCode::kUnsupportedExpressionForm,
            "hierarchical reference to this target form is not yet supported");
      }
      return ValueTargetRefExpr(**reached, *type_id, span);
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
  switch (ClassifyReferent(sym)) {
    case Referent::kParameterConstant:
      return MakeParameterConstantExpr(
          unit_lowerer, frame, sym, *named.type, span);
    case Referent::kEnumConstant:
      return MakeEnumConstantExpr(unit_lowerer, sym, *named.type, span);
    case Referent::kPatternBinding:
      return MakePatternVarRefExpr(
          unit_lowerer, sym.as<slang::ast::PatternVarSymbol>(), *named.type,
          span);
    case Referent::kVariableStorage:
    case Referent::kNetStorage:
      return LowerValueRef(
          unit_lowerer, frame, sym.as<slang::ast::ValueSymbol>(), *named.type,
          span);
    // A static property (LRM 8.9) is one cell owned by the class, reached
    // without a receiver, so it reads here exactly as it does in a process. An
    // instance property is reachable only through a receiver, which a
    // structural expression has none of.
    case Referent::kClassProperty: {
      const auto& prop = sym.as<slang::ast::ClassPropertySymbol>();
      if (prop.lifetime == slang::ast::VariableLifetime::Static) {
        return MakeClassPropertyRefExpr(unit_lowerer, sym, *named.type, span);
      }
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedStructuralExpressionForm,
          "an instance class property is reachable only through a receiver, "
          "which a structural expression has none of");
    }
    case Referent::kUnsupported:
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedNonVariableNamedReference,
          "reference to non-variable declaration is not supported");
  }
  throw InternalError("LowerNamedValueStructural: unknown Referent");
}

}  // namespace lyra::lowering::ast_to_hir
