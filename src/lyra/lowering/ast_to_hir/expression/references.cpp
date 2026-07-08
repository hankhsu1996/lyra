#include "lyra/lowering/ast_to_hir/expression/references.hpp"

#include <expected>
#include <optional>
#include <string>
#include <utility>
#include <variant>

#include <slang/ast/Expression.h>
#include <slang/ast/HierarchicalReference.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/symbols/ClassSymbols.h>
#include <slang/ast/symbols/ParameterSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>
#include <slang/numeric/ConstantValue.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/hir/expr_builders.hpp"
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
    case SymbolKind::PatternVar:
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
    ModuleLowerer& module, WalkFrame frame, const slang::ast::Symbol& sym,
    const slang::ast::Type& type, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto type_id = module.InternType(type, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return MakeConstantValueExpr(
      module.Unit(), frame, sym.as<slang::ast::ParameterSymbol>().getValue(),
      *type_id, span);
}

auto MakeEnumConstantExpr(
    ModuleLowerer& module, const slang::ast::Symbol& sym,
    const slang::ast::Type& type, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto type_id = module.InternType(type, span);
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
    ModuleLowerer& module, const slang::ast::NamedValueExpression& named,
    hir::WithClauseId clause, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto type_id = module.InternType(*named.type, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return MakeRefExpr(
      hir::IterationBindingRef{
          .clause = clause, .role = hir::IterationBindingRole::kElement},
      *type_id, span);
}

auto MakeClassPropertyRefExpr(
    ModuleLowerer& module, const slang::ast::Symbol& sym,
    const slang::ast::Type& type, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto type_id = module.InternType(type, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return hir::MakeRefExpr(
      hir::ClassPropertyRef{
          .field_index =
              ClassPropertyIndex(sym.as<slang::ast::ClassPropertySymbol>())},
      *type_id, span);
}

// Wraps a resolved reader-relative route as a reference Expr. A route is one of
// two primaries -- a typed enclosing structural-data-object reference or a
// cross-unit slot reference -- and each wraps identically.
auto RouteRefExpr(
    const hir::ReferenceRoute& route, hir::TypeId type, diag::SourceSpan span)
    -> hir::Expr {
  return std::visit(
      [&](const auto& primary) -> hir::Expr {
        return hir::MakeRefExpr(primary, type, span);
      },
      route);
}

// Lowers a reference to a structural signal (a variable or net of an enclosing
// scope) through the one reference-route translator. Shared by the procedural
// and structural named-value paths once each has ruled out the non-signal forms
// its context admits. A simple name is always lexically enclosing, so the route
// is always available and its absence is a compiler-bug invariant.
auto LowerStructuralSignalRef(
    ModuleLowerer& module, WalkFrame frame,
    const slang::ast::ValueSymbol& value, const slang::ast::Type& type,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  auto type_id = module.InternType(type, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  auto route = module.TranslateReferenceRoute(frame, value);
  if (!route) {
    throw InternalError(
        "LowerStructuralSignalRef: structural signal has no reader-relative "
        "route");
  }
  return RouteRefExpr(*route, *type_id, span);
}

}  // namespace

auto LowerNamedValueProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::NamedValueExpression& named) -> diag::Result<hir::Expr> {
  auto& module = proc.Module();
  const auto& mapper = module.SourceMapper();
  const auto span = mapper.SpanOf(named.sourceRange);
  const auto& sym = named.symbol;

  if (auto clause = frame.FindIterationClause(sym)) {
    return MakeIterationElementRefExpr(module, named, *clause, span);
  }

  switch (ClassifyReferent(sym)) {
    case Referent::kParameterConstant:
      return MakeParameterConstantExpr(module, frame, sym, *named.type, span);
    case Referent::kEnumConstant:
      return MakeEnumConstantExpr(module, sym, *named.type, span);
    // Inside an instance method, a class property named without an explicit
    // handle (LRM 8.4) reaches the invoking object through the method's
    // receiver, so it lowers to a receiver-relative property reference.
    case Referent::kClassProperty:
      return MakeClassPropertyRefExpr(module, sym, *named.type, span);
    // Subroutine formals (LRM 13.5) and foreach iterators (LRM 12.7.3) are
    // variable-family symbols; each binds to procedural-var storage when the
    // enclosing process declares it, otherwise to the structural signal of the
    // same name reached through the reference-route translator.
    case Referent::kVariableStorage: {
      const auto& var = sym.as<slang::ast::VariableSymbol>();
      if (auto local = proc.LookupProceduralVar(var)) {
        const hir::TypeId type =
            frame.current_procedural_body->procedural_vars.Get(*local).type;
        return hir::MakeRefExpr(
            hir::ProceduralVarRef{.var = *local}, type, span);
      }
      return LowerStructuralSignalRef(
          module, frame, sym.as<slang::ast::ValueSymbol>(), *named.type, span);
    }
    // A net (LRM 6.5) is always a structural signal, never a procedural local.
    case Referent::kNetStorage:
      return LowerStructuralSignalRef(
          module, frame, sym.as<slang::ast::ValueSymbol>(), *named.type, span);
    case Referent::kUnsupported:
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedNonVariableNamedReference,
          "reference to non-variable declaration is not supported");
  }
  throw InternalError("LowerNamedValueProc: unknown Referent");
}

// LRM 23.6 hierarchical reference. A reached constant folds to its value; a
// reached signal's route is produced by the one canonical route translator from
// the reader's elaborated position and the target symbol. slang's resolved
// `ref.path` is provenance, not a routing authority, so it is not consulted.
auto LowerHierarchicalValue(
    ModuleLowerer& module, WalkFrame frame,
    const slang::ast::HierarchicalValueExpression& hve)
    -> diag::Result<hir::Expr> {
  const auto span = module.SourceMapper().SpanOf(hve.sourceRange);

  if (hve.ref.isViaIfacePort()) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "interface-port hierarchical reference is not yet supported");
  }

  const auto& target = hve.symbol;
  switch (ClassifyReferent(target)) {
    // A hierarchically reached constant folds to its value; the path is not
    // navigated because the value is fixed at elaboration.
    case Referent::kParameterConstant:
      return MakeParameterConstantExpr(module, frame, target, *hve.type, span);
    case Referent::kEnumConstant:
      return MakeEnumConstantExpr(module, target, *hve.type, span);
    case Referent::kClassProperty:
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedExpressionForm,
          "hierarchical reference to a class property is not yet supported");
    case Referent::kUnsupported:
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedExpressionForm,
          "hierarchical reference to this declaration kind is not yet "
          "supported");
    case Referent::kVariableStorage:
    case Referent::kNetStorage: {
      auto type_id = module.InternType(*hve.type, span);
      if (!type_id) return std::unexpected(std::move(type_id.error()));
      auto route = module.TranslateReferenceRoute(
          frame, target.as<slang::ast::ValueSymbol>());
      if (!route) {
        return diag::Fail(
            span, diag::DiagCode::kUnsupportedExpressionForm,
            "hierarchical reference to this target form is not yet supported");
      }
      return RouteRefExpr(*route, *type_id, span);
    }
  }
  throw InternalError("LowerHierarchicalValue: unknown Referent");
}

auto LowerNamedValueStructural(
    ModuleLowerer& module, WalkFrame frame,
    const slang::ast::NamedValueExpression& named) -> diag::Result<hir::Expr> {
  const auto& mapper = module.SourceMapper();
  const auto span = mapper.SpanOf(named.sourceRange);
  const auto& sym = named.symbol;
  if (auto clause = frame.FindIterationClause(sym)) {
    return MakeIterationElementRefExpr(module, named, *clause, span);
  }
  switch (ClassifyReferent(sym)) {
    case Referent::kParameterConstant:
      return MakeParameterConstantExpr(module, frame, sym, *named.type, span);
    case Referent::kEnumConstant:
      return MakeEnumConstantExpr(module, sym, *named.type, span);
    case Referent::kVariableStorage:
    case Referent::kNetStorage:
      return LowerStructuralSignalRef(
          module, frame, sym.as<slang::ast::ValueSymbol>(), *named.type, span);
    // A class property has no structural (non-procedural) meaning: it is only
    // reachable through a method receiver, so outside a method it is rejected
    // alongside the genuinely unsupported kinds.
    case Referent::kClassProperty:
    case Referent::kUnsupported:
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedNonVariableNamedReference,
          "reference to non-variable declaration is not supported");
  }
  throw InternalError("LowerNamedValueStructural: unknown Referent");
}

}  // namespace lyra::lowering::ast_to_hir
