#include "lyra/lowering/ast_to_hir/expression/assertion_control_target.hpp"

#include <expected>
#include <optional>
#include <string_view>
#include <utility>
#include <variant>

#include <slang/ast/Expression.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/expressions/CallExpression.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/hir/expr_builders.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/ast_to_hir/instance_array_shape.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

using Target = diag::Result<std::optional<hir::ScopeArgument>>;

// An instance, reached as the object it is.
auto InstanceObject(
    UnitLowerer& unit_lowerer, WalkFrame frame,
    const slang::ast::ArbitrarySymbolExpression& named, diag::SourceSpan span)
    -> Target {
  const auto& instance = named.symbol->as<slang::ast::InstanceSymbol>();
  const RouteOrigin origin =
      unit_lowerer.StartOf(frame, *named.symbol, named.hierRef);
  auto route =
      unit_lowerer.RouteToScopeOrRefuse(frame, instance.body, origin, span);
  if (!route) return std::unexpected(std::move(route.error()));
  const hir::TypeId object_type = unit_lowerer.ScopeClassTypeOf(
      unit_lowerer.ScopeClassOfInstance(instance));
  return std::optional<hir::ScopeArgument>{unit_lowerer.MakeRoutedObjectRef(
      frame.Current(), *std::move(route), object_type)};
}

// A generate block, reached as the object it is, as an instance is.
auto GenerateBlockObject(
    UnitLowerer& unit_lowerer, WalkFrame frame,
    const slang::ast::ArbitrarySymbolExpression& named, diag::SourceSpan span)
    -> Target {
  const auto& block = named.symbol->as<slang::ast::GenerateBlockSymbol>();
  const RouteOrigin origin =
      unit_lowerer.StartOf(frame, *named.symbol, named.hierRef);
  // A loop's block named with no path wrote its select on the block itself.
  auto route =
      named.hierRef.path.empty() && block.getArrayIndex() != nullptr
          ? unit_lowerer.RouteToLoopBlockByItsIndex(frame, block, origin, span)
          : unit_lowerer.RouteToScopeOrRefuse(frame, block, origin, span);
  if (!route) return std::unexpected(std::move(route.error()));
  const hir::TypeId object_type =
      unit_lowerer.ScopeClassTypeOf(unit_lowerer.PublishedClassOf(block));
  return std::optional<hir::ScopeArgument>{unit_lowerer.MakeRoutedObjectRef(
      frame.Current(), *std::move(route), object_type)};
}

// Every instance of an array, each reached as a name selecting it reaches it:
// an element's route follows the positions it stands at (LRM 23.3.2).
auto ArrayObjects(
    UnitLowerer& unit_lowerer, WalkFrame frame,
    const slang::ast::ArbitrarySymbolExpression& named, diag::SourceSpan span)
    -> Target {
  const auto& array = named.symbol->as<slang::ast::InstanceArraySymbol>();
  const RouteOrigin origin =
      unit_lowerer.StartOf(frame, *named.symbol, named.hierRef);
  hir::RoutedObjectsRef objects;
  // An array of no element builds nothing (LRM 23.3.2), so it names nothing.
  if (const std::optional<InstanceArrayShape> shape =
          ResolveInstanceArrayShape(array)) {
    for (const slang::ast::InstanceSymbol* element : shape->elements) {
      auto route =
          unit_lowerer.RouteToScopeOrRefuse(frame, element->body, origin, span);
      if (!route) return std::unexpected(std::move(route.error()));
      const hir::TypeId object_type = unit_lowerer.ScopeClassTypeOf(
          unit_lowerer.ScopeClassOfInstance(*element));
      objects.objects.push_back(unit_lowerer.MakeRoutedObjectRef(
          frame.Current(), *std::move(route), object_type));
    }
  }
  return std::optional<hir::ScopeArgument>{std::move(objects)};
}

// Every block a loop generate construct built, in the object holding it:
// which blocks those are is that object's construction (LRM 27.4), so the
// name is the holder and the loop it published.
auto LoopBlocks(
    UnitLowerer& unit_lowerer, WalkFrame frame,
    const slang::ast::ArbitrarySymbolExpression& named, diag::SourceSpan span)
    -> Target {
  const auto& loop = named.symbol->as<slang::ast::GenerateBlockArraySymbol>();
  const slang::ast::Scope& holder = *loop.getParentScope();
  const RouteOrigin origin =
      unit_lowerer.StartOf(frame, *named.symbol, named.hierRef);
  auto route = unit_lowerer.RouteToScopeOrRefuse(frame, holder, origin, span);
  if (!route) return std::unexpected(std::move(route.error()));
  const auto [scope_class, published] = unit_lowerer.PublishedLoopOf(loop);
  const hir::TypeId holder_type = unit_lowerer.ScopeClassTypeOf(scope_class);
  return std::optional<hir::ScopeArgument>{hir::LoopBlocksRef{
      .holder = unit_lowerer.MakeRoutedObjectRef(
          frame.Current(), *std::move(route), holder_type),
      .scope_class = scope_class,
      .loop = published}};
}

// A named block or a subroutine, reached as a `disable` naming it reaches it,
// whose extent the controls registered within it stand in.
auto ExtentOf(
    UnitLowerer& unit_lowerer, WalkFrame frame,
    const slang::ast::ArbitrarySymbolExpression& named, diag::SourceSpan span)
    -> Target {
  auto target =
      unit_lowerer.DisableTargetOf(frame, *named.symbol, named.hierRef, span);
  if (!target) return std::unexpected(std::move(target.error()));
  // A task an interface leaves to a connected module that defines none has no
  // statement to control (LRM 25.7.4).
  if (!target->has_value()) return std::optional<hir::ScopeArgument>{};
  return std::optional<hir::ScopeArgument>{**std::move(target)};
}

auto NotYetSupported(diag::SourceSpan span, std::string_view what) -> Target {
  return diag::Fail(
      span, diag::DiagCode::kUnsupportedExpressionForm,
      "{} named by an assertion control task is not yet supported", what);
}

// What the scope `named` stands for, or nothing where it is one no statement
// the tasks control can stand in.
auto TargetOf(
    UnitLowerer& unit_lowerer, WalkFrame frame,
    const slang::ast::ArbitrarySymbolExpression& named, diag::SourceSpan span)
    -> Target {
  const auto nothing = [] { return Target{std::nullopt}; };
  // The front end binds every name of the list to a scope or an instance, so
  // a kind that is neither never arrives.
  const auto cannot_arrive = []() -> Target {
    throw InternalError(
        "LowerAssertionControlTarget: the front end binds each name an "
        "assertion control task's list holds to a scope or an instance");
  };
  switch (named.symbol->kind) {
    case slang::ast::SymbolKind::Instance:
      return InstanceObject(unit_lowerer, frame, named, span);
    case slang::ast::SymbolKind::GenerateBlock:
      return GenerateBlockObject(unit_lowerer, frame, named, span);
    case slang::ast::SymbolKind::StatementBlock:
    case slang::ast::SymbolKind::Subroutine:
      return ExtentOf(unit_lowerer, frame, named, span);

    // No statement the tasks control can stand in these: a clocking block
    // holds signals and declarations of sequences and properties (LRM 14.3),
    // a modport names what an interface exports, and the rest are
    // declarations of types, coverage, constraints, primitives, timing and
    // configuration, none of which holds procedural code.
    case slang::ast::SymbolKind::ClockingBlock:
    case slang::ast::SymbolKind::Sequence:
    case slang::ast::SymbolKind::Property:
    case slang::ast::SymbolKind::LetDecl:
    case slang::ast::SymbolKind::Modport:
    case slang::ast::SymbolKind::Primitive:
    case slang::ast::SymbolKind::SpecifyBlock:
    case slang::ast::SymbolKind::ConfigBlock:
    case slang::ast::SymbolKind::ConstraintBlock:
    case slang::ast::SymbolKind::CovergroupType:
    case slang::ast::SymbolKind::CovergroupBody:
    case slang::ast::SymbolKind::Coverpoint:
    case slang::ast::SymbolKind::CoverCross:
    case slang::ast::SymbolKind::CoverCrossBody:
    case slang::ast::SymbolKind::EnumType:
    case slang::ast::SymbolKind::PackedStructType:
    case slang::ast::SymbolKind::UnpackedStructType:
    case slang::ast::SymbolKind::PackedUnionType:
    case slang::ast::SymbolKind::UnpackedUnionType:
      return nothing();

    case slang::ast::SymbolKind::InstanceArray:
      return ArrayObjects(unit_lowerer, frame, named, span);
    case slang::ast::SymbolKind::GenerateBlockArray:
      return LoopBlocks(unit_lowerer, frame, named, span);
    case slang::ast::SymbolKind::ClassType:
      return NotYetSupported(span, "a class");
    case slang::ast::SymbolKind::Package:
      return NotYetSupported(span, "a package");
    case slang::ast::SymbolKind::CompilationUnit:
      return NotYetSupported(span, "a compilation unit");
    case slang::ast::SymbolKind::Root:
      return NotYetSupported(span, "'$root'");
    case slang::ast::SymbolKind::MethodPrototype:
      return NotYetSupported(span, "a method prototype");
    case slang::ast::SymbolKind::Checker:
    case slang::ast::SymbolKind::CheckerInstanceBody:
      return NotYetSupported(span, "a checker");
    case slang::ast::SymbolKind::AnonymousProgram:
      return NotYetSupported(span, "an anonymous program");
    case slang::ast::SymbolKind::RandSeqProduction:
      return NotYetSupported(span, "a production of a 'randsequence'");
    case slang::ast::SymbolKind::InstanceBody:
      return NotYetSupported(span, "the body of an instance");

    case slang::ast::SymbolKind::Unknown:
    case slang::ast::SymbolKind::Definition:
    case slang::ast::SymbolKind::DeferredMember:
    case slang::ast::SymbolKind::TransparentMember:
    case slang::ast::SymbolKind::EmptyMember:
    case slang::ast::SymbolKind::PredefinedIntegerType:
    case slang::ast::SymbolKind::ScalarType:
    case slang::ast::SymbolKind::FloatingType:
    case slang::ast::SymbolKind::EnumValue:
    case slang::ast::SymbolKind::PackedArrayType:
    case slang::ast::SymbolKind::FixedSizeUnpackedArrayType:
    case slang::ast::SymbolKind::DynamicArrayType:
    case slang::ast::SymbolKind::DPIOpenArrayType:
    case slang::ast::SymbolKind::AssociativeArrayType:
    case slang::ast::SymbolKind::QueueType:
    case slang::ast::SymbolKind::VoidType:
    case slang::ast::SymbolKind::NullType:
    case slang::ast::SymbolKind::CHandleType:
    case slang::ast::SymbolKind::StringType:
    case slang::ast::SymbolKind::EventType:
    case slang::ast::SymbolKind::UnboundedType:
    case slang::ast::SymbolKind::TypeRefType:
    case slang::ast::SymbolKind::UntypedType:
    case slang::ast::SymbolKind::SequenceType:
    case slang::ast::SymbolKind::PropertyType:
    case slang::ast::SymbolKind::VirtualInterfaceType:
    case slang::ast::SymbolKind::TypeAlias:
    case slang::ast::SymbolKind::ErrorType:
    case slang::ast::SymbolKind::ForwardingTypedef:
    case slang::ast::SymbolKind::NetType:
    case slang::ast::SymbolKind::Parameter:
    case slang::ast::SymbolKind::TypeParameter:
    case slang::ast::SymbolKind::Port:
    case slang::ast::SymbolKind::MultiPort:
    case slang::ast::SymbolKind::InterfacePort:
    case slang::ast::SymbolKind::ModportPort:
    case slang::ast::SymbolKind::ModportClocking:
    case slang::ast::SymbolKind::ExplicitImport:
    case slang::ast::SymbolKind::WildcardImport:
    case slang::ast::SymbolKind::Attribute:
    case slang::ast::SymbolKind::Genvar:
    case slang::ast::SymbolKind::ProceduralBlock:
    case slang::ast::SymbolKind::Net:
    case slang::ast::SymbolKind::Variable:
    case slang::ast::SymbolKind::FormalArgument:
    case slang::ast::SymbolKind::Field:
    case slang::ast::SymbolKind::ClassProperty:
    case slang::ast::SymbolKind::ContinuousAssign:
    case slang::ast::SymbolKind::ElabSystemTask:
    case slang::ast::SymbolKind::GenericClassDef:
    case slang::ast::SymbolKind::UninstantiatedDef:
    case slang::ast::SymbolKind::Iterator:
    case slang::ast::SymbolKind::PatternVar:
    case slang::ast::SymbolKind::DefParam:
    case slang::ast::SymbolKind::Specparam:
    case slang::ast::SymbolKind::PrimitivePort:
    case slang::ast::SymbolKind::PrimitiveInstance:
    case slang::ast::SymbolKind::AssertionPort:
    case slang::ast::SymbolKind::ClockVar:
    case slang::ast::SymbolKind::LocalAssertionVar:
    case slang::ast::SymbolKind::CheckerInstance:
    case slang::ast::SymbolKind::CoverageBin:
    case slang::ast::SymbolKind::TimingPath:
    case slang::ast::SymbolKind::PulseStyle:
    case slang::ast::SymbolKind::SystemTimingCheck:
    case slang::ast::SymbolKind::NetAlias:
      return cannot_arrive();
  }
  throw InternalError("LowerAssertionControlTarget: unknown symbol kind");
}

}  // namespace

auto IsAssertionControlCall(const slang::ast::CallExpression& call) -> bool {
  if (!call.isSystemCall()) return false;
  const auto* desc = support::FindSystemSubroutine(call.getSubroutineName());
  return desc != nullptr &&
         std::holds_alternative<support::AssertionControlSystemSubroutineInfo>(
             desc->semantic);
}

auto LowerAssertionControlTarget(
    UnitLowerer& unit_lowerer, WalkFrame frame,
    const slang::ast::ArbitrarySymbolExpression& named, diag::SourceSpan span)
    -> diag::Result<std::optional<hir::Expr>> {
  auto target = TargetOf(unit_lowerer, frame, named, span);
  if (!target) return std::unexpected(std::move(target.error()));
  if (!target->has_value()) return std::optional<hir::Expr>{};
  auto type_id = unit_lowerer.InternExpressionType(named, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return std::optional{hir::MakeRefExpr(
      hir::ScopeArgumentRef{.scope = **std::move(target)}, *type_id, span)};
}

}  // namespace lyra::lowering::ast_to_hir
