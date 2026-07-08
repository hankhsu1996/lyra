#include "lyra/lowering/ast_to_hir/expression/references.hpp"

#include <cstdint>
#include <expected>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/HierarchicalReference.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/symbols/ClassSymbols.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/MemberSymbols.h>
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

// A static data object (LRM 6.21) reached through the structural scope, by the
// hop distance from the referrer's frame to the frame that owns it.
auto BindStructuralDataObject(
    ModuleLowerer& module, WalkFrame frame, const slang::ast::ValueSymbol& sym,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  const auto binding = module.LookupStructuralDataObjectBinding(sym);
  if (!binding.has_value()) {
    throw InternalError(
        "BindStructuralDataObject: value was not bound during scope lowering");
  }
  const auto hops = frame.HopsTo(binding->home_frame);
  if (!hops.has_value()) {
    throw InternalError(
        "BindStructuralDataObject: value home frame is not on the current "
        "scope stack");
  }
  return hir::MakeRefExpr(
      hir::StructuralDataObjectRef{.hops = *hops, .var = binding->var_id},
      binding->type, span);
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
    // enclosing process declares it, otherwise to the static data object of the
    // same name reached through the structural scope.
    case Referent::kVariableStorage: {
      const auto& var = sym.as<slang::ast::VariableSymbol>();
      if (auto local = proc.LookupProceduralVar(var)) {
        const hir::TypeId type =
            frame.current_procedural_body->procedural_vars.Get(*local).type;
        return hir::MakeRefExpr(
            hir::ProceduralVarRef{.var = *local}, type, span);
      }
      return BindStructuralDataObject(
          module, frame, sym.as<slang::ast::ValueSymbol>(), span);
    }
    // A net (LRM 6.5) is always a structural signal, never a procedural local.
    case Referent::kNetStorage:
      return BindStructuralDataObject(
          module, frame, sym.as<slang::ast::ValueSymbol>(), span);
    case Referent::kUnsupported:
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedNonVariableNamedReference,
          "reference to non-variable declaration is not supported");
  }
  throw InternalError("LowerNamedValueProc: unknown Referent");
}

// LRM 23.6 hierarchical reference. A downward path is rooted in a local
// owned child identified by binding; an upward path carries an anchor (a
// `$root` token or a canonical named head with any per-dimension index) and
// a descent suffix that walks by name from the anchor down to the leaf
// signal.
auto LowerHierarchicalValue(
    ModuleLowerer& module, WalkFrame frame,
    const slang::ast::HierarchicalValueExpression& hve)
    -> diag::Result<hir::Expr> {
  const auto span = module.SourceMapper().SpanOf(hve.sourceRange);
  const auto& ref = hve.ref;

  if (ref.isViaIfacePort()) {
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
    case Referent::kNetStorage:
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedExpressionForm,
          "hierarchical reference to a net is not yet supported");
    case Referent::kUnsupported:
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedExpressionForm,
          "hierarchical reference to this declaration kind is not yet "
          "supported");
    case Referent::kVariableStorage:
      break;
  }

  auto type_id = module.InternType(*hve.type, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));

  const auto& var = target.as<slang::ast::VariableSymbol>();
  const slang::ast::Symbol& head_sym = *ref.path.front().symbol;

  // ref.path is slang's resolved top-down navigation: path.front() is the
  // head, path.back() is `target`, and each name selector is the resolved
  // symbol's canonical name. Carrying those selectors verbatim into the HIR
  // descent makes the runtime by-name navigation hit each scope by the same
  // key the parent registered under. Index selectors on the head itself are
  // extracted by the caller (into the head struct's `head_indices`) before
  // `build_path` runs, so every path segment here starts with a name selector.
  const auto build_path =
      [&](std::span<const slang::ast::HierarchicalReference::Element> steps)
      -> diag::Result<std::vector<hir::PathSegment>> {
    std::vector<hir::PathSegment> path;
    path.reserve(steps.size());
    for (const auto& step : steps) {
      if (std::holds_alternative<std::string_view>(step.selector)) {
        path.push_back(
            hir::PathSegment{
                .name = std::string{std::get<std::string_view>(step.selector)},
                .indices = {}});
      } else if (std::holds_alternative<std::int32_t>(step.selector)) {
        if (path.empty()) {
          throw InternalError(
              "build_path: index selector precedes any named segment; head "
              "indices must be extracted by the caller before invoking");
        }
        path.back().indices.push_back(
            static_cast<std::uint32_t>(std::get<std::int32_t>(step.selector)));
      } else {
        return diag::Fail(
            span, diag::DiagCode::kUnsupportedExpressionForm,
            "instance-array range select in a hierarchical path is not yet "
            "supported");
      }
    }
    return path;
  };

  // Consumes the run of index selectors that immediately follows the head
  // element; each such index selects a per-dimension element on the head
  // itself (e.g. `g[2]` when `g` is an instance / generate array).
  const auto extract_head_indices =
      [](std::span<const slang::ast::HierarchicalReference::Element>&
             suffix_steps) -> std::vector<std::uint32_t> {
    std::vector<std::uint32_t> indices;
    while (!suffix_steps.empty() && std::holds_alternative<std::int32_t>(
                                        suffix_steps.front().selector)) {
      indices.push_back(
          static_cast<std::uint32_t>(
              std::get<std::int32_t>(suffix_steps.front().selector)));
      suffix_steps = suffix_steps.subspan(1);
    }
    return indices;
  };

  std::span<const slang::ast::HierarchicalReference::Element> suffix_steps =
      ref.path.subspan(1);
  std::vector<std::uint32_t> head_indices = extract_head_indices(suffix_steps);
  auto path = build_path(suffix_steps);
  if (!path) return std::unexpected(std::move(path.error()));

  // Route by segment layout visibility (LRM 23.6), not slang's lexical up/down
  // classification. A head that is an owned child of an enclosing scope in this
  // unit -- a generate block reachable by a typed climb, or any owned child in
  // the referrer's own scope -- takes the typed downward route, regardless of
  // the order the child and the referrer appear in source. A head this unit
  // does not own (an ancestor-unit instance, `$root`, or an owned module
  // instance reached from a nested generate whose body is another unit) is
  // reached by name across the boundary.
  const auto binding = module.LookupOwnedChildBinding(head_sym);
  const bool head_is_intra_unit =
      head_sym.kind == slang::ast::SymbolKind::GenerateBlock ||
      head_sym.kind == slang::ast::SymbolKind::GenerateBlockArray ||
      head_sym.kind == slang::ast::SymbolKind::StatementBlock;
  std::optional<hir::StructuralHops> down_hops;
  if (binding.has_value()) {
    if (const auto hops = frame.HopsTo(binding->home_frame);
        hops.has_value() && (hops->value == 0 || head_is_intra_unit)) {
      down_hops = hops;
    }
  }

  if (!down_hops.has_value()) {
    hir::CrossUnitRefHead anchor;
    switch (head_sym.kind) {
      case slang::ast::SymbolKind::Instance:
      case slang::ast::SymbolKind::InstanceArray:
      case slang::ast::SymbolKind::GenerateBlock:
        anchor = hir::UpwardNamedHead{
            .head_name = std::string{head_sym.name},
            .head_indices = std::move(head_indices)};
        break;
      case slang::ast::SymbolKind::Root:
        anchor = hir::UpwardRootHead{};
        break;
      default:
        return diag::Fail(
            span, diag::DiagCode::kUnsupportedExpressionForm,
            "hierarchical reference whose head is not a module instance, a "
            "named generate block, or $root is not yet supported");
    }
    return module.MakeCrossUnitMemberRef(
        var, frame.Current(), std::move(anchor), std::move(*path), *type_id,
        span);
  }

  hir::DownwardHead head = binding->head;
  head.hops = *down_hops;
  head.head_indices = std::move(head_indices);
  const ScopeFrameId slot_owner =
      down_hops->value == 0 ? binding->home_frame : frame.Current();
  return module.MakeCrossUnitMemberRef(
      var, slot_owner, std::move(head), std::move(*path), *type_id, span);
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
      return BindStructuralDataObject(
          module, frame, sym.as<slang::ast::ValueSymbol>(), span);
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
