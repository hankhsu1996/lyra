#include "lyra/lowering/hir_to_mir/expression/references.hpp"

#include <cstdint>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/integral_constant.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/endpoint.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto LowerTimeScale(hir::TimeScale s) -> mir::TimeScale {
  switch (s) {
    case hir::TimeScale::kFs:
      return mir::TimeScale::kFs;
    case hir::TimeScale::kPs:
      return mir::TimeScale::kPs;
    case hir::TimeScale::kNs:
      return mir::TimeScale::kNs;
    case hir::TimeScale::kUs:
      return mir::TimeScale::kUs;
    case hir::TimeScale::kMs:
      return mir::TimeScale::kMs;
    case hir::TimeScale::kS:
      return mir::TimeScale::kS;
  }
  throw InternalError("LowerTimeScale: unknown HIR TimeScale");
}

auto LowerSignedness(hir::Signedness s) -> mir::Signedness {
  switch (s) {
    case hir::Signedness::kSigned:
      return mir::Signedness::kSigned;
    case hir::Signedness::kUnsigned:
      return mir::Signedness::kUnsigned;
  }
  throw InternalError("LowerSignedness: unknown HIR Signedness");
}

auto LowerStateKind(hir::IntegralStateKind k) -> mir::IntegralStateKind {
  switch (k) {
    case hir::IntegralStateKind::kTwoState:
      return mir::IntegralStateKind::kTwoState;
    case hir::IntegralStateKind::kFourState:
      return mir::IntegralStateKind::kFourState;
  }
  throw InternalError("LowerStateKind: unknown HIR IntegralStateKind");
}

auto LowerHirIntegerLiteral(const hir::IntegerLiteral& i, mir::TypeId type)
    -> mir::Expr {
  return mir::Expr{
      .data = mir::IntegerLiteral{.value = LowerHirIntegralConstant(i.value)},
      .type = type};
}

// LRM 5.9: pack a string literal's bytes into the integer constant they denote,
// the first byte most significant. A byte is 8-bit-aligned, so it never
// straddles a 64-bit word.
auto StringBytesToConstant(
    std::string_view text, const mir::PackedArrayType& pa)
    -> mir::IntegralConstant {
  const auto width = static_cast<std::uint32_t>(pa.BitWidth());
  std::vector<std::uint64_t> value_words((width + 63U) / 64U, 0U);
  for (std::size_t i = 0; i < text.size(); ++i) {
    const std::uint64_t bit_offset = width - (8U * (i + 1U));
    value_words[bit_offset / 64U] |=
        static_cast<std::uint64_t>(static_cast<unsigned char>(text[i]))
        << (bit_offset % 64U);
  }
  return mir::IntegralConstant{
      .value_words = std::move(value_words),
      .state_words = {},
      .width = width,
      .signedness = pa.signedness,
      .state_kind = mir::IntegralStateKind::kTwoState,
  };
}

// An SV string literal is a packed bit-vector constant (LRM 5.9), so MIR
// carries it as the integer constant of its bytes. A string-typed literal (a
// string parameter's value) instead builds a `value::String` via the
// constructor.
auto LowerHirStringLiteral(
    const UnitLowerer& unit_lowerer, const WalkFrame& frame,
    const hir::StringLiteral& s, mir::TypeId type) -> mir::Expr {
  const auto& ty = unit_lowerer.Unit().types.Get(type);
  if (ty.IsIntegralPacked()) {
    return mir::Expr{
        .data =
            mir::IntegerLiteral{
                .value = StringBytesToConstant(s.value, ty.AsIntegralPacked())},
        .type = type};
  }
  // A string-typed literal (e.g. a string parameter's value) builds a
  // `value::String` from the software literal via the constructor, on the
  // block that holds the surrounding expression.
  auto& block = *frame.current_block;
  const mir::ExprId lit = block.exprs.Add(
      mir::Expr{.data = mir::StringLiteral{.value = s.value}, .type = type});
  return mir::Expr{
      .data = mir::CallExpr{.callee = mir::Construct{}, .arguments = {lit}},
      .type = type};
}

auto LowerHirTimeLiteral(
    const UnitLowerer& unit_lowerer, const hir::TimeLiteral& t) -> mir::Expr {
  return mir::Expr{
      .data =
          mir::TimeLiteral{.value = t.value, .scale = LowerTimeScale(t.scale)},
      .type = unit_lowerer.Unit().builtins.realtime};
}

auto LowerHirNullLiteral(mir::TypeId type) -> mir::Expr {
  return mir::Expr{.data = mir::NullLiteral{}, .type = type};
}

auto LowerHirRealLiteral(const hir::RealLiteral& r, mir::TypeId type)
    -> mir::Expr {
  return mir::Expr{.data = mir::RealLiteral{.value = r.value}, .type = type};
}

// A direct or routed reference reaches its endpoint's observable cell as an
// lvalue; the dispatcher wraps it with the read (`kGet`) or write path. Both
// route kinds funnel through the one endpoint binding, so read and write share
// exactly the reach that observation does.
auto LowerReferenceRouteExpr(
    const StructuralScopeLowerer& lowerer, const WalkFrame& frame,
    const hir::ReferenceRoute& route) -> mir::Expr {
  return EndpointCellExpr(
      frame, lowerer.Owner().Unit(), BindEndpoint(lowerer, frame, route));
}

// A package variable (LRM 26.2) is reached by name, never through a
// `self`-based route: the same by-name form serves a referrer in another unit
// and the package's own callable reading its own variable. The result is the
// variable's observable-cell type, so the dispatcher wraps the read in `kGet`
// and a write in `kSet`, exactly as an intra-unit signal's cell. A referrer in
// another unit records the dependency so the backend emits the include and link
// edge; a package's own reference to its own variable records nothing.
auto LowerExternalUnitValueRefExpr(
    mir::CompilationUnit& unit, const hir::ExternalUnitValueRef& r,
    mir::TypeId value_type) -> mir::Expr {
  if (r.unit_name != unit.name) {
    unit.AddExternalReferencedUnit(r.unit_name);
  }
  return mir::Expr{
      .data =
          mir::ExternalUnitVariableRef{
              .unit_name = r.unit_name, .variable_name = r.variable_name},
      .type = unit.types.ObservableCellOf(value_type)};
}

auto LowerProceduralVarRefExpr(
    ProcessLowerer& process, const WalkFrame& frame,
    const hir::ProceduralVarRef& l, mir::TypeId type) -> mir::Expr {
  // Storage placement (static, LRM 13.3.1) and body-local environment
  // (automatic / promoted) are separate authorities. Presence in the
  // body-local environment is the authoritative signal: a body-declared
  // static is not registered there and resolves through the storage plan,
  // while subroutine params / output / result_var ARE registered (regardless
  // of their slang-reported lifetime) and resolve through it.
  const auto* binding = process.LookupProceduralVar(l.var);
  if (binding == nullptr) {
    return process.BuildStaticStorageAccess(
        frame, process.LookupStaticPlacement(l.var));
  }
  // A lifetime-extended automatic (LRM 6.21) lives in a shared activation
  // frame; the read / write reaches its field through the handle. The handle
  // is a carrier the resolver makes available in this body (a by-value, owning
  // copy inside a detached branch), and the promoted field projects from it.
  if (const auto* pb = std::get_if<PromotedVarBinding>(binding)) {
    const BodyBindingRef handle =
        frame.bindings->EnsureCarrier(pb->handle_origin);
    const mir::ExprId handle_ref = frame.current_block->exprs.Add(
        frame.bindings->MakeReadExpr(handle, *frame.current_block));
    return mir::MakeFieldAccessExpr(handle_ref, pb->field, type);
  }
  // An ordinary automatic local: resolve its carrier in this body -- a direct
  // local in the declaring body, a captured field in a closure -- and read it.
  // The reference's value-versus-cell shape follows the materialized binding's
  // type, which the dispatcher unwraps with `kGet` when it is a cell.
  const BodyBindingRef ref =
      frame.bindings->EnsureCarrier(BindingOriginId::Procedural(l.var));
  return frame.bindings->MakeReadExpr(ref, *frame.current_block);
}

// LRM 7.12.4: a with-clause iteration reference reads the named clause's
// element or index closure parameter. The parameter is found by clause
// identity, then resolved by the same rule as any body-local: read directly
// when the reference is in the clause's own closure, captured when it is inside
// a deeper clause's closure (the parameter's declaration sits above that
// closure's boundary).
auto LowerIterationBindingRefExpr(
    const hir::IterationBindingRef& ref, WalkFrame frame) -> mir::Expr {
  // The with-clause parameter's cross-body identity is its clause id and role;
  // `EnsureCarrier` reads it directly in the clause's own closure or captures
  // it
  // -- forwarding one boundary at a time -- when the reference sits inside a
  // deeper clause's closure (LRM 7.12.4). The result type comes from the
  // resolved binding's arena, so no caller-supplied type is needed.
  return frame.bindings->MakeReadExpr(
      frame.bindings->EnsureCarrier(
          BindingOriginId::Iterator(
              ref.clause.value, static_cast<std::uint32_t>(ref.role))),
      *frame.current_block);
}

}  // namespace

auto LowerHirIntegralConstant(const hir::IntegralConstant& c)
    -> mir::IntegralConstant {
  return mir::IntegralConstant{
      .value_words = c.value_words,
      .state_words = c.state_words,
      .width = c.width,
      .signedness = LowerSignedness(c.signedness),
      .state_kind = LowerStateKind(c.state_kind),
  };
}

auto LowerHirPrimaryExprProc(
    ProcessLowerer& process, WalkFrame frame, const hir::Primary& p,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  // A reference to an enclosing-scope declaration resolves against the body's
  // structural scope; it appears only in a structural body, so the scope is
  // reached lazily and a class method body never requests it.
  return std::visit(
      Overloaded{
          [&](const hir::IntegerLiteral& i) -> mir::Expr {
            return LowerHirIntegerLiteral(i, result_type);
          },
          [&](const hir::StringLiteral& s) -> mir::Expr {
            return LowerHirStringLiteral(
                process.Owner(), frame, s, result_type);
          },
          [&](const hir::TimeLiteral& t) -> mir::Expr {
            return LowerHirTimeLiteral(process.Owner(), t);
          },
          [&](const hir::RealLiteral& r) -> mir::Expr {
            return LowerHirRealLiteral(r, result_type);
          },
          [&](const hir::NullLiteral&) -> mir::Expr {
            return LowerHirNullLiteral(result_type);
          },
          [&](const hir::DirectMemberRef& m) -> mir::Expr {
            return LowerReferenceRouteExpr(
                process.EnclosingScopeLowerer(), frame, hir::ReferenceRoute{m});
          },
          [&](const hir::ProceduralVarRef& l) -> mir::Expr {
            return LowerProceduralVarRefExpr(process, frame, l, result_type);
          },
          [&](const hir::ClassPropertyRef& r) -> mir::Expr {
            const mir::TypeId self_type =
                frame.current_class->self_pointer_type;
            const mir::ExprId self_ref = frame.current_block->exprs.Add(
                MakeSelfRefExpr(frame, self_type));
            return mir::MakeFieldAccessExpr(
                self_ref,
                mir::FieldTarget{
                    .owner = process.Owner().TranslateClass(r.owner),
                    .slot = UnitLowerer::TranslateField(r.field_index)},
                result_type);
          },
          [&](const hir::StaticPropertyRef& r) -> mir::Expr {
            return mir::Expr{
                .data =
                    mir::StaticPropertyRef{
                        .owner = process.Owner().TranslateClass(r.owner),
                        .prop = UnitLowerer::TranslateStaticProperty(r.prop),
                    },
                .type = result_type};
          },
          [&](const hir::RoutedRef& c) -> mir::Expr {
            return LowerReferenceRouteExpr(
                process.EnclosingScopeLowerer(), frame, hir::ReferenceRoute{c});
          },
          [&](const hir::IterationBindingRef& r) -> mir::Expr {
            return LowerIterationBindingRefExpr(r, frame);
          },
          [&](const hir::ExternalUnitValueRef& r) -> mir::Expr {
            return LowerExternalUnitValueRefExpr(
                process.Owner().Unit(), r, result_type);
          },
      },
      p);
}

auto LowerHirPrimaryExprStructural(
    const StructuralScopeLowerer& lowerer, WalkFrame frame,
    const hir::Primary& p, mir::TypeId result_type) -> diag::Result<mir::Expr> {
  return std::visit(
      Overloaded{
          [&](const hir::IntegerLiteral& i) -> mir::Expr {
            return LowerHirIntegerLiteral(i, result_type);
          },
          [&](const hir::StringLiteral& s) -> mir::Expr {
            return LowerHirStringLiteral(
                lowerer.Owner(), frame, s, result_type);
          },
          [&](const hir::TimeLiteral& t) -> mir::Expr {
            return LowerHirTimeLiteral(lowerer.Owner(), t);
          },
          [&](const hir::RealLiteral& r) -> mir::Expr {
            return LowerHirRealLiteral(r, result_type);
          },
          [&](const hir::NullLiteral&) -> mir::Expr {
            return LowerHirNullLiteral(result_type);
          },
          [&](const hir::DirectMemberRef& m) -> mir::Expr {
            return LowerReferenceRouteExpr(
                lowerer, frame, hir::ReferenceRoute{m});
          },
          [](const hir::ProceduralVarRef&) -> mir::Expr {
            throw InternalError(
                "LowerHirPrimaryExprStructural: HIR ProceduralVarRef does not "
                "appear in structural expressions");
          },
          [](const hir::ClassPropertyRef&) -> mir::Expr {
            throw InternalError(
                "LowerHirPrimaryExprStructural: HIR ClassPropertyRef does not "
                "appear in structural expressions");
          },
          [](const hir::StaticPropertyRef&) -> mir::Expr {
            throw InternalError(
                "LowerHirPrimaryExprStructural: HIR StaticPropertyRef does not "
                "appear in structural expressions");
          },
          [&](const hir::RoutedRef& c) -> mir::Expr {
            return LowerReferenceRouteExpr(
                lowerer, frame, hir::ReferenceRoute{c});
          },
          [&](const hir::IterationBindingRef& r) -> mir::Expr {
            return LowerIterationBindingRefExpr(r, frame);
          },
          [&](const hir::ExternalUnitValueRef& r) -> mir::Expr {
            return LowerExternalUnitValueRefExpr(
                lowerer.Owner().Unit(), r, result_type);
          },
      },
      p);
}

}  // namespace lyra::lowering::hir_to_mir
