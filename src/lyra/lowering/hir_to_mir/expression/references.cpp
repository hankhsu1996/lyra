#include "lyra/lowering/hir_to_mir/expression/references.hpp"

#include <cstdint>
#include <string_view>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/integral_constant.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/endpoint.hpp"
#include "lyra/lowering/hir_to_mir/expression/operators.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/real_literal.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/static_var_binding.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/unit_object_access.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto LowerHirIntegerLiteral(
    const UnitLowerer& unit_lowerer, const WalkFrame& frame,
    const hir::IntegerLiteral& i, mir::TypeId type) -> mir::Expr {
  mir::Block& block = *frame.current_block;
  return block.exprs.Get(BuildIntegralLiteral(
      unit_lowerer.Unit(), block, type, LowerHirIntegralConstant(i.value)));
}

// LRM 5.9: a string literal's bytes as the integer constant they denote, the
// first byte most significant.
auto StringBytesToConstant(
    std::string_view text, const mir::IntegralType& integral)
    -> mir::IntegralConstant {
  mir::IntegralConstant held = mir::BlankIntegralConstant(integral);
  value::FromBytes(
      value::Planes{.value = held.value_words, .unknown = held.state_words},
      integral.bit_width, text);
  return held;
}

// An SV string literal is a packed bit-vector constant (LRM 5.9), so MIR
// carries it as the integer constant of its bytes. A string-typed literal (a
// string parameter's value) instead builds a `value::String` via the
// constructor.
auto LowerHirStringLiteral(
    const UnitLowerer& unit_lowerer, const WalkFrame& frame,
    const hir::StringLiteral& s, mir::TypeId type) -> mir::Expr {
  const auto& ty = unit_lowerer.Unit().types.Get(type);
  auto& block = *frame.current_block;
  if (ty.IsIntegral()) {
    return block.exprs.Get(BuildIntegralLiteral(
        unit_lowerer.Unit(), block, type,
        StringBytesToConstant(s.value, ty.Integral())));
  }
  const mir::ExprId lit = block.exprs.Add(
      mir::Expr{.data = mir::StringLiteral{.value = s.value}, .type = type});
  return mir::Expr{
      .data = mir::CallExpr{.callee = mir::Construct{}, .arguments = {lit}},
      .type = type};
}

auto LowerHirNullLiteral(mir::TypeId type) -> mir::Expr {
  return mir::Expr{.data = mir::NullLiteral{}, .type = type};
}

// LRM 8.11 `this`: the handle referring to the object the subroutine was
// invoked on. The body holds a borrowed pointer to that object, which serves
// every member access, and a handle is not something that pointer can be read
// as -- so it is produced by an operation taking the receiver rather than by
// loading anything through it.
auto LowerHirThisHandle(const WalkFrame& frame, mir::TypeId type) -> mir::Expr {
  const mir::ExprId self_ref = frame.current_block->exprs.Add(
      MakeSelfRefExpr(frame, frame.current_class->self_pointer_type));
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = support::BuiltinFn::kSelfHandle},
              .arguments = {self_ref}},
      .type = type};
}

// LRM 7.10.1 `$`: the index of the last element of the queue the enclosing
// select is taken from, which is one less than how many elements it holds. The
// select evaluated the queue once, and this reads that.
auto LowerHirQueueLastIndex(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, mir::TypeId type)
    -> mir::Expr {
  if (frame.selected_queue == nullptr) {
    throw InternalError(
        "LowerHirQueueLastIndex: `$` stands under no select taken from a "
        "queue");
  }
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& block = *frame.current_block;
  const mir::ExprId size = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kSize,
                          .receiver = PathValue(
                              unit, block,
                              NamedIn(*frame.selected_queue, block))},
                  .arguments = {}},
          .type = type});
  return BuildMirBinaryExpr(
      unit, block, hir::BinaryOp::kSub, size, BuildIntLiteral(unit, block, 1),
      type);
}

auto LowerHirRealLiteral(
    const UnitLowerer& unit_lowerer, const WalkFrame& frame,
    const hir::RealLiteral& r, mir::TypeId type) -> mir::Expr {
  mir::Block& block = *frame.current_block;
  return block.exprs.Get(
      BuildRealLiteral(unit_lowerer.Unit(), block, type, r.value.Value()));
}

// A value reference reaches its endpoint's observable cell as an lvalue; the
// dispatcher dereferences it to reach the storage the cell stands for. Every
// route funnels through the one endpoint binding, so read and write share
// exactly the reach that observation does.
auto LowerRoutedValueRefExpr(
    const StructuralScopeLowerer& lowerer, const WalkFrame& frame,
    const hir::RoutedValueRef& reference) -> mir::Expr {
  return EndpointCellExpr(
      frame, lowerer.Owner().Unit(), BindEndpoint(lowerer, frame, reference));
}

auto LowerRoutedObjectRefExpr(
    const StructuralScopeLowerer& lowerer, const WalkFrame& frame,
    const hir::RoutedObjectRef& reference, mir::TypeId type) -> mir::Expr {
  return InterfaceValueOf(
      lowerer.Owner().Unit(), *frame.current_block,
      lowerer.RouteEnd(frame, reference.id), type);
}

// The pattern that declares the identifier is its binding origin, so the read
// is the ordinary body-binding read of that origin -- the clause lowering
// materialized it before the arm it guards was walked.
auto LowerPatternVarRefExpr(WalkFrame frame, const hir::PatternVarRef& r)
    -> mir::Expr {
  const BodyBindingRef ref =
      frame.bindings->EnsureCarrier(BindingOriginId::Pattern(r.pattern));
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
  // The parameter's identity across bodies is its clause and role, and a
  // capture forwards it one closure boundary at a time. The read's type is the
  // resolved binding's own, so the caller supplies none.
  return frame.bindings->MakeReadExpr(
      frame.bindings->EnsureCarrier(
          BindingOriginId::Iterator(
              ref.clause.value, static_cast<std::uint32_t>(ref.role))),
      *frame.current_block);
}

}  // namespace

auto LowerProceduralVarRefExpr(
    ProcessLowerer& process, const WalkFrame& frame, hir::ProceduralVarId var)
    -> mir::Expr {
  return std::visit(
      Overloaded{
          // Storage that outlives every activation (LRM 6.21) sits wherever the
          // declaration belongs, which the binding settled; the read is that
          // one access.
          [&](const StaticVarBinding& binding) {
            return BuildStaticStorageAccess(
                process.Owner().Unit(), frame, binding.home, binding.cell_type,
                mir::EnclosingHops{});
          },
          // A lifetime-extended automatic (LRM 6.21) lives in a shared cell;
          // the read reaches it through the handle, a carrier the resolver
          // makes available in this body (a by-value, owning copy inside a
          // detached branch).
          [&](const PromotedVarBinding& promoted) {
            return PromotedVarPlace(frame, promoted);
          },
          // An ordinary automatic local: resolve its carrier in this body -- a
          // direct local in the declaring body, a captured field in a closure
          // -- and read it. The reference's value-versus-cell shape follows the
          // materialized binding's type, which the dispatcher dereferences when
          // it is a cell.
          [&](const AutomaticVarBinding&) {
            const BodyBindingRef ref =
                frame.bindings->EnsureCarrier(BindingOriginId::Procedural(var));
            return frame.bindings->MakeReadExpr(ref, *frame.current_block);
          }},
      process.LookupProceduralVar(var));
}

// A variable of a unit's namespace (LRM 26.2) is reached without a `self`-based
// route: a namespace has no instance for a receiver to arrive through. The
// result is the variable's observable-cell type, so the dispatcher reaches its
// value one dereference further, exactly as an intra-unit signal's cell.
auto LowerExternalUnitValueRefExpr(
    UnitLowerer& unit_lowerer, const hir::ExternalUnitValueRef& r)
    -> mir::Expr {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  const mir::TypeId cell_type = mir::ObservableCellOf(
      unit.types, unit_lowerer.TranslateType(r.value_type));
  // A reference into this unit's own namespace has the arena the storage lives
  // in, so it names the position; one into another unit has only the identifier
  // that unit published, and consuming that signature is what makes the unit a
  // dependency whose header and link edge the backend then emits.
  if (r.unit_name == unit.name) {
    const std::optional<mir::StaticVariableId> variable =
        mir::StaticVariableNamed(unit.named_static_variables, r.variable_name);
    if (!variable.has_value()) {
      throw InternalError(
          "LowerExternalUnitValueRefExpr: this unit's namespace publishes no "
          "variable under the identifier a reference inside it spells");
    }
    return mir::Expr{
        .data =
            mir::ReferenceExpr{
                .target = mir::StaticVariableRef{.variable = *variable}},
        .type = cell_type};
  }
  unit.ConsumeNamespaceOf(r.unit_name);
  return mir::Expr{
      .data =
          mir::ReferenceExpr{
              .target =
                  mir::ExternalUnitVariableRef{
                      .unit_name = r.unit_name,
                      .variable_name = r.variable_name}},
      .type = cell_type};
}

auto LowerHirIntegralConstant(const hir::IntegralConstant& c)
    -> mir::IntegralConstant {
  return mir::IntegralConstant{
      .value_words = c.value_words, .state_words = c.state_words};
}

// A static property (LRM 8.9) belongs to the type rather than to an object of
// it, so it is reached without a receiver. Where its cell sits follows from
// what replicates the class declaration, which the shape settled; the reference
// states how far out of this body the instance replicating it sits, where one
// does. The reference names the cell rather than the value it holds, so the
// dispatcher reads through it the way it does any other cell.
auto LowerStaticPropertyRefExpr(
    UnitLowerer& unit_lowerer, const WalkFrame& frame,
    const hir::StaticPropertyRef& r) -> mir::Expr {
  const mir::TypeId cell_type = mir::ObservableCellOf(
      unit_lowerer.Unit().types, unit_lowerer.TranslateType(r.value_type));
  const auto own_cell = [&](const hir::LocalStaticPropertyTarget& property,
                            mir::EnclosingHops hops) {
    const mir::ClassId owner = unit_lowerer.TranslateClass(property.owner);
    return BuildStaticStorageAccess(
        unit_lowerer.Unit(), frame,
        unit_lowerer.GetClassShape(owner).static_property_translation.Get(
            property.prop),
        cell_type, hops);
  };
  return std::visit(
      Overloaded{
          [&](const hir::InstanceStaticPropertyTarget& t) -> mir::Expr {
            return own_cell(t.property, mir::EnclosingHops{t.hops.value});
          },
          [&](const hir::LocalStaticPropertyTarget& t) -> mir::Expr {
            return own_cell(t, mir::EnclosingHops{});
          },
          [&](const hir::ExternalStaticPropertyTarget& t) -> mir::Expr {
            return mir::Expr{
                .data =
                    mir::ReferenceExpr{
                        .target =
                            unit_lowerer.MakeExternalStaticPropertyRef(t)},
                .type = cell_type};
          }},
      r.target);
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
            return LowerHirIntegerLiteral(
                process.Owner(), frame, i, result_type);
          },
          [&](const hir::StringLiteral& s) -> mir::Expr {
            return LowerHirStringLiteral(
                process.Owner(), frame, s, result_type);
          },
          [&](const hir::RealLiteral& r) -> mir::Expr {
            return LowerHirRealLiteral(process.Owner(), frame, r, result_type);
          },
          [&](const hir::NullLiteral&) -> mir::Expr {
            return LowerHirNullLiteral(result_type);
          },
          [&](const hir::ThisHandle&) -> mir::Expr {
            return LowerHirThisHandle(frame, result_type);
          },
          [&](const hir::QueueLastIndex&) -> mir::Expr {
            return LowerHirQueueLastIndex(process.Owner(), frame, result_type);
          },
          [&](const hir::ProceduralVarRef& l) -> mir::Expr {
            return LowerProceduralVarRefExpr(process, frame, l.var);
          },
          [&](const hir::PatternVarRef& r) -> mir::Expr {
            return LowerPatternVarRefExpr(frame, r);
          },
          [&](const hir::ClassPropertyRef& r) -> mir::Expr {
            const mir::TypeId self_type =
                frame.current_class->self_pointer_type;
            const mir::ExprId self_ref = frame.current_block->exprs.Add(
                MakeSelfRefExpr(frame, self_type));
            return BuildClassPropertyAccess(
                process, frame, self_ref, r.target, result_type);
          },
          [&](const hir::StaticPropertyRef& r) -> mir::Expr {
            return LowerStaticPropertyRefExpr(process.Owner(), frame, r);
          },
          [&](const hir::RoutedValueRef& c) -> mir::Expr {
            return LowerRoutedValueRefExpr(
                process.EnclosingScopeLowerer(), frame, c);
          },
          [&](const hir::RoutedObjectRef& o) -> mir::Expr {
            return LowerRoutedObjectRefExpr(
                process.EnclosingScopeLowerer(), frame, o, result_type);
          },
          [&](const hir::IterationBindingRef& r) -> mir::Expr {
            return LowerIterationBindingRefExpr(r, frame);
          },
          [&](const hir::ExternalUnitValueRef& r) -> mir::Expr {
            return LowerExternalUnitValueRefExpr(process.Owner(), r);
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
            return LowerHirIntegerLiteral(
                lowerer.Owner(), frame, i, result_type);
          },
          [&](const hir::StringLiteral& s) -> mir::Expr {
            return LowerHirStringLiteral(
                lowerer.Owner(), frame, s, result_type);
          },
          [&](const hir::RealLiteral& r) -> mir::Expr {
            return LowerHirRealLiteral(lowerer.Owner(), frame, r, result_type);
          },
          [&](const hir::NullLiteral&) -> mir::Expr {
            return LowerHirNullLiteral(result_type);
          },
          [](const hir::ThisHandle&) -> mir::Expr {
            throw InternalError(
                "LowerHirPrimaryExprStructural: HIR ThisHandle does not appear "
                "in structural expressions");
          },
          [&](const hir::QueueLastIndex&) -> mir::Expr {
            return LowerHirQueueLastIndex(lowerer.Owner(), frame, result_type);
          },
          [](const hir::ProceduralVarRef&) -> mir::Expr {
            throw InternalError(
                "LowerHirPrimaryExprStructural: HIR ProceduralVarRef does not "
                "appear in structural expressions");
          },
          [&](const hir::PatternVarRef& r) -> mir::Expr {
            return LowerPatternVarRefExpr(frame, r);
          },
          [](const hir::ClassPropertyRef&) -> mir::Expr {
            throw InternalError(
                "LowerHirPrimaryExprStructural: HIR ClassPropertyRef does not "
                "appear in structural expressions");
          },
          [&](const hir::StaticPropertyRef& r) -> mir::Expr {
            return LowerStaticPropertyRefExpr(lowerer.Owner(), frame, r);
          },
          [&](const hir::RoutedValueRef& c) -> mir::Expr {
            return LowerRoutedValueRefExpr(lowerer, frame, c);
          },
          [&](const hir::RoutedObjectRef& o) -> mir::Expr {
            return LowerRoutedObjectRefExpr(lowerer, frame, o, result_type);
          },
          [&](const hir::IterationBindingRef& r) -> mir::Expr {
            return LowerIterationBindingRefExpr(r, frame);
          },
          [&](const hir::ExternalUnitValueRef& r) -> mir::Expr {
            return LowerExternalUnitValueRefExpr(lowerer.Owner(), r);
          },
      },
      p);
}

}  // namespace lyra::lowering::hir_to_mir
