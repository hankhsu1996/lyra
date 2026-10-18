#include "lyra/lowering/hir_to_mir/continuous_assign.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <optional>
#include <span>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/overloaded.hpp"
#include "lyra/base/time.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/continuous_assign.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/closure_builder.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/lvalue.hpp"
#include "lyra/lowering/hir_to_mir/net_declaration.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/value_operations.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/callable_bounds.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// A field of the enclosing class a drive keeps: the driver a net target is
// written through, and for a drive carrying a delay the value last scheduled
// and the timer it propagates through.
struct ClassField {
  mir::FieldId field;
  mir::TypeId type;
};

auto AddField(mir::Class& mir_class, mir::TypeId type) -> ClassField {
  return ClassField{
      .field = mir_class.fields.Add(mir::FieldDecl{.type = type}),
      .type = type};
}

// The access reaching `field` of the object `frame`'s body runs on.
auto FieldOfSelf(
    const mir::CompilationUnit& unit, const WalkFrame& frame, mir::Block& block,
    const ClassField& field) -> mir::ExprId {
  const mir::ExprId self = block.exprs.Add(MakeSelfRefExpr(frame));
  return block.exprs.Add(
      mir::MakeFieldAccessExpr(
          BuildObjectDeref(unit, block, self),
          mir::ClassFieldTarget{
              .owner = mir::IntraUnitClassRef{frame.current_class_id},
              .slot = field.field},
          field.type));
}

// `target = value`, appended to `block`.
void AppendAssign(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId target,
    mir::ExprId value) {
  block.AppendStmt(
      mir::ExprStmt{
          .expr = block.exprs.Add(
              mir::MakeAssignExpr(unit.builtins, target, value))});
}

// Each place `lvalue` is made of, in the order the source wrote them.
auto PlacesOf(const Lvalue& lvalue) -> std::vector<AccessPath> {
  std::vector<AccessPath> places;
  ForEachPlace(
      lvalue, [&](const AccessPath& place) { places.push_back(place); });
  return places;
}

// States to the variable `place` lies in that a continuous assignment drives
// it (LRM 10.3). Which storage a write lands in is known only where it lands,
// so the variable is named by a reference to the whole of it; storage no
// variable holds takes the statement and has nothing to be told.
auto StateContinuousDriver(
    mir::CompilationUnit& unit, mir::Block& block, const AccessPath& place)
    -> mir::Stmt {
  const mir::ExprId whole = PathReference(
      unit, block, AccessPath{.owner = place.owner, .descent = {}});
  return mir::Stmt{
      .label = std::nullopt,
      .data = mir::ExprStmt{
          .expr = block.exprs.Add(
              mir::MakeCallExpr(
                  mir::Direct{
                      .target = support::BuiltinFn::kDrivesContinuously},
                  {whole}, unit.builtins.void_type))}};
}

// The entry telling a driver that what it gives is the whole of the object
// `source` names, unchanged: a variable, a net, or an input port declared as a
// variable, named whole, of the type the drive writes. A net this drive alone
// decides then shows that object where it lies, which only the built design
// says (LRM 23.3.3 has a port connection be a continuous assignment, and
// LRM 4.8 leaves open when its target sees the update). Any other source is
// evaluated and given.
auto CarriedObjectEntry(
    const hir::StructuralScope& scope, const hir::Expr& source,
    hir::TypeId written_type) -> std::optional<support::BuiltinFn> {
  const auto* primary = std::get_if<hir::PrimaryExpr>(&source.data);
  const auto* named = primary == nullptr
                          ? nullptr
                          : std::get_if<hir::RoutedValueRef>(&primary->data);
  if (named == nullptr || source.type != written_type) return std::nullopt;
  return std::visit(
      Overloaded{
          [](const hir::VariableStorage&) -> std::optional<support::BuiltinFn> {
            return support::BuiltinFn::kDriveFromVariable;
          },
          [](const hir::NetStorage&) -> std::optional<support::BuiltinFn> {
            return support::BuiltinFn::kDriveFromNet;
          },
          [](const hir::ReferenceStorage&)
              -> std::optional<support::BuiltinFn> {
            return support::BuiltinFn::kDriveFromReference;
          },
          [](const hir::BorrowedObjectStorage&)
              -> std::optional<support::BuiltinFn> { return std::nullopt; }},
      hir::CellOf(scope.routes.values.Get(named->id).leaf).storage);
}

// Whether `target` names the whole of a net declared with no packed
// dimension, the one target whose changes take a gate's delays (LRM 10.3.3,
// 28.16). A bit of a vector net is a scalar's worth of bits and still a part
// of a vector net.
auto IsScalarNet(
    const UnitLowerer& owner, const hir::StructuralScope& scope,
    const hir::Expr& target) -> bool {
  const auto* primary = std::get_if<hir::PrimaryExpr>(&target.data);
  const auto* named = primary == nullptr
                          ? nullptr
                          : std::get_if<hir::RoutedValueRef>(&primary->data);
  return named != nullptr &&
         std::holds_alternative<hir::NetStorage>(
             hir::CellOf(scope.routes.values.Get(named->id).leaf).storage) &&
         owner.Hir().types.Get(target.type).Is<hir::ScalarBitType>();
}

// The steps of the scope's precision one written delay amount is, read where
// `frame` stands as a delay control reads it (LRM 9.4.1, 3.14.1). Which entry
// reads it follows the amount's own type: an integral amount counts whole time
// units, a real one may name a fraction of one.
auto DelayTicksOf(
    const StructuralScopeLowerer& lowerer, const WalkFrame& frame,
    hir::ExprId amount) -> diag::Result<mir::ExprId> {
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  mir::Block& block = *frame.current_block;
  auto lowered = lowerer.LowerExpr(lowerer.HirScope().exprs.Get(amount), frame);
  if (!lowered) return std::unexpected(std::move(lowered.error()));
  mir::ExprId amount_id = block.exprs.Add(*std::move(lowered));
  const mir::Type& amount_type =
      unit.types.Get(block.exprs.Get(amount_id).type);
  const bool is_real = amount_type.IsRealFamily();
  if (amount_type.Is<mir::ShortRealType>()) {
    // LRM 6.12.1: `real` and `realtime` are one type, and a `shortreal` differs
    // from them only in host precision, so the entry takes the wider.
    amount_id = ConvertToType(unit, block, amount_id, unit.builtins.real);
  }
  const TimeResolution resolution = lowerer.HirScope().time_resolution;
  const mir::ExprId unit_power =
      BuildMachineIntLiteral(unit, block, std::int64_t{resolution.unit_power});
  const mir::ExprId precision_power = BuildMachineIntLiteral(
      unit, block, std::int64_t{resolution.precision_power});
  return block.exprs.Add(
      mir::MakeCallExpr(
          mir::Direct{
              .target = is_real ? support::BuiltinFn::kDelayTicksReal
                                : support::BuiltinFn::kDelayTicks},
          {amount_id, unit_power, precision_power},
          unit.builtins.machine_word));
}

// The delay a change of the drive to `changed_to` takes, of the delays it is
// written with, where `frame` stands (LRM 10.3.3, 28.16). One delay is every
// change's; of several, the value changed to picks, and given two the
// turn-off delay is the lesser of them.
auto PropagationDelayOf(
    const StructuralScopeLowerer& lowerer, const WalkFrame& frame,
    const DriveDelay& delay, mir::ExprId changed_to)
    -> diag::Result<mir::ExprId> {
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  mir::Block& block = *frame.current_block;
  const auto ticks = [&](hir::ExprId amount) -> diag::Result<mir::ExprId> {
    auto lowered = DelayTicksOf(lowerer, frame, amount);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    return EvaluatedOnce(frame, *lowered);
  };
  const auto pick = [&](mir::ExprId rise, mir::ExprId fall,
                        mir::ExprId turn_off) -> mir::ExprId {
    return block.exprs.Add(
        mir::MakeCallExpr(
            mir::Direct{.target = delay.propagation_delay},
            {changed_to, rise, fall, turn_off}, unit.builtins.machine_word));
  };
  return std::visit(
      Overloaded{
          [&](const hir::SingleDelay& d) -> diag::Result<mir::ExprId> {
            return DelayTicksOf(lowerer, frame, d.amount);
          },
          [&](const hir::RiseFallDelay& d) -> diag::Result<mir::ExprId> {
            auto rise = ticks(d.rise);
            if (!rise) return std::unexpected(std::move(rise.error()));
            auto fall = ticks(d.fall);
            if (!fall) return std::unexpected(std::move(fall.error()));
            const mir::ExprId rise_is_lesser = block.exprs.Add(
                mir::Expr{
                    .data =
                        mir::BinaryExpr{
                            .op = mir::BinaryOp::kLessThan,
                            .lhs = *rise,
                            .rhs = *fall},
                    .type = unit.builtins.machine_bool});
            const mir::ExprId lesser = block.exprs.Add(
                mir::Expr{
                    .data =
                        mir::ConditionalExpr{
                            .condition = rise_is_lesser,
                            .then_value = *rise,
                            .else_value = *fall},
                    .type = unit.builtins.machine_word});
            return pick(*rise, *fall, lesser);
          },
          [&](const hir::RiseFallTurnOffDelay& d) -> diag::Result<mir::ExprId> {
            auto rise = ticks(d.rise);
            if (!rise) return std::unexpected(std::move(rise.error()));
            auto fall = ticks(d.fall);
            if (!fall) return std::unexpected(std::move(fall.error()));
            auto turn_off = ticks(d.turn_off);
            if (!turn_off) return std::unexpected(std::move(turn_off.error()));
            return pick(*rise, *fall, *turn_off);
          }},
      delay.delay);
}

// Arms `timer` with the delay a change to `changed_to` takes, where `frame`
// stands: the event pending is replaced by this one.
auto AppendArm(
    const StructuralScopeLowerer& lowerer, const WalkFrame& frame,
    const DriveDelay& delay, const ClassField& timer, mir::ExprId changed_to)
    -> diag::Result<void> {
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  mir::Block& block = *frame.current_block;
  auto ticks = PropagationDelayOf(lowerer, frame, delay, changed_to);
  if (!ticks) return std::unexpected(std::move(ticks.error()));
  const mir::ExprId runtime =
      block.exprs.Add(BuildCurrentRuntimeCallExpr(lowerer.Owner()));
  const mir::ExprId precision_power = BuildMachineIntLiteral(
      unit, block,
      std::int64_t{lowerer.HirScope().time_resolution.precision_power});
  block.AppendStmt(
      mir::ExprStmt{
          .expr = block.exprs.Add(
              mir::MakeCallExpr(
                  mir::Direct{
                      .target = support::BuiltinFn::kTimerArm,
                      .receiver = FieldOfSelf(unit, frame, block, timer)},
                  {runtime, *ticks, precision_power},
                  unit.builtins.void_type))});
  return {};
}

}  // namespace

// LRM 10.3.2 (continuous assignment) and LRM 9.2.2.2.1 (always_comb) share a
// runtime mental model: re-evaluate the assignment whenever any RHS read
// changes. HIR keeps continuous assignment as a distinct scope-level node so
// source diagnostics retain provenance; at HIR -> MIR we materialise the
// runtime shape as a coroutine body `forever { <store>; wait on reads; }`,
// which the caller registers as a continuous driver. The body executes once
// when it starts (the natural fall-through of the eternal loop) before the
// first wait, which is the evaluation LRM 4.9.1 asks for to propagate constant
// values.
//
// The target is an lvalue (LRM Table 10-1), and where each store lands follows
// the type of the place it reaches. A net accepts no store, only a drive (LRM
// 6.5): the assignment acquires a driver for it at Resolve and every store
// re-roots onto that driver's contribution, so several assignments to one net
// install independent drivers the net resolves. A place that names only part
// of a net drives only that part, because the rest of its driver's
// contribution stays at the resolution identity and keeps deferring to whoever
// drives it. Every other place is written where it is named. A drive of a whole
// net from the whole of an object of its type may find at its turn that the
// net shows that object, and then ends there.
auto LowerContinuousAssign(
    const StructuralScopeLowerer& lowerer, const WalkFrame& ctor_frame,
    const WalkFrame& resolve_frame, const WalkFrame& init_frame,
    const hir::ContinuousAssign& src) -> diag::Result<mir::CallableDecl> {
  const hir::StructuralScope& hir_scope = lowerer.HirScope();
  const hir::Expr& hir_lhs = hir_scope.exprs.Get(src.lhs);
  std::optional<DriveDelay> delay;
  if (src.delay.has_value()) {
    // LRM 10.3.3 and 28.16 say which of several delays a change takes only of
    // a scalar net and a vector net, both of which hold an integral value.
    const bool chooses = !std::holds_alternative<hir::SingleDelay>(*src.delay);
    if (chooses &&
        !lowerer.Owner().Hir().types.Get(hir_lhs.type).IsIntegral()) {
      return diag::Fail(
          src.span, diag::DiagCode::kUnsupportedContinuousAssignForm,
          "rise, fall and turn-off delays on a target holding no integral "
          "value are not yet supported, since the standard chooses among them "
          "only for a scalar or a vector net");
    }
    delay = DriveDelay{
        .delay = *src.delay,
        .propagation_delay = IsScalarNet(lowerer.Owner(), hir_scope, hir_lhs)
                                 ? support::BuiltinFn::kScalarPropagationDelay
                                 : support::BuiltinFn::kVectorPropagationDelay};
  }
  return LowerContinuousDrive(
      lowerer, ctor_frame, resolve_frame, init_frame,
      [&](const WalkFrame& frame) {
        return LowerLvalue(lowerer, hir_lhs, frame);
      },
      hir_scope.exprs.Get(src.rhs), src.strength, delay, src.sensitivity_list);
}

auto LowerContinuousDrive(
    const StructuralScopeLowerer& lowerer, const WalkFrame& ctor_frame,
    const WalkFrame& resolve_frame, const WalkFrame& init_frame,
    const DriveDestination& destination, const hir::Expr& source,
    support::StrengthLevel drive_strength,
    const std::optional<DriveDelay>& delay,
    std::span<const hir::SensitivityEntry> sensitivity)
    -> diag::Result<mir::CallableDecl> {
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  const mir::TypeId self_ptr_type = ctor_frame.current_class->self_pointer_type;

  // What each place of the target is driven through: a net accepts no store,
  // only a drive, so each place that is one acquires a driver in Resolve, held
  // in a field of the enclosing class, and a variable has none. The place's
  // root decides it: the source may have named a whole net or a part of one,
  // and a part of a net is still a net.
  std::vector<std::optional<ClassField>> drivers;
  // The driver of a whole net this drive gives the whole of an object to,
  // which is told so as the design is built and asked at its turn whether its
  // net shows that object.
  std::optional<ClassField> carrying;
  {
    mir::Block& resolve_block = *resolve_frame.current_block;
    auto named = destination(resolve_frame);
    if (!named) return std::unexpected(std::move(named.error()));
    ForEachPlace(*named, [&](const AccessPath& place_path) {
      // A property of an object is a variable's storage, never a net's, so
      // only a place can be a net.
      const auto* place = std::get_if<mir::ExprId>(&place_path.owner);
      const auto* net =
          place == nullptr
              ? nullptr
              : unit.types.Get(resolve_block.exprs.Get(*place).type)
                    .As<mir::ResolvedType>();
      if (net == nullptr) {
        drivers.emplace_back(std::nullopt);
        return;
      }
      const mir::ExprId cell = *place;
      const ClassField driver = AddField(
          *resolve_frame.current_class,
          unit.types.Intern(mir::Type{mir::DriverType{.value = net->value}}));
      const mir::ExprId strength =
          BuildStrengthOperand(unit, resolve_block, drive_strength);
      const mir::ExprId attach = resolve_block.exprs.Add(
          mir::MakeNetAttachDriverCallExpr(cell, strength, driver.type));
      AppendAssign(
          unit, resolve_block,
          FieldOfSelf(unit, resolve_frame, resolve_block, driver), attach);
      drivers.emplace_back(driver);
    });

    // A drive carrying a delay gives what its source held a while ago, and so
    // does a program's own, which runs once the design has settled while what
    // it reads may be written there (LRM 24.3.1); in neither does the source
    // show what the drive gives, so no net shows its source in its place.
    const auto* whole = std::get_if<AccessPath>(&named->form);
    const std::optional<support::BuiltinFn> carried =
        delay.has_value() || hir::IsProgram(lowerer.Owner().Hir().role) ||
                whole == nullptr || !whole->descent.empty() ||
                !drivers.front().has_value()
            ? std::nullopt
            : CarriedObjectEntry(
                  lowerer.HirScope(), source, named->source_type);
    if (carried.has_value()) {
      auto object = lowerer.LowerAccessPath(source, resolve_frame);
      if (!object) return std::unexpected(std::move(object.error()));
      const mir::ExprId place = OwnerPlace(*object);
      const mir::TypeId pointer_type = unit.types.Intern(
          mir::Type{mir::PointerType{
              .pointee = resolve_block.exprs.Get(place).type,
              .ownership = mir::PointerOwnership::kBorrowed}});
      carrying = drivers.front();
      resolve_block.AppendStmt(
          mir::ExprStmt{
              .expr = resolve_block.exprs.Add(
                  mir::MakeDriveFromCallExpr(
                      FieldOfSelf(
                          unit, resolve_frame, resolve_block, *carrying),
                      resolve_block.exprs.Add(
                          mir::MakeAddressOfExpr(place, pointer_type)),
                      *carried, unit.builtins.void_type))});
    }
  }

  // Writes `value` to the target where `frame` stands: each place its share, a
  // net's re-rooted onto its driver's contribution.
  const auto store = [&](const WalkFrame& frame,
                         mir::ExprId value) -> diag::Result<void> {
    mir::Block& block = *frame.current_block;
    auto named = destination(frame);
    if (!named) return std::unexpected(std::move(named.error()));
    auto shares = Shares(lowerer.Owner(), frame, *named, value);
    if (!shares) return std::unexpected(std::move(shares.error()));
    for (std::size_t i = 0; i < shares->size(); ++i) {
      Share& share = (*shares)[i];
      const std::optional<ClassField>& driver = drivers[i];
      if (driver.has_value()) {
        share.place.owner = FieldOfSelf(unit, frame, block, *driver);
      }
      block.AppendStmt(
          mir::ExprStmt{
              .expr = block.exprs.Add(
                  BuildStoreExpr(unit, block, share.place, share.value))});
    }
    return {};
  };
  // The right-hand side, read where `frame` stands.
  const auto evaluate =
      [&](const WalkFrame& frame) -> diag::Result<mir::ExprId> {
    auto value = lowerer.LowerExpr(source, frame);
    if (!value) return std::unexpected(std::move(value.error()));
    mir::Block& block = *frame.current_block;
    return block.AsValue(unit.types, block.exprs.Add(*std::move(value)));
  };

  // A variable shows, once nothing overrides it, what its continuous driver
  // last produced (LRM 10.6.2), so the variable each place lies in is told it
  // has one. It is told while the scope initializes, by when every reference a
  // place may be reached through is bound. A net keeps each driver's
  // contribution by itself.
  {
    mir::Block& init_block = *init_frame.current_block;
    auto named = destination(init_frame);
    if (!named) return std::unexpected(std::move(named.error()));
    const std::vector<AccessPath> places = PlacesOf(*named);
    for (std::size_t i = 0; i < places.size(); ++i) {
      if (drivers[i].has_value()) continue;
      init_block.AppendStmt(StateContinuousDriver(unit, init_block, places[i]));
    }
  }

  mir::CallableCode code = mir::CallableCode::Defined();
  CallableBindings bindings(unit, code);
  const mir::LocalId self_id =
      bindings.Declare(ReceiverOrigin{}, self_ptr_type);
  const WalkFrame entry_frame =
      ctor_frame.WithBindings(&bindings).WithBlock(&code.Body());

  mir::Block body_block;
  const WalkFrame body_frame = entry_frame.WithBlock(&body_block);

  if (!delay.has_value()) {
    auto value = evaluate(body_frame);
    if (!value) return std::unexpected(std::move(value.error()));
    if (auto stored = store(body_frame, *value); !stored) {
      return std::unexpected(std::move(stored.error()));
    }
    auto waited =
        BuildValueChangeWaitStmt(body_block, body_frame, lowerer, sensitivity);
    if (!waited) return std::unexpected(std::move(waited.error()));
    body_block.AppendStmt(*std::move(waited));
  } else {
    // LRM 10.3.3: each value computed reaches the target the delay it takes
    // later, unless a different one is computed first. The value last
    // scheduled is kept, and a timer holds the one propagation pending, which
    // arming it again replaces; a change to the value already on its way puts
    // nothing off. The timer's firing writes the kept value, which is always
    // the pending one's, so it is bound once rather than handed a value per
    // change.
    mir::Block& entry = code.Body();
    auto first = evaluate(entry_frame);
    if (!first) return std::unexpected(std::move(first.error()));
    mir::Class& mir_class = *ctor_frame.current_class;
    const ClassField scheduled =
        AddField(mir_class, entry.exprs.Get(*first).type);
    const ClassField timer = AddField(
        mir_class, unit.types.Intern(
                       mir::Type{mir::RuntimeLibraryType{
                           .kind = mir::RuntimeLibraryKind::kTimer}}));

    ClosureBuilder propagation(unit, entry_frame, support::ClosureKind::kFn);
    if (auto stored = store(
            propagation.Frame(),
            propagation.Body().AsValue(
                unit.types,
                FieldOfSelf(
                    unit, propagation.Frame(), propagation.Body(), scheduled)));
        !stored) {
      return std::unexpected(std::move(stored.error()));
    }
    const mir::ExprId fire =
        BuildKeptClosure(unit, entry, propagation.BuildVoid());
    entry.AppendStmt(
        mir::ExprStmt{
            .expr = entry.exprs.Add(
                mir::MakeCallExpr(
                    mir::Direct{
                        .target = support::BuiltinFn::kTimerBind,
                        .receiver =
                            FieldOfSelf(unit, entry_frame, entry, timer)},
                    {fire}, unit.builtins.void_type))});

    // LRM 4.9.1: the evaluation at time zero is scheduled whatever it is, as
    // an assignment with no delay stores it whatever it is. What it schedules
    // that the driver already gives lands as no change.
    AppendAssign(
        unit, entry, FieldOfSelf(unit, entry_frame, entry, scheduled), *first);
    if (auto armed = AppendArm(
            lowerer, entry_frame, *delay, timer,
            entry.AsValue(
                unit.types, FieldOfSelf(unit, entry_frame, entry, scheduled)));
        !armed) {
      return std::unexpected(std::move(armed.error()));
    }

    auto waited =
        BuildValueChangeWaitStmt(body_block, body_frame, lowerer, sensitivity);
    if (!waited) return std::unexpected(std::move(waited.error()));
    body_block.AppendStmt(*std::move(waited));
    auto value = evaluate(body_frame);
    if (!value) return std::unexpected(std::move(value.error()));
    const mir::TypeId value_type = scheduled.type;
    const mir::LocalId changed_to = DeclareLocal(body_frame, *value);
    // A value differs from the one scheduled where its bits do -- an unknown
    // bit matching only the same unknown, a real its own pattern -- which is
    // what decides whether a write changes a variable, too (LRM 4.3).
    const mir::ExprId differs = BuildConditionNot(
        unit, body_block,
        BuildValueOperation(
            unit, body_block, support::BuiltinFn::kBitIdentical,
            body_block.ReadLocal(changed_to, value_type),
            {body_block.AsValue(
                unit.types,
                FieldOfSelf(unit, body_frame, body_block, scheduled))},
            unit.builtins.machine_bool));
    mir::Block rescheduled;
    const WalkFrame rescheduled_frame = body_frame.WithBlock(&rescheduled);
    AppendAssign(
        unit, rescheduled,
        FieldOfSelf(unit, rescheduled_frame, rescheduled, scheduled),
        rescheduled.ReadLocal(changed_to, value_type));
    if (auto armed = AppendArm(
            lowerer, rescheduled_frame, *delay, timer,
            rescheduled.ReadLocal(changed_to, value_type));
        !armed) {
      return std::unexpected(std::move(armed.error()));
    }
    body_block.AppendStmt(
        mir::IfStmt{
            .condition = differs,
            .then_scope = body_block.child_scopes.Add(std::move(rescheduled)),
            .else_scope = std::nullopt});
  }

  // A drive carrying the whole of an object first asks, at its turn, whether
  // its net now shows that object, which leaves it nothing to do.
  if (carrying.has_value()) {
    mir::Block& entry = code.Body();
    mir::Block leave;
    leave.AppendStmt(mir::ReturnStmt{.value = std::nullopt});
    const mir::ExprId shows = entry.exprs.Add(
        mir::MakeShowSourceCallExpr(
            FieldOfSelf(unit, entry_frame, entry, *carrying),
            unit.builtins.machine_bool));
    entry.AppendStmt(
        mir::IfStmt{
            .condition = shows,
            .then_scope = entry.child_scopes.Add(std::move(leave)),
            .else_scope = std::nullopt});
  }

  const mir::BlockId body_scope_id =
      code.Body().child_scopes.Add(std::move(body_block));
  code.Body().AppendStmt(
      mir::ForStmt{
          .init = {},
          .condition = std::nullopt,
          .step = {},
          .scope = body_scope_id});
  code.Body().AppendStmt(mir::ReturnStmt{.value = std::nullopt});
  code.params = {self_id};
  code.result_type = unit.builtins.coroutine_void;
  return mir::CallableDecl{
      .code = std::move(code),
      .foreign = std::nullopt,
      .virtual_dispatch = std::nullopt};
}

}  // namespace lyra::lowering::hir_to_mir
