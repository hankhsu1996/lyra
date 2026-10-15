#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"

#include <cstdint>
#include <expected>
#include <optional>
#include <span>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/reads_storage_only.hpp"
#include "lyra/hir/timing.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/endpoint.hpp"
#include "lyra/lowering/hir_to_mir/expression/references.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/object_change.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/subroutine_call.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/unit_object_access.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// Where a routed leaf's cell is, reached from the scope the route is counted
// from: a scope's own lowering is that scope, and a procedural body's is the
// scope enclosing the body. Only a route asks.
auto BindRouted(
    const StructuralScopeLowerer& scope, const WalkFrame& frame,
    const hir::RoutedValueRef& reference) -> BoundEndpoint {
  return BindEndpoint(scope, frame, reference);
}
auto BindRouted(
    const ProcessLowerer& process, const WalkFrame& frame,
    const hir::RoutedValueRef& reference) -> BoundEndpoint {
  return BindEndpoint(process.EnclosingScopeLowerer(), frame, reference);
}

// What a wait on a variable the running body declares enrols on: its cell,
// or, for a `ref` formal, whatever a write through it is told to (LRM 13.5.2).
// Only a procedural body declares one.
template <typename Lowerer>
auto BodyVariableSource(
    Lowerer& lowerer, const WalkFrame& frame, mir::Block& block,
    const hir::ProceduralVarRef& var) -> mir::ExprId {
  if constexpr (!std::same_as<Lowerer, ProcessLowerer>) {
    throw InternalError(
        "BodyVariableSource: a wait outside a procedural body named a variable "
        "of one");
  } else {
    mir::CompilationUnit& unit = lowerer.Owner().Unit();
    const hir::ProceduralVarDecl& decl =
        lowerer.HirBody().procedural_vars.Get(var.var);
    auto lowered = LowerHirPrimaryExprProc(
        lowerer, frame.WithBlock(&block), hir::Primary{var},
        lowerer.Owner().TranslateType(decl.type));
    if (!lowered) {
      throw InternalError(
          "BodyVariableSource: a variable the body declares failed to lower as "
          "the storage a wait enrols on");
    }
    const mir::ExprId storage = block.exprs.Add(*std::move(lowered));
    const mir::TypeId storage_type = block.exprs.Get(storage).type;
    const mir::Type& storage_form = unit.types.Get(storage_type);
    if (storage_form.IsReference()) {
      return ReferenceReportsTo(unit, block, storage);
    }
    // A closure's snapshot of a variable is the closure's own copy, which no
    // other process reaches, so nothing that could change it is ever told.
    if (!storage_form.Is<mir::ObservableType>()) {
      return block.exprs.Add(
          mir::Expr{
              .data = mir::NullLiteral{},
              .type = mir::ErasedPointer(unit.types)});
    }
    return block.exprs.Add(
        mir::MakeAddressOfExpr(
            storage, unit.types.Intern(
                         mir::Type{mir::PointerType{
                             .pointee = storage_type,
                             .ownership = mir::PointerOwnership::kBorrowed,
                             .mutability = mir::Mutability::kMutable}})));
  }
}

}  // namespace

template <typename Lowerer>
auto BuildObservableCellExpr(
    mir::Block& block, const WalkFrame& frame, mir::CompilationUnit& unit,
    Lowerer& lowerer, const hir::ValueTarget& cell) -> mir::ExprId {
  return std::visit(
      Overloaded{
          [&](const hir::RoutedValueRef& reference) -> mir::ExprId {
            return block.exprs.Add(EndpointCellExpr(
                frame, unit, BindRouted(lowerer, frame, reference)));
          },
          [&](const hir::ExternalUnitValueRef& pkg) -> mir::ExprId {
            return block.exprs.Add(
                LowerExternalUnitValueRefExpr(lowerer.Owner(), pkg));
          },
          [&](const hir::StaticPropertyRef& property) -> mir::ExprId {
            return block.exprs.Add(
                LowerStaticPropertyRefExpr(lowerer.Owner(), frame, property));
          },
      },
      cell);
}

auto BuildReportCall(
    const mir::CompilationUnit& unit, mir::Block& block, mir::LocalId report,
    support::BuiltinFn entry, std::vector<mir::ExprId> arguments,
    mir::TypeId type) -> mir::ExprId {
  const mir::ExprId object = BuildObjectDeref(
      unit, block,
      block.exprs.Add(
          mir::MakeLocalRefExpr(report, unit.builtins.read_report_ptr)));
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Direct{.target = entry, .receiver = object},
                  .arguments = std::move(arguments)},
          .type = type});
}

namespace {

// What a wait's trigger hands the runtime: a borrowed pointer to what reports a
// write there. A route answers for this form itself, because an endpoint that
// reached out of the unit already holds a pointer and composing one from the
// cell would send that case through a dereference and back; so does whatever
// is found through a handle or a reference. A cell reached as itself answers
// with its address, and a net with what it shows its value through.
template <typename Lowerer>
auto BuildObservablePtrExpr(
    mir::Block& block, const WalkFrame& frame, mir::CompilationUnit& unit,
    Lowerer& lowerer, const hir::SensitivityEntry& entry) -> mir::ExprId {
  const auto address_of_cell =
      [&](const hir::ValueTarget& sealed) -> mir::ExprId {
    const mir::ExprId cell =
        BuildObservableCellExpr(block, frame, unit, lowerer, sealed);
    const mir::TypeId ptr_type = unit.types.Intern(
        mir::Type{mir::PointerType{
            .pointee = block.exprs.Get(cell).type,
            .ownership = mir::PointerOwnership::kBorrowed,
            .mutability = mir::Mutability::kMutable}});
    return block.exprs.Add(mir::MakeAddressOfExpr(cell, ptr_type));
  };
  const auto sealed_cell = [&](const hir::ValueTarget& sealed) -> mir::ExprId {
    return std::visit(
        Overloaded{
            [&](const hir::RoutedValueRef& reference) -> mir::ExprId {
              return EndpointObservablePtr(
                  block, frame, unit, BindRouted(lowerer, frame, reference));
            },
            [&](const hir::ExternalUnitValueRef&) -> mir::ExprId {
              return address_of_cell(sealed);
            },
            [&](const hir::StaticPropertyRef&) -> mir::ExprId {
              return address_of_cell(sealed);
            },
        },
        sealed);
  };
  const mir::ExprId reached = std::visit(
      Overloaded{
          [&](const hir::ValueTarget& sealed) -> mir::ExprId {
            return sealed_cell(sealed);
          },
          [&](const hir::ProceduralVarRef& var) -> mir::ExprId {
            return BodyVariableSource(lowerer, frame, block, var);
          }},
      entry.cell);
  // A net shows its own value or the object its one driver carries, which
  // only the built design says, so a wait on it is enrolled through what it
  // shows its value by, as a wait through a reference is.
  const auto* pointer =
      unit.types.Get(block.exprs.Get(reached).type).As<mir::PointerType>();
  if (pointer == nullptr ||
      !unit.types.Get(pointer->pointee).Is<mir::ResolvedType>()) {
    return reached;
  }
  const mir::ExprId net = block.exprs.Add(
      mir::Expr{
          .data = mir::DerefExpr{.pointer = reached},
          .type = pointer->pointee});
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{.target = support::BuiltinFn::kNetReportsTo},
                  .arguments = {net}},
          .type = mir::ErasedPointer(unit.types)});
}

// Which part of a place a leaf watches, as a wait's trigger and a report take
// it (LRM 9.4.2 / 9.4.2.2 / 9.4.3, 9.2.2.2.1): the steps from the place to the
// part, and which bits of the part where it is a packed value or which
// elements where it is an array. A
// read of the whole of the part is a width of zero, which is also what a named
// event and an object carry, having no bits at all.
struct LeafPart {
  // One array of machine integers, empty for the whole place.
  mir::ExprId steps;
  mir::ExprId first;
  mir::ExprId width;
  // Whether that is the part itself, and not a larger part that holds it.
  bool exact = true;
};

// `steps` as the one array a leaf or a report is handed.
auto StepArray(
    const mir::CompilationUnit& unit, mir::Block& block,
    std::vector<mir::ExprId> steps) -> mir::ExprId {
  const mir::TypeId array = mir::MachineArrayOf(
      unit.types, unit.builtins.machine_int64, steps.size());
  return block.exprs.Add(
      mir::Expr{
          .data = mir::CompositeExpr{.parts = std::move(steps)},
          .type = array});
}

auto WholePlace(const mir::CompilationUnit& unit, mir::Block& block)
    -> LeafPart {
  return LeafPart{
      .steps = StepArray(unit, block, {}),
      .first = BuildMachineIntLiteral(unit, block, 0),
      .width = BuildMachineIntLiteral(unit, block, 0)};
}

template <typename Lowerer>
auto LeafPartOf(
    mir::Block& block, const WalkFrame& frame, mir::CompilationUnit& unit,
    Lowerer& lowerer, const hir::WatchedPart& part) -> diag::Result<LeafPart> {
  const auto int_literal = [&](std::uint64_t value) {
    return BuildMachineIntLiteral(
        unit, block, static_cast<std::int64_t>(value));
  };
  return std::visit(
      Overloaded{
          [&](const hir::WatchedWhole&) -> diag::Result<LeafPart> {
            return WholePlace(unit, block);
          },
          [&](const hir::WatchedSelect& select) -> diag::Result<LeafPart> {
            auto part = lowerer.LowerAccessPath(
                lowerer.HirExprs().Get(select.prefix), frame.WithBlock(&block));
            if (!part) return std::unexpected(std::move(part.error()));
            NamedPart named = PartNamedBy(unit, block, *part);
            const mir::ExprId steps =
                StepArray(unit, block, std::move(named.steps));
            if (!named.positions.has_value()) {
              return LeafPart{
                  .steps = steps,
                  .first = int_literal(0),
                  .width = int_literal(0),
                  .exact = named.exact};
            }
            return LeafPart{
                .steps = steps,
                .first = BuildToInt64Call(unit, block, named.positions->first),
                .width = int_literal(named.positions->width),
                .exact = named.exact};
          },
      },
      part);
}

// One leaf of the wait: the place it watches, what decides whether what happens
// there is an event, and which bits of that place's packed encoding it reads.
template <typename Lowerer>
auto BuildTriggerExpr(
    mir::Block& block, const WalkFrame& frame, mir::CompilationUnit& unit,
    Lowerer& lowerer, const hir::SensitivityEntry& entry,
    mir::LocalId observation) -> diag::Result<mir::ExprId> {
  const mir::ExprId observable_ptr =
      BuildObservablePtrExpr(block, frame, unit, lowerer, entry);
  auto watched = LeafPartOf(block, frame, unit, lowerer, entry.part);
  if (!watched) return std::unexpected(std::move(watched.error()));
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Construct{},
                  .arguments =
                      {observable_ptr,
                       block.exprs.Add(
                           mir::MakeLocalRefExpr(
                               observation, unit.builtins.observation)),
                       watched->steps, watched->first, watched->width}},
          .type = unit.builtins.trigger});
}

// Appends `entry(arguments)`, acting on the report, to `block`.
void ActOnReport(
    const mir::CompilationUnit& unit, mir::Block& block, mir::LocalId report,
    support::BuiltinFn entry, std::vector<mir::ExprId> arguments) {
  block.AppendStmt(
      mir::ExprStmt{
          .expr = BuildReportCall(
              unit, block, report, entry, std::move(arguments),
              unit.builtins.void_type)});
}

// The cell `cell` names as a report is told of it: what a write there reports
// to, and which part of it.
struct ReportedCell {
  mir::ExprId place;
  LeafPart part;
};

template <typename Lowerer>
auto ReportedCellOf(
    Lowerer& lowerer, const WalkFrame& frame, const hir::SensitivityEntry& cell)
    -> diag::Result<ReportedCell> {
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  mir::Block& block = *frame.current_block;
  const mir::ExprId place =
      BuildObservablePtrExpr(block, frame, unit, lowerer, cell);
  auto part = LeafPartOf(block, frame, unit, lowerer, cell.part);
  if (!part) return std::unexpected(std::move(part.error()));
  return ReportedCell{.place = place, .part = *part};
}

// Tells the report of `cell`, read or written as `entry` says.
void ReportCell(
    const mir::CompilationUnit& unit, mir::Block& block, mir::LocalId report,
    support::BuiltinFn entry, const ReportedCell& cell) {
  ActOnReport(
      unit, block, report, entry,
      {cell.place, cell.part.steps, cell.part.first, cell.part.width});
}

// Records a place reached through a handle -- an object a chain passes
// through, by its event source, or a variable of the instance a virtual
// interface holds -- and which bits of it are read.
void ReportThroughHandle(
    const mir::CompilationUnit& unit, mir::Block& block, mir::LocalId report,
    mir::ExprId place, LeafPart watched) {
  ActOnReport(
      unit, block, report, support::BuiltinFn::kReadReportAddThroughHandle,
      {place, watched.steps, watched.first, watched.width});
}

// Appends `if (holds(local)) { fill(inner) }` to `frame`'s block: what reaches
// through a handle is reached only where the handle names something, because a
// function's report stands where nothing has tested the handle (LRM 8.4).
template <typename Fill>
auto WhereItNamesSomething(
    const mir::CompilationUnit& unit, const WalkFrame& frame,
    mir::LocalId handle, mir::TypeId handle_type, Fill fill)
    -> diag::Result<void> {
  mir::Block& block = *frame.current_block;
  mir::Block inner;
  auto filled = fill(frame.WithBlock(&inner));
  if (!filled) return std::unexpected(std::move(filled.error()));
  const mir::ExprId held =
      block.exprs.Add(mir::MakeLocalRefExpr(handle, handle_type));
  block.AppendStmt(
      mir::IfStmt{
          .condition = ReduceToCondition(unit, block, held),
          .then_scope = block.child_scopes.Add(std::move(inner)),
          .else_scope = std::nullopt});
  return {};
}

template <typename Lowerer>
auto ReportChainFrom(
    Lowerer& lowerer, const WalkFrame& frame, mir::LocalId report,
    mir::LocalId handle, mir::TypeId handle_type,
    std::span<const hir::ObjectChain::Hop> hops) -> diag::Result<void>;

// Records the object `object` reaches, then the ones the chain's remaining hops
// reach from it. `object` is read afresh each time it is needed, as a handle or
// as the running method's own receiver.
template <typename Lowerer, typename ReadObject>
auto ReportObjectThenHops(
    Lowerer& lowerer, const WalkFrame& frame, mir::LocalId report,
    ReadObject object, std::span<const hir::ObjectChain::Hop> hops)
    -> diag::Result<void> {
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  mir::Block& block = *frame.current_block;
  ReportThroughHandle(
      unit, block, report, ObjectEventSourceOf(unit, block, object()),
      WholePlace(unit, block));
  if (hops.empty()) return {};
  const hir::ObjectChain::Hop& hop = hops.front();
  const mir::TypeId next_type = lowerer.Owner().TranslateType(hop.handle_type);
  const mir::LocalId next = DeclareLocal(
      frame, block.exprs.Add(BuildClassPropertyAccess(
                 lowerer, frame, object(), hop.property, next_type)));
  return ReportChainFrom(
      lowerer, frame, report, next, next_type, hops.subspan(1));
}

// Records the object `handle` names and what the remaining hops reach from it,
// where the handle names something at all.
template <typename Lowerer>
auto ReportChainFrom(
    Lowerer& lowerer, const WalkFrame& frame, mir::LocalId report,
    mir::LocalId handle, mir::TypeId handle_type,
    std::span<const hir::ObjectChain::Hop> hops) -> diag::Result<void> {
  return WhereItNamesSomething(
      lowerer.Owner().Unit(), frame, handle, handle_type,
      [&](const WalkFrame& inner) -> diag::Result<void> {
        return ReportObjectThenHops(
            lowerer, inner, report,
            [&] {
              return inner.current_block->exprs.Add(
                  mir::MakeLocalRefExpr(handle, handle_type));
            },
            hops);
      });
}

template <typename Lowerer>
auto ReportLeaf(
    Lowerer& lowerer, const WalkFrame& frame, mir::LocalId report,
    const hir::WaitLeaf& leaf) -> diag::Result<void> {
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  mir::Block& block = *frame.current_block;
  return std::visit(
      Overloaded{
          [&](const hir::SensitivityEntry& cell) -> diag::Result<void> {
            auto read = ReportedCellOf(lowerer, frame, cell);
            if (!read) return std::unexpected(std::move(read.error()));
            ReportCell(
                unit, block, report, support::BuiltinFn::kReadReportAdd, *read);
            return {};
          },
          // A variable of the instance a virtual interface holds (LRM 25.9),
          // whose address the instance answers with.
          [&](const hir::InterfaceMemberAccessExpr& held)
              -> diag::Result<void> {
            auto handle = lowerer.LowerExpr(
                lowerer.HirExprs().Get(held.instance.handle), frame);
            if (!handle) return std::unexpected(std::move(handle.error()));
            const mir::TypeId handle_type = handle->type;
            const mir::LocalId bound =
                DeclareLocal(frame, block.exprs.Add(*std::move(handle)));
            return WhereItNamesSomething(
                unit, frame, bound, handle_type,
                [&](const WalkFrame& inner) -> diag::Result<void> {
                  auto place = HeldInterfaceMember(lowerer, inner, held);
                  if (!place) return std::unexpected(std::move(place.error()));
                  ReportThroughHandle(
                      unit, *inner.current_block, report, *place,
                      WholePlace(unit, *inner.current_block));
                  return {};
                });
          },
          [&](const hir::ObjectChain& chain) -> diag::Result<void> {
            return std::visit(
                Overloaded{
                    [&](hir::ExprId handle) -> diag::Result<void> {
                      auto root = lowerer.LowerExpr(
                          lowerer.HirExprs().Get(handle), frame);
                      if (!root) {
                        return std::unexpected(std::move(root.error()));
                      }
                      const mir::TypeId root_type = root->type;
                      const mir::LocalId bound = DeclareLocal(
                          frame, block.exprs.Add(*std::move(root)));
                      return ReportChainFrom(
                          lowerer, frame, report, bound, root_type, chain.hops);
                    },
                    // A method runs on an object, so there is nothing to
                    // test before reaching it.
                    [&](const hir::ReceiverObject&) -> diag::Result<void> {
                      return ReportObjectThenHops(
                          lowerer, frame, report,
                          [&] {
                            return block.exprs.Add(MakeSelfRefExpr(
                                frame, frame.current_class->self_pointer_type));
                          },
                          chain.hops);
                    }},
                chain.root);
          },
          [&](const hir::EveryObject&) -> diag::Result<void> {
            ActOnReport(
                unit, block, report,
                support::BuiltinFn::kReadReportAddEveryObject, {});
            return {};
          },
      },
      leaf);
}

// `value` as the evaluation `frame` is lowering goes on to read through it,
// having stated the whole of the place `place_of` makes of it as reached, where
// the evaluation states what it reaches at all.
template <typename PlaceOf>
auto Reported(
    const mir::CompilationUnit& unit, const WalkFrame& frame, mir::ExprId value,
    PlaceOf place_of) -> mir::ExprId {
  if (!frame.reports_reached_to.has_value()) {
    return value;
  }
  mir::Block& block = *frame.current_block;
  const mir::ExprId held = EvaluatedOnce(frame, value);
  ReportThroughHandle(
      unit, block, *frame.reports_reached_to, place_of(held),
      WholePlace(unit, block));
  return held;
}

}  // namespace

auto ReportedObject(
    mir::CompilationUnit& unit, const WalkFrame& frame, mir::ExprId object)
    -> mir::ExprId {
  return Reported(unit, frame, object, [&](mir::ExprId held) {
    return ObjectEventSourceOf(unit, *frame.current_block, held);
  });
}

auto ReportedPlace(
    const mir::CompilationUnit& unit, const WalkFrame& frame, mir::ExprId place)
    -> mir::ExprId {
  return Reported(unit, frame, place, [](mir::ExprId held) { return held; });
}

template <typename Lowerer>
auto ReportCells(
    Lowerer& lowerer, const WalkFrame& frame, mir::LocalId report,
    std::span<const hir::SensitivityEntry> cells) -> diag::Result<void> {
  for (const hir::SensitivityEntry& cell : cells) {
    auto reported = ReportLeaf(lowerer, frame, report, hir::WaitLeaf{cell});
    if (!reported) return std::unexpected(std::move(reported.error()));
  }
  return {};
}

template <typename Lowerer>
auto ReportReads(
    Lowerer& lowerer, const WalkFrame& frame, const hir::Reads& reads,
    mir::LocalId report) -> diag::Result<void> {
  for (const hir::WaitLeaf& leaf : reads.leaves) {
    auto reported = ReportLeaf(lowerer, frame, report, leaf);
    if (!reported) return std::unexpected(std::move(reported.error()));
  }
  for (const hir::SensitivityEntry& write : reads.writes) {
    auto written = ReportedCellOf(lowerer, frame, write);
    if (!written) return std::unexpected(std::move(written.error()));
    // A write is taken out of what the list reads (LRM 9.2.2.2.1), so one
    // named by a larger part than the one it reaches is left out: it would
    // take out what the procedure did not write.
    if (!written->part.exact) continue;
    ReportCell(
        lowerer.Owner().Unit(), *frame.current_block, report,
        support::BuiltinFn::kReadReportAddWrite, *written);
  }
  for (const hir::ReportingCall& call : reads.calls) {
    auto made = EmitReportingCall(lowerer, frame, call, report);
    if (!made) return std::unexpected(std::move(made.error()));
  }
  return {};
}

auto DeclareObservation(
    const mir::CompilationUnit& unit, const WalkFrame& frame, mir::Block& block,
    support::BuiltinFn entry, std::vector<mir::ExprId> arguments)
    -> mir::LocalId {
  return DeclareLocal(
      frame.WithBlock(&block),
      block.exprs.Add(
          mir::MakeCallExpr(
              mir::Direct{.target = entry}, std::move(arguments),
              unit.builtins.observation)));
}

auto WaitStorageBlock(
    const WalkFrame& frame, mir::Block& stop_block,
    std::span<const hir::SensitivityEntry> cells,
    const base::Arena<hir::Expr, hir::ExprId>& exprs,
    std::span<const hir::ExprId> evaluated) -> mir::Block& {
  const bool watches_a_body_variable =
      std::ranges::any_of(cells, [](const hir::SensitivityEntry& cell) {
        return std::visit(
            Overloaded{
                [](const hir::ValueTarget&) { return false; },
                [](const hir::ProceduralVarRef&) { return true; }},
            cell.cell);
      });
  const bool evaluates_a_body_variable =
      !std::ranges::all_of(evaluated, [&](hir::ExprId expr) {
        return hir::ReadsElaboratedStorageOnly(exprs, expr);
      });
  return watches_a_body_variable || evaluates_a_body_variable
             ? stop_block
             : frame.bindings->RootBlock();
}

namespace {

// One trigger per leaf, as the machine array a wait is built on, built in
// `target_block`. A leaf watching part of its cell names the part by the
// select the source wrote, whose indices are constants and are lowered here;
// `lowerer` is the lowering that owns those expressions.
template <typename Lowerer>
auto BuildTriggerArray(
    mir::Block& target_block, const WalkFrame& frame, Lowerer& lowerer,
    std::span<const ObservedLeaf> leaves) -> diag::Result<mir::ExprId> {
  auto& unit = lowerer.Owner().Unit();
  std::vector<mir::ExprId> triggers;
  triggers.reserve(leaves.size());
  for (const ObservedLeaf& leaf : leaves) {
    auto trigger = BuildTriggerExpr(
        target_block, frame, unit, lowerer, leaf.entry, leaf.observation);
    if (!trigger) return std::unexpected(std::move(trigger.error()));
    triggers.push_back(*trigger);
  }
  const mir::TypeId triggers_type =
      mir::MachineArrayOf(unit.types, unit.builtins.trigger, triggers.size());
  return target_block.exprs.Add(
      mir::Expr{
          .data = mir::CompositeExpr{.parts = std::move(triggers)},
          .type = triggers_type});
}

}  // namespace

template <typename Lowerer>
auto BuildWaitOnStmt(
    mir::Block& storage_block, mir::Block& stop_block, const WalkFrame& frame,
    Lowerer& lowerer, std::span<const ObservedLeaf> leaves)
    -> diag::Result<mir::Stmt> {
  const WalkFrame storage_frame = frame.WithBlock(&storage_block);
  auto triggers =
      BuildTriggerArray(storage_block, storage_frame, lowerer, leaves);
  if (!triggers) return std::unexpected(std::move(triggers.error()));
  const mir::LocalId wait = DeclareLocal(
      storage_frame,
      storage_block.exprs.Add(
          mir::MakeCallExpr(
              mir::Direct{.target = support::BuiltinFn::kWaitOn}, {*triggers},
              lowerer.Owner().Unit().builtins.wait)));
  return BuildParkStmt(lowerer.Owner(), stop_block, wait);
}

template <typename Lowerer>
auto BuildValueChangeWaitStmt(
    mir::Block& stop_block, const WalkFrame& frame, Lowerer& lowerer,
    std::span<const hir::SensitivityEntry> sensitivity_list)
    -> diag::Result<mir::Stmt> {
  mir::Block& storage_block = WaitStorageBlock(
      frame, stop_block, sensitivity_list, lowerer.HirExprs(), {});
  const mir::LocalId observation = DeclareObservation(
      lowerer.Owner().Unit(), frame, storage_block,
      support::BuiltinFn::kObservationOnReaching, {});
  std::vector<ObservedLeaf> leaves;
  leaves.reserve(sensitivity_list.size());
  for (const hir::SensitivityEntry& entry : sensitivity_list) {
    leaves.push_back(ObservedLeaf{.entry = entry, .observation = observation});
  }
  return BuildWaitOnStmt(storage_block, stop_block, frame, lowerer, leaves);
}

auto HoldImplicitList(
    const WalkFrame& frame, ProcessLowerer& lowerer, const hir::Reads& reads)
    -> diag::Result<mir::LocalId> {
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  mir::Block& block = *frame.current_block;
  // A report no evaluation makes, so a function called to report into it runs
  // nothing of its body.
  const mir::LocalId report = DeclareLocal(
      frame,
      block.exprs.Add(
          mir::MakeCallExpr(
              mir::Direct{
                  .target = support::BuiltinFn::kReadReportForImplicitList},
              {}, unit.builtins.read_report)));
  const mir::LocalId pointer = DeclareLocal(
      frame,
      block.exprs.Add(
          mir::MakeAddressOfExpr(
              block.exprs.Add(
                  mir::MakeLocalRefExpr(report, unit.builtins.read_report)),
              unit.builtins.read_report_ptr)));
  auto reported = ReportReads(lowerer, frame, reads, pointer);
  if (!reported) return std::unexpected(std::move(reported.error()));
  ActOnReport(
      unit, block, pointer, support::BuiltinFn::kReadReportSettleAsImplicitList,
      {});
  return DeclareLocal(
      frame,
      block.exprs.Add(
          mir::MakeCallExpr(
              mir::Direct{.target = support::BuiltinFn::kWaitOnImplicitList},
              {block.exprs.Add(
                  mir::MakeLocalRefExpr(
                      pointer, unit.builtins.read_report_ptr))},
              unit.builtins.wait)));
}

// One instantiation per lowering a wait is built in.
template auto BuildObservableCellExpr(
    mir::Block&, const WalkFrame&, mir::CompilationUnit&, ProcessLowerer&,
    const hir::ValueTarget&) -> mir::ExprId;
template auto BuildObservableCellExpr(
    mir::Block&, const WalkFrame&, mir::CompilationUnit&,
    const StructuralScopeLowerer&, const hir::ValueTarget&) -> mir::ExprId;
template auto ReportReads(
    ProcessLowerer&, const WalkFrame&, const hir::Reads&, mir::LocalId)
    -> diag::Result<void>;
template auto ReportReads(
    const StructuralScopeLowerer&, const WalkFrame&, const hir::Reads&,
    mir::LocalId) -> diag::Result<void>;
template auto BuildWaitOnStmt(
    mir::Block&, mir::Block&, const WalkFrame&, ProcessLowerer&,
    std::span<const ObservedLeaf>) -> diag::Result<mir::Stmt>;
template auto BuildWaitOnStmt(
    mir::Block&, mir::Block&, const WalkFrame&, const StructuralScopeLowerer&,
    std::span<const ObservedLeaf>) -> diag::Result<mir::Stmt>;
template auto ReportCells(
    ProcessLowerer&, const WalkFrame&, mir::LocalId,
    std::span<const hir::SensitivityEntry>) -> diag::Result<void>;
template auto ReportCells(
    const StructuralScopeLowerer&, const WalkFrame&, mir::LocalId,
    std::span<const hir::SensitivityEntry>) -> diag::Result<void>;
template auto BuildValueChangeWaitStmt(
    mir::Block&, const WalkFrame&, ProcessLowerer&,
    std::span<const hir::SensitivityEntry>) -> diag::Result<mir::Stmt>;
template auto BuildValueChangeWaitStmt(
    mir::Block&, const WalkFrame&, const StructuralScopeLowerer&,
    std::span<const hir::SensitivityEntry>) -> diag::Result<mir::Stmt>;

}  // namespace lyra::lowering::hir_to_mir
