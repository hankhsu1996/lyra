#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"

#include <cstdint>
#include <optional>
#include <span>
#include <utility>
#include <vector>

#include "lyra/base/overloaded.hpp"
#include "lyra/hir/timing.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/endpoint.hpp"
#include "lyra/lowering/hir_to_mir/expression/references.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/object_change.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
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

}  // namespace

template <typename Lowerer>
auto BuildObservableCellExpr(
    mir::Block& block, const WalkFrame& frame, mir::CompilationUnit& unit,
    Lowerer& lowerer, const hir::SensitivityEntry& entry) -> mir::ExprId {
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
      entry.cell);
}

auto BuildReportCall(
    const mir::CompilationUnit& unit, mir::Block& block, mir::LocalId report,
    support::BuiltinFn entry, std::vector<mir::ExprId> arguments,
    mir::TypeId type) -> mir::ExprId {
  const mir::ExprId object = block.exprs.Add(
      mir::Expr{
          .data =
              mir::DerefExpr{
                  .pointer = block.exprs.Add(
                      mir::MakeLocalRefExpr(
                          report, unit.builtins.read_report_ptr))},
          .type = unit.builtins.read_report});
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Direct{.target = entry, .receiver = object},
                  .arguments = std::move(arguments)},
          .type = type});
}

namespace {

// The same storage as a borrowed pointer, which is the form a registration
// hands the runtime. A route answers for this form itself, because an endpoint
// that reached out of the unit already holds a pointer and composing one from
// the cell would send that case through a dereference and back. Every other
// form is reached as the cell itself, so its pointer is that cell's address.
template <typename Lowerer>
auto BuildObservablePtrExpr(
    mir::Block& block, const WalkFrame& frame, mir::CompilationUnit& unit,
    Lowerer& lowerer, const hir::SensitivityEntry& entry) -> mir::ExprId {
  const auto address_of_cell = [&]() -> mir::ExprId {
    const mir::ExprId cell =
        BuildObservableCellExpr(block, frame, unit, lowerer, entry);
    const mir::TypeId ptr_type = unit.types.Intern(
        mir::Type{mir::PointerType{
            .pointee = block.exprs.Get(cell).type,
            .ownership = mir::PointerOwnership::kBorrowed,
            .mutability = mir::Mutability::kMutable}});
    return block.exprs.Add(mir::MakeAddressOfExpr(cell, ptr_type));
  };
  return std::visit(
      Overloaded{
          [&](const hir::RoutedValueRef& reference) -> mir::ExprId {
            return EndpointObservablePtr(
                block, frame, unit, BindRouted(lowerer, frame, reference));
          },
          [&](const hir::ExternalUnitValueRef&) -> mir::ExprId {
            return address_of_cell();
          },
          [&](const hir::StaticPropertyRef&) -> mir::ExprId {
            return address_of_cell();
          },
      },
      entry.cell);
}

// Which bits of a cell's packed encoding a leaf reads, as the `(lsb, width)` a
// registration takes (LRM 9.4.2 / 9.4.2.2 / 9.4.3): a bit-addressed footprint
// becomes `(lsb, hi - lsb + 1)`, and a read of the whole of it (no footprint)
// is width 0, which is also what a named event carries, having no bits at all.
struct LeafBits {
  std::int64_t lsb = 0;
  std::int64_t width = 0;
};

auto BitsOf(const hir::SensitivityEntry& entry) -> LeafBits {
  if (!entry.footprint.has_value()) {
    return {};
  }
  return LeafBits{
      .lsb = static_cast<std::int64_t>(entry.footprint->first),
      .width = static_cast<std::int64_t>(
          entry.footprint->second - entry.footprint->first + 1)};
}

// One leaf of the wait: the place it watches, what decides whether what happens
// there is an event, and which bits of that place's packed encoding it reads.
template <typename Lowerer>
auto BuildTriggerExpr(
    mir::Block& block, const WalkFrame& frame, mir::CompilationUnit& unit,
    Lowerer& lowerer, const hir::SensitivityEntry& entry,
    mir::LocalId observation) -> mir::ExprId {
  const mir::ExprId observable_ptr =
      BuildObservablePtrExpr(block, frame, unit, lowerer, entry);
  const LeafBits bits = BitsOf(entry);
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
                       BuildIntLiteral(unit, block, bits.lsb),
                       BuildIntLiteral(unit, block, bits.width)}},
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

// Records the place `place` points at, and which bits of it are read.
void ReportPlace(
    mir::CompilationUnit& unit, mir::Block& block, mir::LocalId report,
    mir::ExprId place, LeafBits bits) {
  ActOnReport(
      unit, block, report, support::BuiltinFn::kReadReportAdd,
      {place, BuildIntLiteral(unit, block, bits.lsb),
       BuildIntLiteral(unit, block, bits.width)});
}

// Binds `value` to a local of `frame`'s block, so a block nested in it can read
// what was evaluated here.
auto Bind(const WalkFrame& frame, mir::ExprId value) -> mir::LocalId {
  mir::Block& block = *frame.current_block;
  const mir::LocalId local =
      frame.bindings->DeclareAnonymous(block.exprs.Get(value).type);
  block.AppendStmt(mir::LocalDeclStmt{.target = local, .init = value});
  return local;
}

// Appends `if (holds(local)) { fill(inner) }` to `frame`'s block: what reaches
// through a handle is reached only where the handle names something, because a
// wait collects its leaves where nothing has tested the handle (LRM 8.4).
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
  ReportPlace(
      unit, block, report,
      ObjectEventSourceOf(unit, block, ObjectRootOf(unit, block, object())),
      LeafBits{});
  if (hops.empty()) return {};
  const hir::ObjectChain::Hop& hop = hops.front();
  const mir::TypeId next_type = lowerer.Owner().TranslateType(hop.handle_type);
  const mir::LocalId next = Bind(
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
          [&](const hir::SensitivityEntry& entry) -> diag::Result<void> {
            ReportPlace(
                unit, block, report,
                BuildObservablePtrExpr(block, frame, unit, lowerer, entry),
                BitsOf(entry));
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
                Bind(frame, block.exprs.Add(*std::move(handle)));
            return WhereItNamesSomething(
                unit, frame, bound, handle_type,
                [&](const WalkFrame& inner) -> diag::Result<void> {
                  auto place = HeldInterfaceMember(lowerer, inner, held);
                  if (!place) return std::unexpected(std::move(place.error()));
                  ReportPlace(
                      unit, *inner.current_block, report, *place, LeafBits{});
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
                      const mir::LocalId bound =
                          Bind(frame, block.exprs.Add(*std::move(root)));
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

}  // namespace

auto SealedCells(const hir::Reads& reads)
    -> std::optional<std::vector<hir::SensitivityEntry>> {
  if (!reads.calls.empty()) {
    return std::nullopt;
  }
  std::vector<hir::SensitivityEntry> cells;
  cells.reserve(reads.leaves.size());
  for (const hir::WaitLeaf& leaf : reads.leaves) {
    const auto* cell = std::get_if<hir::SensitivityEntry>(&leaf);
    if (cell == nullptr) {
      return std::nullopt;
    }
    cells.push_back(*cell);
  }
  return cells;
}

template <typename Lowerer>
auto ReportReads(
    Lowerer& lowerer, const WalkFrame& frame, const hir::Reads& reads,
    mir::LocalId report) -> diag::Result<void> {
  for (const hir::WaitLeaf& leaf : reads.leaves) {
    auto reported = ReportLeaf(lowerer, frame, report, leaf);
    if (!reported) return std::unexpected(std::move(reported.error()));
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
  const mir::ExprId observe_id = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Direct{.target = entry},
                  .arguments = std::move(arguments)},
          .type = unit.builtins.observation});
  const mir::LocalId local =
      frame.bindings->DeclareAnonymous(unit.builtins.observation);
  block.AppendStmt(mir::LocalDeclStmt{.target = local, .init = observe_id});
  return local;
}

template <typename Lowerer>
auto BuildWaitStmt(
    mir::Block& target_block, const WalkFrame& frame, Lowerer& lowerer,
    std::span<const ObservedLeaf> leaves, support::BuiltinFn entry)
    -> mir::Stmt {
  auto& unit = lowerer.Owner().Unit();
  std::vector<mir::ExprId> triggers;
  triggers.reserve(leaves.size());
  for (const ObservedLeaf& leaf : leaves) {
    triggers.push_back(BuildTriggerExpr(
        target_block, frame, unit, lowerer, leaf.entry, leaf.observation));
  }
  const mir::TypeId triggers_type =
      mir::MachineArrayOf(unit.types, unit.builtins.trigger, triggers.size());
  const mir::ExprId triggers_id = target_block.exprs.Add(
      mir::Expr{
          .data = mir::CompositeExpr{.parts = std::move(triggers)},
          .type = triggers_type});

  const mir::ExprId runtime_id =
      target_block.exprs.Add(BuildCurrentRuntimeCallExpr(lowerer.Owner()));
  const mir::ExprId call_id = target_block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Direct{.target = entry},
                  .arguments = {runtime_id, triggers_id}},
          .type = unit.builtins.machine_bool});

  return BuildWaitStmt(lowerer.Owner(), target_block, call_id);
}

template <typename Lowerer>
auto BuildCollectingWaitStmt(
    const WalkFrame& frame, Lowerer& lowerer,
    std::span<const CollectedExpression> expressions, support::BuiltinFn entry)
    -> diag::Result<mir::Stmt> {
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  mir::Block& block = *frame.current_block;
  std::vector<mir::LocalId> reports;
  reports.reserve(expressions.size());
  for (const CollectedExpression& expression : expressions) {
    const mir::LocalId report =
        frame.bindings->DeclareAnonymous(unit.builtins.read_report);
    block.AppendStmt(
        mir::LocalDeclStmt{
            .target = report,
            .init = block.exprs.Add(
                mir::Expr{
                    .data =
                        mir::CallExpr{
                            .callee =
                                mir::Direct{
                                    .target =
                                        support::BuiltinFn::kReadReportFor},
                            .arguments = {block.exprs.Add(
                                mir::MakeLocalRefExpr(
                                    expression.observation,
                                    unit.builtins.observation))}},
                    .type = unit.builtins.read_report})});
    const mir::LocalId pointer = Bind(
        frame,
        block.exprs.Add(
            mir::MakeAddressOfExpr(
                block.exprs.Add(
                    mir::MakeLocalRefExpr(report, unit.builtins.read_report)),
                unit.builtins.read_report_ptr)));
    auto reported = ReportReads(lowerer, frame, *expression.reads, pointer);
    if (!reported) return std::unexpected(std::move(reported.error()));
    reports.push_back(pointer);
  }

  std::vector<mir::ExprId> parts;
  parts.reserve(reports.size());
  for (const mir::LocalId pointer : reports) {
    parts.push_back(block.exprs.Add(
        mir::MakeLocalRefExpr(pointer, unit.builtins.read_report_ptr)));
  }
  const mir::TypeId reports_type = mir::MachineArrayOf(
      unit.types, unit.builtins.read_report_ptr, parts.size());
  const mir::ExprId reports_id = block.exprs.Add(
      mir::Expr{
          .data = mir::CompositeExpr{.parts = std::move(parts)},
          .type = reports_type});
  const mir::ExprId runtime_id =
      block.exprs.Add(BuildCurrentRuntimeCallExpr(lowerer.Owner()));
  const mir::ExprId call_id = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Direct{.target = entry},
                  .arguments = {runtime_id, reports_id}},
          .type = unit.builtins.machine_bool});
  return BuildWaitStmt(lowerer.Owner(), block, call_id);
}

template <typename Lowerer>
auto BuildValueChangeWaitStmt(
    mir::Block& target_block, const WalkFrame& frame, Lowerer& lowerer,
    const std::vector<hir::SensitivityEntry>& sensitivity_list,
    support::BuiltinFn entry) -> mir::Stmt {
  const mir::LocalId observation = DeclareObservation(
      lowerer.Owner().Unit(), frame, target_block,
      support::BuiltinFn::kObservationOnReaching, {});
  std::vector<ObservedLeaf> leaves;
  leaves.reserve(sensitivity_list.size());
  for (const hir::SensitivityEntry& read : sensitivity_list) {
    leaves.push_back(ObservedLeaf{.entry = read, .observation = observation});
  }
  return BuildWaitStmt(target_block, frame, lowerer, leaves, entry);
}

// One instantiation per lowering a wait is built in.
template auto BuildObservableCellExpr(
    mir::Block&, const WalkFrame&, mir::CompilationUnit&, ProcessLowerer&,
    const hir::SensitivityEntry&) -> mir::ExprId;
template auto BuildObservableCellExpr(
    mir::Block&, const WalkFrame&, mir::CompilationUnit&,
    const StructuralScopeLowerer&, const hir::SensitivityEntry&) -> mir::ExprId;
template auto ReportReads(
    ProcessLowerer&, const WalkFrame&, const hir::Reads&, mir::LocalId)
    -> diag::Result<void>;
template auto ReportReads(
    const StructuralScopeLowerer&, const WalkFrame&, const hir::Reads&,
    mir::LocalId) -> diag::Result<void>;
template auto BuildWaitStmt(
    mir::Block&, const WalkFrame&, ProcessLowerer&,
    std::span<const ObservedLeaf>, support::BuiltinFn) -> mir::Stmt;
template auto BuildWaitStmt(
    mir::Block&, const WalkFrame&, const StructuralScopeLowerer&,
    std::span<const ObservedLeaf>, support::BuiltinFn) -> mir::Stmt;
template auto BuildCollectingWaitStmt(
    const WalkFrame&, ProcessLowerer&, std::span<const CollectedExpression>,
    support::BuiltinFn) -> diag::Result<mir::Stmt>;
template auto BuildCollectingWaitStmt(
    const WalkFrame&, const StructuralScopeLowerer&,
    std::span<const CollectedExpression>, support::BuiltinFn)
    -> diag::Result<mir::Stmt>;
template auto BuildValueChangeWaitStmt(
    mir::Block&, const WalkFrame&, ProcessLowerer&,
    const std::vector<hir::SensitivityEntry>&, support::BuiltinFn) -> mir::Stmt;
template auto BuildValueChangeWaitStmt(
    mir::Block&, const WalkFrame&, const StructuralScopeLowerer&,
    const std::vector<hir::SensitivityEntry>&, support::BuiltinFn) -> mir::Stmt;

}  // namespace lyra::lowering::hir_to_mir
