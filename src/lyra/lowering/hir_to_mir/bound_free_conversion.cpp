#include "lyra/lowering/hir_to_mir/bound_free_conversion.hpp"

#include <optional>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/callee_interface.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/container_bodies.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/entry_walk.hpp"
#include "lyra/lowering/hir_to_mir/equivalence_identity.hpp"
#include "lyra/lowering/hir_to_mir/queue_building.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/statement/loops.hpp"
#include "lyra/lowering/hir_to_mir/value_operations.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/storage_part.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/library_decl.hpp"
#include "lyra/support/value_operation.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The body answering `answers` with a value of `to` built out of the value of
// `from` its one parameter refers to, the two of one kind and differing only
// in the bounds of the queues they hold (LRM 6.22.2 e). Each element is built
// afresh as an element of `to` is assigned one (LRM 7.6); a queue first keeps
// what its own bound admits, so what it discards is never built (LRM 7.10.5);
// an associative array keeps the value its absent indexes read (LRM 7.9.11).
auto ConversionBody(
    const mir::CompilationUnit& unit, support::BoundFreeItem answers,
    mir::TypeId from, mir::TypeId to) -> mir::CallableCode {
  mir::CallableCode code = mir::CallableCode::Defined(diag::SourceSpan{});
  CallableBindings bindings(unit, code);
  const mir::TypeId reference = ReferenceTo(unit, from);
  const mir::LocalId param = code.AddLocal(reference);
  code.params = {param};
  if (support::ReachesAReceiver(answers)) {
    code.receiver = param;
  }
  code.result_type = to;
  DeclareCallerLocation(unit, true, bindings, code, code.params);

  // The body reads its one parameter and calls the library and the bodies of
  // its elements' types, so the frame it is built against carries a binding
  // context and a block and nothing else.
  WalkFrame frame;
  frame.bindings = &bindings;
  frame.current_block = &code.Body();
  mir::Builder block = frame.Builder();

  const mir::Type& to_type = unit.types.Get(to);
  const mir::TypeId element = *to_type.ContainerElementType();
  mir::ExprId source =
      block.Deref(unit.types, block.LocalPlace(param, reference), from);
  if (to_type.IsDeclaredBy(support::LibraryDecl::kQueue) &&
      std::holds_alternative<support::BoundedQueue>(
          to_type.Get<mir::LibraryType>().Bound())) {
    const mir::TypeId kept_type = mir::QueueOf(
        unit.types, *unit.types.Get(from).ContainerElementType(),
        to_type.Get<mir::LibraryType>().Bound());
    source = MaterializedTemporary(
        frame,
        block.Add(
            mir::Expr{
                .data =
                    mir::CallExpr{
                        .callee =
                            mir::Direct{
                                .target =
                                    mir::TypeOperationTarget{
                                        .type = kept_type,
                                        .answers =
                                            support::BuiltinFn::kQueueFromArray,
                                        .arguments = {from}}},
                        .arguments = {mir::Borrow(unit, block, source)}},
                .type = kept_type}));
  }

  const WalkedArray walked = WalkedArrayAt(
      unit, frame, AccessPath{.owner = source, .descent = {}}, std::nullopt);
  const mir::LocalId built = DeclareLocal(
      frame,
      to_type.IsDeclaredBy(support::LibraryDecl::kDynamicArray)
          ? block.Add(BuildDynamicArrayNew(
                unit, block, to,
                BuildToInt64Call(unit, block, EntryCount(unit, block, walked))))
          : block.Add(BuildDefaultValueExpr(unit, block, to)));

  auto loop = BuildEntryLoopStmt(
      unit, frame, walked, WalkOrder::kFromFirst, std::nullopt,
      [&](const WalkFrame& at,
          const VisitedEntry& entry) -> diag::Result<void> {
        mir::Builder b = at.Builder();
        const mir::ExprId value =
            ConvertToType(unit, b, entry.element, element);
        if (to_type.IsDeclaredBy(support::LibraryDecl::kQueue)) {
          AppendPushBack(unit, b, built, to, value);
          return {};
        }
        AccessPath whole{.owner = b.LocalPlace(built, to), .descent = {}};
        DescentStep step = StepToVisited(unit, b, entry.place, element);
        const AccessPath into = DescendInto(std::move(whole), std::move(step));
        b.AppendStmt(
            mir::ExprStmt{.expr = b.Add(BuildStoreExpr(unit, b, into, value))});
        return {};
      });
  if (!loop) {
    throw InternalError(
        "hir_to_mir: a walk over an array's entries refused to "
        "build -- please report this as a bug");
  }
  block.AppendStmt(*std::move(loop));

  if (to_type.IndexType().has_value()) {
    const mir::TypeId from_element =
        *unit.types.Get(from).ContainerElementType();
    const mir::TypeId stated_type = mir::BorrowedPointerTo(
        unit.types, from_element, mir::Mutability::kReadOnly);
    const mir::LocalId stated = DeclareLocal(
        frame,
        block.Add(ContainerOperationCall(
                      unit, block, support::BuiltinFn::kAssocStatedDefault,
                      PathPlace(unit, block, NamedIn(unit, walked.path, block)),
                      {}, stated_type)
                      .value()));
    mir::Block given;
    mir::Builder given_builder = block.Into(given);
    given_builder.AppendStmt(
        mir::ExprStmt{
            .expr = given_builder.Add(
                ContainerOperationCall(
                    unit, given_builder,
                    support::BuiltinFn::kAssocSetStatedDefault,
                    given_builder.LocalPlace(built, to),
                    {ConvertToType(
                        unit, given_builder,
                        given_builder.AsValue(
                            unit.types,
                            given_builder.Deref(
                                unit.types,
                                given_builder.ReadLocal(stated, stated_type),
                                from_element)),
                        element)},
                    unit.builtins.void_type)
                    .value())});
    block.AppendIfThen(
        BuildConditionNot(
            unit, block,
            BuildIsNull(unit, block, block.ReadLocal(stated, stated_type))),
        std::move(given));
  }

  block.AppendStmt(mir::ReturnStmt{.value = block.ReadLocal(built, to)});
  return code;
}

}  // namespace

auto BuildToBoundFreeBody(const mir::CompilationUnit& unit, mir::TypeId type)
    -> mir::CallableCode {
  return ConversionBody(
      unit, support::BoundFreeItem::kToBoundFree, type,
      WithoutQueueBounds(unit.types, type));
}

auto BuildFromBoundFreeBody(const mir::CompilationUnit& unit, mir::TypeId type)
    -> mir::CallableCode {
  return ConversionBody(
      unit, support::BoundFreeItem::kFromBoundFree,
      WithoutQueueBounds(unit.types, type), type);
}

}  // namespace lyra::lowering::hir_to_mir
