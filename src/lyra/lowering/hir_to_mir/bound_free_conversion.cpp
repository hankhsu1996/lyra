#include "lyra/lowering/hir_to_mir/bound_free_conversion.hpp"

#include <optional>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/entry_walk.hpp"
#include "lyra/lowering/hir_to_mir/equivalence_identity.hpp"
#include "lyra/lowering/hir_to_mir/queue_building.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/statement/loops.hpp"
#include "lyra/lowering/hir_to_mir/value_operations.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/value_operation.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// A call of the library entry `entry` on what `receiver` names, answering
// `result`.
auto EntryCall(
    const mir::CompilationUnit& unit, mir::Block& block,
    support::BuiltinFn entry, std::optional<mir::ExprId> receiver,
    std::vector<mir::ExprId> arguments, mir::TypeId result) -> mir::ExprId {
  return block.exprs.Add(
      mir::MakeBuiltinCall(
          unit, block, entry, receiver, std::move(arguments), result));
}

// The body answering `answers` with a value of `to` built out of the value of
// `from` its one parameter refers to, the two of one kind and differing only
// in the bounds of the queues they hold (LRM 6.22.2 e). Each element is built
// afresh as an element of `to` is assigned one (LRM 7.6); a queue first keeps
// what its own bound admits, so what it discards is never built (LRM 7.10.5);
// an associative array keeps the value its absent indexes read (LRM 7.9.11).
auto ConversionBody(
    UnitLowerer& lowerer, support::BuiltinFn answers, mir::TypeId from,
    mir::TypeId to) -> mir::CallableCode {
  mir::CompilationUnit& unit = lowerer.Unit();
  mir::CallableCode code = mir::CallableCode::Defined();
  CallableBindings bindings(unit, code);
  const mir::TypeId reference = ReferenceTo(unit, from);
  const mir::LocalId param = code.AddLocal(reference);
  code.params = {param};
  if (support::ReachesAReceiver(answers)) {
    code.receiver = param;
  }
  code.result_type = to;

  // The body reads its one parameter and calls the library and the bodies of
  // its elements' types, so the frame it is built against carries a binding
  // context and a block and nothing else.
  WalkFrame frame;
  frame.bindings = &bindings;
  frame.current_block = &code.Body();
  mir::Block& block = code.Body();

  const mir::Type& to_type = unit.types.Get(to);
  const mir::TypeId element = *to_type.ContainerElementType();
  mir::ExprId source =
      block.Deref(unit.types, block.LocalPlace(param, reference), from);
  if (const auto* queue = to_type.As<mir::QueueType>();
      queue != nullptr && queue->max_bound.has_value()) {
    const mir::TypeId kept_type = unit.types.Intern(
        mir::Type{mir::QueueType{
            .element_type = *unit.types.Get(from).ContainerElementType(),
            .max_bound = queue->max_bound}});
    const mir::LocalId kept = DeclareLocal(
        frame,
        EntryCall(
            unit, block, support::BuiltinFn::kQueueFromArray, std::nullopt,
            {block.AsValue(unit.types, source)}, kept_type));
    source = block.LocalPlace(kept, kept_type);
  }

  const WalkedArray walked = WalkedArrayAt(
      lowerer, frame, AccessPath{.owner = source, .descent = {}}, std::nullopt);
  const mir::LocalId built = DeclareLocal(
      frame, to_type.Is<mir::DynamicArrayType>()
                 ? EntryCall(
                       unit, block, support::BuiltinFn::kMakeDynamicArrayNew,
                       std::nullopt,
                       {BuildToInt64Call(
                           unit, block, EntryCount(unit, block, walked))},
                       to)
                 : block.exprs.Add(BuildDefaultValueExpr(unit, block, to)));

  auto loop = BuildEntryLoopStmt(
      lowerer, frame, walked, WalkOrder::kFromFirst, std::nullopt,
      [&](const WalkFrame& at,
          const VisitedEntry& entry) -> diag::Result<void> {
        mir::Block& b = *at.current_block;
        const mir::ExprId value =
            ConvertToType(unit, b, entry.element, element);
        if (to_type.Is<mir::QueueType>()) {
          AppendPushBack(lowerer, b, built, to, value);
          return {};
        }
        AccessPath whole{.owner = b.LocalPlace(built, to), .descent = {}};
        DescentStep step = StepToVisited(unit, b, entry.place, element);
        const AccessPath into = DescendInto(std::move(whole), std::move(step));
        b.AppendStmt(
            mir::ExprStmt{
                .expr = b.exprs.Add(BuildStoreExpr(unit, b, into, value))});
        return {};
      });
  if (!loop) {
    throw InternalError(
        "BoundFreeConversionsOf: a walk over an array's entries refused to "
        "build -- please report this as a bug");
  }
  block.AppendStmt(*std::move(loop));

  if (to_type.Is<mir::AssociativeArrayType>()) {
    const mir::TypeId from_element =
        *unit.types.Get(from).ContainerElementType();
    mir::Block given;
    const mir::ExprId absent = EntryCall(
        unit, given, support::BuiltinFn::kAssocAbsentValue,
        PathPlace(unit, given, NamedIn(walked.path, given)), {}, from_element);
    const mir::ExprId held = given.ReadLocal(built, to);
    const mir::ExprId converted = ConvertToType(unit, given, absent, element);
    AssignLocal(
        unit, given, built,
        EntryCall(
            unit, given, support::BuiltinFn::kAssocWithDefault, held,
            {converted}, to));
    block.AppendIfThen(
        EntryCall(
            unit, block, support::BuiltinFn::kAssocHasStatedDefault,
            PathPlace(unit, block, NamedIn(walked.path, block)), {},
            unit.builtins.machine_bool),
        std::move(given));
  }

  block.AppendStmt(mir::ReturnStmt{.value = block.ReadLocal(built, to)});
  return code;
}

}  // namespace

auto BoundFreeConversionsOf(UnitLowerer& lowerer, mir::TypeId type)
    -> std::vector<mir::TypeOperation> {
  const mir::TypeId bound_free = WithoutQueueBounds(lowerer.Unit().types, type);
  std::vector<mir::TypeOperation> stated;
  stated.push_back(
      mir::TypeOperation{
          .type = type,
          .answers = support::BuiltinFn::kToBoundFree,
          .code = ConversionBody(
              lowerer, support::BuiltinFn::kToBoundFree, type, bound_free)});
  stated.push_back(
      mir::TypeOperation{
          .type = type,
          .answers = support::BuiltinFn::kFromBoundFree,
          .code = ConversionBody(
              lowerer, support::BuiltinFn::kFromBoundFree, bound_free, type)});
  return stated;
}

}  // namespace lyra::lowering::hir_to_mir
