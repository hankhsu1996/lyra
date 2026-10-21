#include "lyra/lowering/hir_to_mir/expression/array_iterator_method.hpp"

#include <array>
#include <cstdint>
#include <expected>
#include <functional>
#include <optional>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/with_clause_id.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/block_builder.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/closure_builder.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/entry_walk.hpp"
#include "lyra/lowering/hir_to_mir/expression/calls.hpp"
#include "lyra/lowering/hir_to_mir/expression/operators.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/queue_building.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/statement/loops.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/value_operations.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/callable_bounds.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The array the method acts on, which the source writes before the method's
// name and the call carries as its leading argument.
auto ArrayOperand(const hir::CallExpr& c) -> hir::ExprId {
  if (c.arguments.empty() || !c.arguments.front().has_value()) {
    throw InternalError(
        "LowerArrayIteratorMethod: an array method reached lowering without "
        "the array it acts on");
  }
  return *c.arguments.front();
}

// The identity the variable holding an entry's index carries, which is the
// clause's `item.index` where the call writes a clause (LRM 7.12.4) and none
// where nothing can name it.
auto IndexOrigin(const hir::ArrayIteratorMethodRef& ref)
    -> std::optional<BindingOriginId> {
  return ref.with_clause.transform(
      [](const hir::WithClause& clause) -> BindingOriginId {
        return IteratorOrigin{
            .clause = clause.id, .role = hir::IterationBindingRole::kIndex};
      });
}

// The type of one element of `array`.
auto ElementTypeOf(const mir::CompilationUnit& unit, const WalkedArray& array)
    -> mir::TypeId {
  const std::optional<mir::TypeId> element =
      unit.types.Get(array.type).ContainerElementType();
  if (!element.has_value()) {
    throw InternalError(
        "LowerArrayIteratorMethod: an array method acts on no unpacked array");
  }
  return *element;
}

// The type of the key the method evaluates at each entry: what the clause's
// expression is, or the element's where the call writes none.
template <ExprLowerer Lowerer>
auto KeyType(
    Lowerer& lowerer, const hir::ArrayIteratorMethodRef& ref,
    const WalkedArray& array) -> mir::TypeId {
  if (ref.with_clause.has_value()) {
    return lowerer.Owner().TranslateType(
        lowerer.HirExprs().Get(ref.with_clause->expr).type);
  }
  return ElementTypeOf(lowerer.Owner().Unit(), array);
}

// One entry as the method sees it: the entry's element held in a variable,
// which is the clause's iterator where the call writes a clause, and the key --
// what the clause answers for the entry, or the element itself where the call
// writes none (LRM 7.12.1, 7.12.4).
struct EntryKey {
  mir::LocalId item;
  mir::TypeId item_type;
  mir::ExprId key;
};

template <ExprLowerer Lowerer>
auto EvaluateKey(
    Lowerer& lowerer, const hir::ArrayIteratorMethodRef& ref,
    const WalkFrame& at, const VisitedEntry& entry) -> diag::Result<EntryKey> {
  mir::Block& block = *at.current_block;
  const mir::TypeId item_type = block.exprs.Get(entry.element).type;
  if (!ref.with_clause.has_value()) {
    const mir::LocalId item = at.bindings->DeclareAnonymous(item_type);
    block.AppendStmt(mir::LocalDeclStmt{.target = item, .init = entry.element});
    return EntryKey{
        .item = item,
        .item_type = item_type,
        .key = block.ReadLocal(item, item_type)};
  }
  const hir::WithClause& clause = *ref.with_clause;
  const mir::LocalId item = at.bindings->DeclareNamed(
      IteratorOrigin{
          .clause = clause.id, .role = hir::IterationBindingRole::kElement},
      clause.element_name, item_type);
  block.AppendStmt(mir::LocalDeclStmt{.target = item, .init = entry.element});
  auto key = lowerer.LowerExpr(lowerer.HirExprs().Get(clause.expr), at);
  if (!key) return std::unexpected(std::move(key.error()));
  return EntryKey{
      .item = item, .item_type = item_type, .key = block.exprs.Add(*key)};
}

// What a method does with one entry's key, appended to the block the frame
// names.
using KeyUse = std::function<diag::Result<void>(
    const WalkFrame& at, const VisitedEntry& entry, const EntryKey& key)>;

// Appends to the steps the loop that evaluates the call's key at every entry
// of `array`, from the end `order` names, and hands each to `use`.
template <ExprLowerer Lowerer>
auto AppendKeyLoop(
    Lowerer& lowerer, const hir::ArrayIteratorMethodRef& ref,
    BlockBuilder& steps, const WalkedArray& array, WalkOrder order,
    const KeyUse& use) -> diag::Result<void> {
  auto walked = BuildEntryLoopStmt(
      lowerer.Owner(), steps.Frame(), array, order, IndexOrigin(ref),
      [&](const WalkFrame& at,
          const VisitedEntry& entry) -> diag::Result<void> {
        auto key = EvaluateKey(lowerer, ref, at, entry);
        if (!key) return std::unexpected(std::move(key.error()));
        return use(at, entry, *key);
      });
  if (!walked) return std::unexpected(std::move(walked.error()));
  steps.Body().AppendStmt(*std::move(walked));
  return {};
}

// A local of `type` starting out as that type's default.
auto DeclareDefault(
    const mir::CompilationUnit& unit, const WalkFrame& frame, mir::TypeId type)
    -> mir::LocalId {
  mir::Block& block = *frame.current_block;
  return DeclareLocal(
      frame, block.exprs.Add(BuildDefaultValueExpr(unit, block, type)));
}

// A machine boolean flag starting out false.
auto DeclareFlag(const mir::CompilationUnit& unit, const WalkFrame& frame)
    -> mir::LocalId {
  return DeclareLocal(
      frame, BuildMachineBool(unit, *frame.current_block, false));
}

// What a method collects of an entry it keeps: the element, or its index.
enum class CollectedPart : std::uint8_t { kElement, kIndex };

auto CollectedOf(
    mir::Block& block, const EntryKey& key, const VisitedEntry& entry,
    CollectedPart collected) -> mir::ExprId {
  switch (collected) {
    case CollectedPart::kElement:
      return block.ReadLocal(key.item, key.item_type);
    case CollectedPart::kIndex:
      return block.ReadLocal(entry.place.index, entry.place.index_type);
  }
  throw InternalError("LowerArrayIteratorMethod: unknown collected part");
}

// The array the call acts on, lowered where it lies in the frame's block and
// settled for the loop that walks it.
template <ExprLowerer Lowerer>
auto WalkedOperand(
    Lowerer& lowerer, const hir::CallExpr& c, const WalkFrame& frame)
    -> diag::Result<WalkedArray> {
  mir::Block& block = *frame.current_block;
  const hir::Expr& operand = lowerer.HirExprs().Get(ArrayOperand(c));
  auto object = lowerer.LowerAsPlace(operand, frame);
  if (!object) return std::unexpected(std::move(object.error()));
  return WalkedArrayOf(
      lowerer.Owner(), frame, block.exprs.Add(*object),
      DeclaredRangeOf(lowerer.Owner(), operand.type));
}

// LRM 7.12.3: the keys folded by `op`, the first seeding the fold, at the type
// the call answers in. The clause states no answer for an array with no
// entries, which answers that type's default.
template <ExprLowerer Lowerer>
auto LowerReduction(
    Lowerer& lowerer, const WalkFrame& frame, const hir::CallExpr& c,
    const hir::ArrayIteratorMethodRef& ref, hir::BinaryOp op,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  const mir::CompilationUnit& unit = lowerer.Owner().Unit();
  const mir::TypeId boolean = unit.builtins.machine_bool;
  BlockBuilder steps(frame);
  auto array = WalkedOperand(lowerer, c, steps.Frame());
  if (!array) return std::unexpected(std::move(array.error()));
  const mir::LocalId folded = DeclareDefault(unit, steps.Frame(), result_type);
  const mir::LocalId seeded = DeclareFlag(unit, steps.Frame());
  auto walked = AppendKeyLoop(
      lowerer, ref, steps, *array, WalkOrder::kFromFirst,
      [&](const WalkFrame& at, const VisitedEntry&,
          const EntryKey& key) -> diag::Result<void> {
        mir::Block& b = *at.current_block;
        const mir::LocalId held =
            DeclareLocal(at, ConvertToType(unit, b, key.key, result_type));
        const mir::ExprId combined = b.exprs.Add(BuildMirBinaryExpr(
            unit, b, op, b.ReadLocal(folded, result_type),
            b.ReadLocal(held, result_type), result_type));
        AssignLocal(
            unit, b, folded,
            b.exprs.Add(
                mir::Expr{
                    .data =
                        mir::ConditionalExpr{
                            .condition = b.ReadLocal(seeded, boolean),
                            .then_value = combined,
                            .else_value = b.ReadLocal(held, result_type)},
                    .type = result_type}));
        AssignLocal(unit, b, seeded, BuildMachineBool(unit, b, true));
        return {};
      });
  if (!walked) return std::unexpected(std::move(walked.error()));
  return steps.Build(steps.Body().ReadLocal(folded, result_type));
}

// Which entries a locator collects (LRM 7.12.1): every one whose key holds,
// the first, or the last.
enum class LocatedEntries : std::uint8_t { kEvery, kFirst, kLast };

// Where a locator's walk starts, and whether it ends at the first entry it
// collects: the last entry whose key holds is the first one a walk from the
// last entry meets.
struct LocatorWalk {
  WalkOrder order;
  bool ends_at_first_found;
};

auto LocatorWalkOf(LocatedEntries located) -> LocatorWalk {
  switch (located) {
    case LocatedEntries::kEvery:
      return {.order = WalkOrder::kFromFirst, .ends_at_first_found = false};
    case LocatedEntries::kFirst:
      return {.order = WalkOrder::kFromFirst, .ends_at_first_found = true};
    case LocatedEntries::kLast:
      return {.order = WalkOrder::kFromLast, .ends_at_first_found = true};
  }
  throw InternalError("LowerArrayIteratorMethod: unknown locator");
}

// LRM 7.12.1 `find` and its forms: a queue of what is collected of each entry
// whose key holds as a condition, in order. An unknown key selects nothing.
template <ExprLowerer Lowerer>
auto LowerLocator(
    Lowerer& lowerer, const WalkFrame& frame, const hir::CallExpr& c,
    const hir::ArrayIteratorMethodRef& ref, LocatedEntries located,
    CollectedPart collected, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  const mir::CompilationUnit& unit = unit_lowerer.Unit();
  const LocatorWalk walk = LocatorWalkOf(located);
  BlockBuilder steps(frame);
  auto array = WalkedOperand(lowerer, c, steps.Frame());
  if (!array) return std::unexpected(std::move(array.error()));
  const mir::LocalId found = DeclareDefault(unit, steps.Frame(), result_type);
  auto walked = AppendKeyLoop(
      lowerer, ref, steps, *array, walk.order,
      [&](const WalkFrame& at, const VisitedEntry& entry,
          const EntryKey& key) -> diag::Result<void> {
        mir::Block& b = *at.current_block;
        const mir::ExprId holds = ReduceToCondition(unit, b, key.key);
        mir::Block taken;
        AppendPushBack(
            unit_lowerer, taken, found, result_type,
            CollectedOf(taken, key, entry, collected));
        if (walk.ends_at_first_found) {
          taken.AppendStmt(mir::BreakStmt{.target = at.break_leaves->Name()});
        }
        b.AppendIfThen(holds, std::move(taken));
        return {};
      });
  if (!walked) return std::unexpected(std::move(walked.error()));
  return steps.Build(steps.Body().ReadLocal(found, result_type));
}

// LRM 7.12.1 `min` / `max`: a queue of the element whose key comes first by
// `comparison` -- `<` for `min`, `>` for `max` -- the first of several, and
// empty for an array with no entries. A key carrying x or z compares unknown,
// which reads as not first. The best entry so far is kept, and the answer made
// once the walk is done.
template <ExprLowerer Lowerer>
auto LowerMinMax(
    Lowerer& lowerer, const WalkFrame& frame, const hir::CallExpr& c,
    const hir::ArrayIteratorMethodRef& ref, hir::BinaryOp comparison,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  const mir::CompilationUnit& unit = unit_lowerer.Unit();
  const mir::TypeId boolean = unit.builtins.machine_bool;
  BlockBuilder steps(frame);
  mir::Block& body = steps.Body();
  auto array = WalkedOperand(lowerer, c, steps.Frame());
  if (!array) return std::unexpected(std::move(array.error()));
  const mir::TypeId key_type = KeyType(lowerer, ref, *array);
  const mir::TypeId item_type = ElementTypeOf(unit, *array);
  const mir::LocalId found = DeclareDefault(unit, steps.Frame(), result_type);
  const mir::LocalId seen = DeclareFlag(unit, steps.Frame());
  const mir::LocalId best = DeclareDefault(unit, steps.Frame(), key_type);
  const mir::LocalId best_item = DeclareDefault(unit, steps.Frame(), item_type);
  auto walked = AppendKeyLoop(
      lowerer, ref, steps, *array, WalkOrder::kFromFirst,
      [&](const WalkFrame& at, const VisitedEntry&,
          const EntryKey& key) -> diag::Result<void> {
        mir::Block& b = *at.current_block;
        const mir::LocalId held =
            DeclareLocal(at, ConvertToType(unit, b, key.key, key_type));
        const std::array compared{key_type, key_type};
        const mir::ExprId comes_first = ReduceToCondition(
            unit, b,
            b.exprs.Add(BuildMirBinaryExpr(
                unit, b, comparison, b.ReadLocal(held, key_type),
                b.ReadLocal(best, key_type),
                OneBitAnswerType(unit, compared))));
        const std::array better{
            BuildConditionNot(unit, b, b.ReadLocal(seen, boolean)),
            comes_first};
        mir::Block taken;
        AssignLocal(unit, taken, best, taken.ReadLocal(held, key_type));
        AssignLocal(
            unit, taken, best_item,
            ConvertToType(
                unit, taken, taken.ReadLocal(key.item, key.item_type),
                item_type));
        AssignLocal(unit, taken, seen, BuildMachineBool(unit, taken, true));
        b.AppendIfThen(AnyHolds(unit, b, better), std::move(taken));
        return {};
      });
  if (!walked) return std::unexpected(std::move(walked.error()));
  mir::Block answered;
  AppendPushBack(
      unit_lowerer, answered, found, result_type,
      answered.ReadLocal(best_item, item_type));
  body.AppendIfThen(body.ReadLocal(seen, boolean), std::move(answered));
  return steps.Build(body.ReadLocal(found, result_type));
}

// LRM 7.12.1 `unique` and its index form: what is collected of the first entry
// of each distinct key, in the order first seen. Keys are one value where
// their bits are, unknown bits included (LRM 11.4.5 `===`).
template <ExprLowerer Lowerer>
auto LowerUnique(
    Lowerer& lowerer, const WalkFrame& frame, const hir::CallExpr& c,
    const hir::ArrayIteratorMethodRef& ref, CollectedPart collected,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  const mir::TypeId boolean = unit.builtins.machine_bool;
  BlockBuilder steps(frame);
  auto array = WalkedOperand(lowerer, c, steps.Frame());
  if (!array) return std::unexpected(std::move(array.error()));
  const mir::LocalId found = DeclareDefault(unit, steps.Frame(), result_type);
  const mir::TypeId key_type = KeyType(lowerer, ref, *array);
  const mir::TypeId keys_type = QueueOf(unit, key_type);
  const mir::LocalId seen_keys = DeclareDefault(unit, steps.Frame(), keys_type);
  auto walked = AppendKeyLoop(
      lowerer, ref, steps, *array, WalkOrder::kFromFirst,
      [&](const WalkFrame& at, const VisitedEntry& entry,
          const EntryKey& key) -> diag::Result<void> {
        mir::Block& b = *at.current_block;
        const mir::LocalId held =
            DeclareLocal(at, ConvertToType(unit, b, key.key, key_type));
        const mir::LocalId repeated = DeclareFlag(unit, at);
        const WalkedArray seen = WalkedArrayOf(
            unit_lowerer, at, b.LocalPlace(seen_keys, keys_type), std::nullopt);
        auto searched = BuildEntryLoopStmt(
            unit_lowerer, at, seen, WalkOrder::kFromFirst, std::nullopt,
            [&](const WalkFrame& in,
                const VisitedEntry& earlier) -> diag::Result<void> {
              mir::Block& ib = *in.current_block;
              mir::Block same;
              AssignLocal(
                  unit, same, repeated, BuildMachineBool(unit, same, true));
              same.AppendStmt(
                  mir::BreakStmt{.target = in.break_leaves->Name()});
              ib.AppendIfThen(
                  BuildBitIdentity(
                      unit, ib, ib.ReadLocal(held, key_type), earlier.element),
                  std::move(same));
              return {};
            });
        if (!searched) return std::unexpected(std::move(searched.error()));
        b.AppendStmt(*std::move(searched));
        mir::Block first;
        AppendPushBack(
            unit_lowerer, first, seen_keys, keys_type,
            first.ReadLocal(held, key_type));
        AppendPushBack(
            unit_lowerer, first, found, result_type,
            CollectedOf(first, key, entry, collected));
        b.AppendIfThen(
            BuildConditionNot(unit, b, b.ReadLocal(repeated, boolean)),
            std::move(first));
        return {};
      });
  if (!walked) return std::unexpected(std::move(walked.error()));
  return steps.Build(steps.Body().ReadLocal(found, result_type));
}

// LRM 7.12.5 `map`: an array of the receiver's own kind and range holding each
// entry's key in the entry's place -- a keyed array under each entry's index,
// every other at the entry's own position.
template <ExprLowerer Lowerer>
auto LowerMap(
    Lowerer& lowerer, const WalkFrame& frame, const hir::CallExpr& c,
    const hir::ArrayIteratorMethodRef& ref, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  BlockBuilder steps(frame);
  mir::Block& body = steps.Body();
  auto array = WalkedOperand(lowerer, c, steps.Frame());
  if (!array) return std::unexpected(std::move(array.error()));
  const mir::Type& answer = unit.types.Get(result_type);
  const std::optional<mir::TypeId> mapped_element =
      answer.ContainerElementType();
  if (!mapped_element.has_value()) {
    throw InternalError(
        "LowerArrayIteratorMethod: a map answers with something other than an "
        "unpacked array");
  }
  const bool sized_while_running = answer.Is<mir::DynamicArrayType>();
  const bool grows = answer.Is<mir::QueueType>();
  // A dynamic array is made as many elements long as the receiver, which the
  // loop then fills by position; every other kind starts empty, or at its
  // fixed size.
  const mir::LocalId mapped =
      sized_while_running
          ? DeclareLocal(
                steps.Frame(),
                body.exprs.Add(
                    mir::Expr{
                        .data =
                            mir::CallExpr{
                                .callee =
                                    mir::Direct{
                                        .target = support::BuiltinFn::
                                            kMakeDynamicArrayNew},
                                .arguments = {BuildToInt64Call(
                                    unit, body,
                                    EntryCount(unit, body, *array))}},
                        .type = result_type}))
          : DeclareDefault(unit, steps.Frame(), result_type);
  auto walked = AppendKeyLoop(
      lowerer, ref, steps, *array, WalkOrder::kFromFirst,
      [&](const WalkFrame& at, const VisitedEntry& entry,
          const EntryKey& key) -> diag::Result<void> {
        mir::Block& b = *at.current_block;
        const mir::ExprId value =
            ConvertToType(unit, b, key.key, *mapped_element);
        if (grows) {
          AppendPushBack(unit_lowerer, b, mapped, result_type, value);
          return {};
        }
        const AccessPath into = DescendInto(
            AccessPath{
                .owner = b.LocalPlace(mapped, result_type), .descent = {}},
            StepToVisited(unit, b, entry.place, *mapped_element));
        b.AppendStmt(
            mir::ExprStmt{
                .expr = b.exprs.Add(BuildStoreExpr(unit, b, into, value))});
        return {};
      });
  if (!walked) return std::unexpected(std::move(walked.error()));
  return steps.Build(body.ReadLocal(mapped, result_type));
}

// The order a sort puts the entries in, as a closure answering whether the
// entry at one position comes before the one at another: whether the first's
// entry of `sequence` compares by `comparison` with the second's, an entry
// carrying x or z comparing as not before (LRM 7.12.2). It is the `is_less`
// Rust's sort is handed. The library that applies it borrows it, so it never
// outlives the call, and it refers to `sequence` where it lies rather than
// copying it -- the keys the sort gathered, or the array itself where the call
// writes no clause and the elements are compared.
auto BuildIsLess(
    UnitLowerer& unit_lowerer, const WalkFrame& frame,
    const AccessPath& sequence, mir::TypeId entry_type,
    hir::BinaryOp comparison) -> mir::ExprId {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& block = *frame.current_block;
  ClosureBuilder is_less(unit, frame, support::ClosureKind::kFn);
  mir::Block& body = is_less.Body();
  const BodyBindingRef held = SnapshotBindingIntoClosure(
      unit_lowerer, frame, is_less, PathReference(unit, block, sequence));
  const mir::TypeId position = unit.builtins.machine_int64;
  const auto entry_at = [&](mir::LocalId at) {
    const mir::ExprId reached = body.exprs.Add(
        mir::MakeBuiltinCall(
            unit, body, support::BuiltinFn::kIntegralFromInt, std::nullopt,
            {body.ReadLocal(at, position)}, mir::PositionType(unit.types)));
    return PathValue(
        unit, body,
        DescendInto(
            AccessPath{
                .owner = body.exprs.Add(
                    is_less.Bindings().MakeBindingPlace(held, body)),
                .descent = {}},
            DescentStep{
                .to = StepToElement{.position = reached},
                .part_type = entry_type}));
  };
  const mir::LocalId first = is_less.AddParam(position);
  const mir::LocalId second = is_less.AddParam(position);
  const std::array compared{entry_type, entry_type};
  const mir::ExprId before = ReduceToCondition(
      unit, body,
      body.exprs.Add(BuildMirBinaryExpr(
          unit, body, comparison, entry_at(first), entry_at(second),
          OneBitAnswerType(unit, compared))));
  const mir::ExprId made = block.exprs.Add(is_less.Build(before));
  const mir::TypeId closure_type = block.exprs.Get(made).type;
  return BuildBorrowedClosure(
      unit, block, block.LocalPlace(DeclareLocal(frame, made), closure_type));
}

// LRM 7.12.2 `sort` / `rsort`: the library told to put the entries in the
// order of their keys, which it is handed as a comparison of two entries by
// `comparison` -- `<` for `sort` and `>` for `rsort`, so one library entry
// sorts both ways, as Rust sorts descending by `sort_by` --
// of the keys the loop gathered in order where the call writes a clause, and
// of the elements themselves where it writes none, which the library reads
// only while it decides the order and before it moves anything. The array is
// changed where the source named it; one lying where another member of an
// untagged union may have written is sorted in a copy read as its type holds
// a value, which is then written back (LRM 7.3.2).
template <ExprLowerer Lowerer>
auto LowerSort(
    Lowerer& lowerer, const WalkFrame& frame, const hir::CallExpr& c,
    const hir::ArrayIteratorMethodRef& ref, hir::BinaryOp comparison)
    -> diag::Result<mir::Expr> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  BlockBuilder steps(frame);
  mir::Block& body = steps.Body();
  const hir::Expr& operand = lowerer.HirExprs().Get(ArrayOperand(c));
  auto target = lowerer.LowerLhsExpr(operand, steps.Frame());
  if (!target) return std::unexpected(std::move(target.error()));
  const std::optional<hir::UnpackedRange> declared =
      DeclaredRangeOf(unit_lowerer, operand.type);
  std::optional<AccessPath> written_back;
  WalkedArray array = [&] {
    if (!ReachesOverlaidStorage(*target)) {
      return WalkedArrayAt(
          unit_lowerer, steps.Frame(), *std::move(target), declared);
    }
    ReadThenWritten changed =
        ReadThenWrite(unit_lowerer, steps.Frame(), *std::move(target));
    const mir::TypeId type = body.exprs.Get(changed.incoming).type;
    written_back = std::move(changed.place);
    return WalkedArrayAt(
        unit_lowerer, steps.Frame(),
        AccessPath{
            .owner = body.LocalPlace(
                DeclareLocal(steps.Frame(), changed.incoming), type),
            .descent = {}},
        declared);
  }();

  const mir::TypeId key_type = KeyType(lowerer, ref, array);
  auto compared = [&]() -> diag::Result<AccessPath> {
    if (!ref.with_clause.has_value()) {
      return NamedIn(array.path, body);
    }
    const mir::TypeId keys_type = QueueOf(unit, key_type);
    const mir::LocalId keys = DeclareDefault(unit, steps.Frame(), keys_type);
    auto walked = AppendKeyLoop(
        lowerer, ref, steps, array, WalkOrder::kFromFirst,
        [&](const WalkFrame& at, const VisitedEntry&,
            const EntryKey& key) -> diag::Result<void> {
          AppendPushBack(
              unit_lowerer, *at.current_block, keys, keys_type, key.key);
          return {};
        });
    if (!walked) return std::unexpected(std::move(walked.error()));
    return AccessPath{.owner = body.LocalPlace(keys, keys_type), .descent = {}};
  }();
  if (!compared) return std::unexpected(std::move(compared.error()));

  const mir::ExprId order =
      BuildIsLess(unit_lowerer, steps.Frame(), *compared, key_type, comparison);
  const mir::ExprId sorted = body.exprs.Add(BuildBuiltinMethodCall(
      unit_lowerer, body, support::BuiltinFn::kSort,
      PathPlace(unit, body, NamedIn(array.path, body)), {order},
      unit.builtins.void_type));
  if (!written_back.has_value()) {
    return steps.Build(sorted);
  }
  body.AppendStmt(mir::ExprStmt{.expr = sorted});
  return steps.Build(body.exprs.Add(BuildStoreExpr(
      unit, body, *written_back,
      PathValue(unit, body, NamedIn(array.path, body)))));
}

}  // namespace

template <ExprLowerer Lowerer>
auto LowerArrayIteratorMethod(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& c,
    const hir::ArrayIteratorMethodRef& ref, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  using hir::ArrayIteratorMethod;
  switch (ref.method) {
    case ArrayIteratorMethod::kSort:
      return LowerSort(lowerer, frame, c, ref, hir::BinaryOp::kLessThan);
    case ArrayIteratorMethod::kRsort:
      return LowerSort(lowerer, frame, c, ref, hir::BinaryOp::kGreaterThan);
    case ArrayIteratorMethod::kSum:
      return LowerReduction(
          lowerer, frame, c, ref, hir::BinaryOp::kAdd, result_type);
    case ArrayIteratorMethod::kProduct:
      return LowerReduction(
          lowerer, frame, c, ref, hir::BinaryOp::kMul, result_type);
    case ArrayIteratorMethod::kAnd:
      return LowerReduction(
          lowerer, frame, c, ref, hir::BinaryOp::kBitwiseAnd, result_type);
    case ArrayIteratorMethod::kOr:
      return LowerReduction(
          lowerer, frame, c, ref, hir::BinaryOp::kBitwiseOr, result_type);
    case ArrayIteratorMethod::kXor:
      return LowerReduction(
          lowerer, frame, c, ref, hir::BinaryOp::kBitwiseXor, result_type);
    case ArrayIteratorMethod::kFind:
      return LowerLocator(
          lowerer, frame, c, ref, LocatedEntries::kEvery,
          CollectedPart::kElement, result_type);
    case ArrayIteratorMethod::kFindIndex:
      return LowerLocator(
          lowerer, frame, c, ref, LocatedEntries::kEvery, CollectedPart::kIndex,
          result_type);
    case ArrayIteratorMethod::kFindFirst:
      return LowerLocator(
          lowerer, frame, c, ref, LocatedEntries::kFirst,
          CollectedPart::kElement, result_type);
    case ArrayIteratorMethod::kFindFirstIndex:
      return LowerLocator(
          lowerer, frame, c, ref, LocatedEntries::kFirst, CollectedPart::kIndex,
          result_type);
    case ArrayIteratorMethod::kFindLast:
      return LowerLocator(
          lowerer, frame, c, ref, LocatedEntries::kLast,
          CollectedPart::kElement, result_type);
    case ArrayIteratorMethod::kFindLastIndex:
      return LowerLocator(
          lowerer, frame, c, ref, LocatedEntries::kLast, CollectedPart::kIndex,
          result_type);
    case ArrayIteratorMethod::kMin:
      return LowerMinMax(
          lowerer, frame, c, ref, hir::BinaryOp::kLessThan, result_type);
    case ArrayIteratorMethod::kMax:
      return LowerMinMax(
          lowerer, frame, c, ref, hir::BinaryOp::kGreaterThan, result_type);
    case ArrayIteratorMethod::kUnique:
      return LowerUnique(
          lowerer, frame, c, ref, CollectedPart::kElement, result_type);
    case ArrayIteratorMethod::kUniqueIndex:
      return LowerUnique(
          lowerer, frame, c, ref, CollectedPart::kIndex, result_type);
    case ArrayIteratorMethod::kMap:
      return LowerMap(lowerer, frame, c, ref, result_type);
  }
  throw InternalError("LowerArrayIteratorMethod: unknown array method");
}

template auto LowerArrayIteratorMethod(
    ProcessLowerer&, WalkFrame, const hir::CallExpr&,
    const hir::ArrayIteratorMethodRef&, mir::TypeId) -> diag::Result<mir::Expr>;
template auto LowerArrayIteratorMethod(
    const StructuralScopeLowerer&, WalkFrame, const hir::CallExpr&,
    const hir::ArrayIteratorMethodRef&, mir::TypeId) -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
