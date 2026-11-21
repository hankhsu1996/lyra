#include "lyra/mir_transform/inst_simplify.hpp"

#include <algorithm>
#include <cstdint>
#include <optional>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/constant_value.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/operand_use.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir_transform/block_rebuild.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::mir_transform {

namespace {

// The call `expr` makes of the library entry `entry`, if that is what it is.
auto CallOf(const mir::Expr& expr, support::BuiltinFn entry)
    -> const mir::CallExpr* {
  const auto* call = std::get_if<mir::CallExpr>(&expr.data);
  if (call == nullptr || mir::DirectBuiltinFn(*call) != entry) {
    return nullptr;
  }
  return call;
}

auto ReceiverOf(const mir::CallExpr& call) -> mir::ExprId {
  return *std::get<mir::Direct>(call.callee).receiver;
}

// The call `expr` is where it is a run operation of an array's type -- its
// own body for the run, at the run's type -- which it is handed by reference.
auto RunCallOf(const mir::Expr& expr, support::BuiltinFn operation)
    -> const mir::CallExpr* {
  const auto* call = std::get_if<mir::CallExpr>(&expr.data);
  if (call == nullptr) {
    return nullptr;
  }
  const auto* direct = std::get_if<mir::Direct>(&call->callee);
  if (direct == nullptr || !direct->receiver.has_value()) {
    return nullptr;
  }
  const auto* target = std::get_if<mir::TypeOperationTarget>(&direct->target);
  if (target == nullptr ||
      target->answers != support::ValueOperation{operation} ||
      target->arguments.size() != 1) {
    return nullptr;
  }
  return call;
}

// What a run operation was handed by reference: the operand of the borrow, and
// anything else as it is -- a designation of the part a write is opened on.
auto Handed(const mir::Block& block, mir::ExprId id) -> mir::ExprId {
  if (const auto* borrowed =
          std::get_if<mir::BorrowExpr>(&block.exprs.Get(id).data)) {
    return borrowed->operand;
  }
  return id;
}

auto RunTypeOf(const mir::CallExpr& call) -> mir::TypeId {
  return std::get<mir::TypeOperationTarget>(
             std::get<mir::Direct>(call.callee).target)
      .arguments.front();
}

// Whether the position `id` names is the first, which every select's numbering
// starts from: the constant 0, with no unknown bit, as LLVM's `m_Zero()`
// matches an operand.
auto IsFirstPosition(
    const mir::CompilationUnit& unit, const mir::Block& block, mir::ExprId id)
    -> bool {
  const mir::IntegralConstant* position =
      mir::IntegralPlanesNamedBy(unit.constants, block.exprs.Get(id).data);
  if (position == nullptr) {
    return false;
  }
  const auto is_zero = [](std::uint64_t word) { return word == 0; };
  return std::ranges::all_of(position->value_words, is_zero) &&
         std::ranges::all_of(position->state_words, is_zero);
}

// Whether bits read at type `part` from the first bit of a value of type
// `whole` are every bit of it, held the same way: as many, of the same kind.
auto BitsSpanTheWhole(
    const mir::CompilationUnit& unit, mir::TypeId part, mir::TypeId whole)
    -> bool {
  const mir::Type& part_ty = unit.types.Get(part);
  const mir::Type& whole_ty = unit.types.Get(whole);
  return part_ty.IsIntegral() && whole_ty.IsIntegral() &&
         part_ty.Integral().bit_width == whole_ty.Integral().bit_width &&
         part_ty.Integral().state_kind == whole_ty.Integral().state_kind;
}

// The node `id` of `block` as a slice naming every bit of what it is taken
// on, from its first, where it is one.
auto WholeSliceAt(
    const mir::CompilationUnit& unit, const mir::Block& block, mir::ExprId id)
    -> const mir::SliceExpr* {
  const mir::Expr& expr = block.exprs.Get(id);
  const auto* slice = std::get_if<mir::SliceExpr>(&expr.data);
  if (slice == nullptr || !IsFirstPosition(unit, block, slice->start)) {
    return nullptr;
  }
  mir::TypeId taken_on = block.exprs.Get(slice->receiver).type;
  if (const auto* designated =
          unit.types.Get(taken_on).As<mir::DesignationType>()) {
    taken_on = designated->value;
  }
  return BitsSpanTheWhole(unit, expr.type, taken_on) ? slice : nullptr;
}

// The wrapper a write in progress was opened on, where `designation` is the
// whole of what it holds designated within that write.
auto WrapperWrittenWhole(const mir::Block& block, mir::ExprId designation)
    -> std::optional<mir::ExprId> {
  const mir::CallExpr* whole =
      CallOf(block.exprs.Get(designation), support::BuiltinFn::kDesignateWhole);
  if (whole == nullptr) {
    return std::nullopt;
  }
  const mir::CallExpr* opened = CallOf(
      block.exprs.Get(ReceiverOf(*whole)), support::BuiltinFn::kOpenForWrite);
  if (opened == nullptr) {
    return std::nullopt;
  }
  return ReceiverOf(*opened);
}

// Which nodes of `block` are taken as the storage itself where they stand.
auto TakenAsStorage(const mir::CompilationUnit& unit, const mir::Block& block)
    -> std::vector<bool> {
  std::vector<bool> taken(block.exprs.size());
  for (const mir::Expr& expr : block.exprs) {
    mir::ForEachOperandAsTaken(
        unit.types, expr, [&](mir::ExprId operand, mir::OperandUse use) {
          if (use == mir::OperandUse::kStorage) {
            taken.at(operand.value) = true;
          }
        });
  }
  return taken;
}

// The rewrite of one body: each select of the whole is the whole. A select
// stands in a read, as a value of its own kind of part, or as the destination
// of a write, where what it reaches is the whole that write lands on.
class Simplifier {
 public:
  explicit Simplifier(const mir::CompilationUnit& unit) : unit_(&unit) {
  }

  // Whether the node `expr` of `block` is a select of the whole of what it is
  // taken on, in any of the positions one stands in.
  [[nodiscard]] auto Fires(const mir::Block& block, const mir::Expr& expr) const
      -> bool {
    if (const auto* read = std::get_if<mir::CopyExpr>(&expr.data)) {
      const mir::SliceExpr* whole = WholeSliceAt(*unit_, block, read->place);
      return whole != nullptr &&
             !unit_->types.Get(block.exprs.Get(whole->receiver).type)
                  .Is<mir::DesignationType>();
    }
    if (const mir::CallExpr* read =
            RunCallOf(expr, support::BuiltinFn::kElementSlice)) {
      return IsFirstPosition(
                 *unit_, block, Handed(block, read->arguments[0])) &&
             RunTypeOf(*read) ==
                 block.exprs.Get(Handed(block, ReceiverOf(*read))).type;
    }
    if (const auto* write = std::get_if<mir::AssignExpr>(&expr.data)) {
      return WholeSliceAt(*unit_, block, write->target) != nullptr;
    }
    return false;
  }

  // Whether anything in `block`, or in a block nested in it, is rewritten.
  [[nodiscard]] auto FiresIn(const mir::Block& block) const -> bool {
    return std::ranges::any_of(
               block.exprs,
               [&](const mir::Expr& expr) { return Fires(block, expr); }) ||
           std::ranges::any_of(
               block.child_scopes,
               [&](const mir::Block& child) { return FiresIn(child); });
  }

  // The node of `block` that the node `id` is, where it is a read of the whole
  // of what it is taken on at that value's own type: the value, where the read
  // stands as a value; and a place it is taken on, where the read is taken as
  // the storage itself, since a select naming all of a place is an lvalue
  // naming it (LRM 11.5.1). `taken_as_storage` says how the node is taken.
  [[nodiscard]] auto StandsFor(
      const mir::Block& block, mir::ExprId id, bool taken_as_storage) const
      -> std::optional<mir::ExprId> {
    const std::optional<mir::ExprId> whole = WholeRead(block, id);
    if (!whole.has_value() ||
        block.exprs.Get(*whole).type != block.exprs.Get(id).type) {
      return std::nullopt;
    }
    if (mir::IsPlace(block.exprs.Get(*whole).data) != taken_as_storage) {
      return std::nullopt;
    }
    // Bits are no storage, so a read taken as storage stands for its whole
    // only where that whole lies at an address of its own.
    if (taken_as_storage &&
        std::holds_alternative<mir::SliceExpr>(block.exprs.Get(*whole).data)) {
      return std::nullopt;
    }
    return whole;
  }

  // The node standing in the place of the node `id`, where it is a select of
  // the whole and is no node that already exists. A read of the whole of a
  // value taken as the storage itself is left as it is: what it is taken as is
  // the value it makes, which no node names.
  auto Node(BlockRebuild& rebuild, mir::ExprId id, bool taken_as_storage) const
      -> std::optional<mir::Expr> {
    const mir::Block& from = rebuild.From();
    const mir::Expr& expr = from.exprs.Get(id);
    if (!Fires(from, expr)) {
      return std::nullopt;
    }
    if (const std::optional<mir::ExprId> whole = WholeRead(from, id)) {
      const mir::Expr& taken_on = from.exprs.Get(*whole);
      if (!mir::IsPlace(taken_on.data) && taken_on.type == expr.type &&
          taken_as_storage) {
        return std::nullopt;
      }
      return Read(rebuild, *whole, expr.type);
    }
    const auto& write = std::get<mir::AssignExpr>(expr.data);
    return WholeWrite(
        rebuild, WholeSliceAt(*unit_, from, write.target)->receiver,
        write.value);
  }

 private:
  // What the node `id` of `block` reads the whole of, where it is a read of
  // every part of a value from its first.
  [[nodiscard]] auto WholeRead(const mir::Block& block, mir::ExprId id) const
      -> std::optional<mir::ExprId> {
    const mir::Expr& expr = block.exprs.Get(id);
    if (const mir::CallExpr* run =
            RunCallOf(expr, support::BuiltinFn::kElementSlice);
        run != nullptr && Fires(block, expr)) {
      return Handed(block, ReceiverOf(*run));
    }
    const auto* read = std::get_if<mir::CopyExpr>(&expr.data);
    if (read == nullptr || !Fires(block, expr)) {
      return std::nullopt;
    }
    return WholeSliceAt(*unit_, block, read->place)->receiver;
  }

  // The whole of what `whole` names, at the select's type: read where it lies
  // in a place, and converted where the select's type is another, since a
  // select is unsigned whatever the value it is taken from is (LRM 11.8.1).
  [[nodiscard]] auto Read(
      BlockRebuild& rebuild, mir::ExprId whole, mir::TypeId type) const
      -> mir::Expr {
    const mir::Expr& taken_on = rebuild.From().exprs.Get(whole);
    const mir::ExprId reached = rebuild.Expr(whole);
    if (mir::IsPlace(taken_on.data) && taken_on.type == type) {
      return mir::MakeCopyExpr(reached, type);
    }
    const mir::ExprId value =
        mir::IsPlace(taken_on.data)
            ? rebuild.Add(mir::MakeCopyExpr(reached, taken_on.type))
            : reached;
    return mir::MakeIntegralConversion(*unit_, rebuild.To(), value, type);
  }

  // The write of `value` into the whole of the place `destination`: the
  // wrapper's own whole write where the place is all of what a write opened on
  // it holds, and an assignment to the place otherwise.
  auto WholeWrite(
      BlockRebuild& rebuild, mir::ExprId destination, mir::ExprId value) const
      -> mir::Expr {
    const mir::Block& from = rebuild.From();
    const auto* designated = unit_->types.Get(from.exprs.Get(destination).type)
                                 .As<mir::DesignationType>();
    const mir::TypeId landed = designated != nullptr
                                   ? designated->value
                                   : from.exprs.Get(destination).type;
    // What is written is held at the select's type, which a signed whole
    // differs from in its signedness alone.
    mir::ExprId written = rebuild.Expr(value);
    if (from.exprs.Get(value).type != landed) {
      written = rebuild.Add(
          mir::MakeIntegralConversion(*unit_, rebuild.To(), written, landed));
    }
    const auto set_on = [&](mir::ExprId receiver) {
      return mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kSet,
                          .receiver = receiver},
                  .arguments = {written}},
          .type = unit_->builtins.void_type};
    };
    if (designated == nullptr) {
      return mir::MakeAssignExpr(
          unit_->builtins, rebuild.Expr(destination), written);
    }
    // The whole a write in progress designates is landed on, or set on the
    // wrapper the write was opened on where it designates all of it.
    if (const std::optional<mir::ExprId> wrapper =
            WrapperWrittenWhole(from, destination)) {
      return set_on(rebuild.Expr(*wrapper));
    }
    return mir::MakeAssignExpr(
        unit_->builtins,
        rebuild.Add(mir::MakeDerefExpr(rebuild.Expr(destination), landed)),
        written);
  }

  const mir::CompilationUnit* unit_;
};

}  // namespace

auto InstSimplifyOnBody(
    const PassContext& context, const mir::CallableCode& code)
    -> std::optional<mir::CallableCode> {
  const mir::CompilationUnit& unit = *context.unit;
  const Simplifier simplifier(unit);
  if (!code.body.has_value() || !simplifier.FiresIn(*code.body)) {
    return std::nullopt;
  }
  // Which nodes of each block of this body are taken as the storage itself,
  // worked out once for a block the rewrite reaches.
  std::unordered_map<const mir::Block*, std::vector<bool>> taken_as_storage;
  const auto taken_at = [&](const mir::Block& block, mir::ExprId id) {
    auto [found, inserted] = taken_as_storage.try_emplace(&block);
    if (inserted) {
      found->second = TakenAsStorage(unit, block);
    }
    return found->second.at(id.value);
  };
  const RebuildRules rules{
      .stands_for =
          [&](const mir::Block& block, mir::ExprId id) {
            return simplifier.StandsFor(block, id, taken_at(block, id));
          },
      .expr =
          [&](BlockRebuild& rebuild, mir::ExprId id) {
            return simplifier.Node(rebuild, id, taken_at(rebuild.From(), id));
          }};
  return RebuildCode(code, rules);
}

}  // namespace lyra::mir_transform
