#include "lyra/mir_transform/inst_simplify.hpp"

#include <algorithm>
#include <cstdint>
#include <optional>
#include <span>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/code_source.hpp"
#include "lyra/mir/compilation_unit.hpp"
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

auto TypeArgumentOf(const mir::CallExpr& call) -> mir::TypeId {
  return *std::get<mir::Direct>(call.callee).type_argument;
}

// Whether the position `id` names is the first, which every select's numbering
// starts from: the constant 0, with no unknown bit, as LLVM's `m_Zero()`
// matches an operand.
auto IsFirstPosition(
    const mir::CompilationUnit& unit, const mir::Block& block, mir::ExprId id)
    -> bool {
  const auto* reference =
      std::get_if<mir::ReferenceExpr>(&block.exprs.Get(id).data);
  const auto* constant =
      reference == nullptr
          ? nullptr
          : std::get_if<mir::IntegralConstantRef>(&reference->target);
  if (constant == nullptr) {
    return false;
  }
  const mir::IntegralConstant& position =
      unit.integral_constants.Get(constant->constant).value;
  const auto is_zero = [](std::uint64_t word) { return word == 0; };
  return std::ranges::all_of(position.value_words, is_zero) &&
         std::ranges::all_of(position.state_words, is_zero);
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

// The value a write reaching through a place of type `type` lands on: what a
// designation designates within a write in progress, the bits a reference to
// bits reaches, or what the place itself holds.
auto WrittenValueType(const mir::CompilationUnit& unit, mir::TypeId type)
    -> mir::TypeId {
  const mir::Type& ty = unit.types.Get(type);
  if (const auto* designation = ty.As<mir::DesignationType>()) {
    return designation->value;
  }
  if (const auto* bits = ty.As<mir::BitsReferenceType>()) {
    return bits->bits;
  }
  return type;
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
    if (const mir::CallExpr* read = CallOf(expr, support::BuiltinFn::kSlice)) {
      return IsFirstPosition(*unit_, block, read->arguments[0]) &&
             BitsSpanTheWhole(
                 *unit_, TypeArgumentOf(*read),
                 block.exprs.Get(ReceiverOf(*read)).type);
    }
    if (const mir::CallExpr* read =
            CallOf(expr, support::BuiltinFn::kElementSlice)) {
      return IsFirstPosition(*unit_, block, read->arguments[0]) &&
             TypeArgumentOf(*read) == block.exprs.Get(ReceiverOf(*read)).type;
    }
    if (const mir::CallExpr* set = CallOf(expr, support::BuiltinFn::kSet)) {
      const mir::CallExpr* bits = CallOf(
          block.exprs.Get(ReceiverOf(*set)), support::BuiltinFn::kSliceMut);
      return bits != nullptr &&
             IsFirstPosition(*unit_, block, bits->arguments[0]) &&
             BitsSpanTheWhole(
                 *unit_, TypeArgumentOf(*bits),
                 WrittenValueType(
                     *unit_, block.exprs.Get(ReceiverOf(*bits)).type));
    }
    if (const mir::CallExpr* run =
            CallOf(expr, support::BuiltinFn::kAssignElementSlice)) {
      return IsFirstPosition(*unit_, block, run->arguments[0]) &&
             block.exprs.Get(run->arguments[2]).type ==
                 WrittenValueType(
                     *unit_, block.exprs.Get(ReceiverOf(*run)).type);
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
    if (const mir::CallExpr* set = CallOf(expr, support::BuiltinFn::kSet)) {
      const mir::CallExpr& bits = *CallOf(
          from.exprs.Get(ReceiverOf(*set)), support::BuiltinFn::kSliceMut);
      return WholeWrite(rebuild, ReceiverOf(bits), set->arguments[0]);
    }
    const mir::CallExpr& run =
        *CallOf(expr, support::BuiltinFn::kAssignElementSlice);
    return WholeWrite(rebuild, ReceiverOf(run), run.arguments[2]);
  }

 private:
  // What the node `id` of `block` reads the whole of, where it is a read of
  // every part of a value from its first.
  [[nodiscard]] auto WholeRead(const mir::Block& block, mir::ExprId id) const
      -> std::optional<mir::ExprId> {
    const mir::Expr& expr = block.exprs.Get(id);
    const mir::CallExpr* read = CallOf(expr, support::BuiltinFn::kSlice);
    if (read == nullptr) {
      read = CallOf(expr, support::BuiltinFn::kElementSlice);
    }
    if (read == nullptr || !Fires(block, expr)) {
      return std::nullopt;
    }
    return ReceiverOf(*read);
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

  // The write of `value` into the whole of what `destination` reaches: the
  // wrapper's own whole write where the destination is all of what a write
  // opened on it holds, an assignment where it lands, and the reference's own
  // write where it is a reference to bits.
  auto WholeWrite(
      BlockRebuild& rebuild, mir::ExprId destination, mir::ExprId value) const
      -> mir::Expr {
    const mir::Block& from = rebuild.From();
    const mir::TypeId destination_type = from.exprs.Get(destination).type;
    const mir::TypeId landed = WrittenValueType(*unit_, destination_type);
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
    const mir::Type& reached = unit_->types.Get(destination_type);
    if (reached.Is<mir::DesignationType>()) {
      if (const std::optional<mir::ExprId> wrapper =
              WrapperWrittenWhole(from, destination)) {
        return set_on(rebuild.Expr(*wrapper));
      }
      return mir::MakeAssignExpr(
          unit_->builtins,
          rebuild.Add(mir::MakeDerefExpr(rebuild.Expr(destination), landed)),
          written);
    }
    if (reached.Is<mir::BitsReferenceType>()) {
      return set_on(rebuild.Expr(destination));
    }
    return mir::MakeAssignExpr(
        unit_->builtins, rebuild.Expr(destination), written);
  }

  const mir::CompilationUnit* unit_;
};

}  // namespace

auto InstSimplify(mir::CompilationUnit unit) -> mir::CompilationUnit {
  return mir::RewriteEachCode(
      std::move(unit),
      [](const mir::CompilationUnit& unit, const mir::CodeSource&,
         const mir::CallableCode& code,
         std::span<const mir::ExprId> outside_roots)
          -> std::optional<mir::SourcedCode> {
        const Simplifier simplifier(unit);
        if (!code.body.has_value() || !simplifier.FiresIn(*code.body)) {
          return std::nullopt;
        }
        // Which nodes of each block of this body are taken as the storage
        // itself, worked out once for a block the rewrite reaches.
        std::unordered_map<const mir::Block*, std::vector<bool>>
            taken_as_storage;
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
                  return simplifier.Node(
                      rebuild, id, taken_at(rebuild.From(), id));
                }};
        return RebuildCode(code, outside_roots, rules);
      });
}

}  // namespace lyra::mir_transform
