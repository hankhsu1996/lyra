#include "lyra/lowering/ast_to_hir/expression/aggregates.hpp"

#include <algorithm>
#include <expected>
#include <optional>
#include <utility>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/CallExpression.h>
#include <slang/ast/expressions/LiteralExpressions.h>
#include <slang/ast/expressions/OperatorExpressions.h>
#include <slang/ast/expressions/SelectExpressions.h>
#include <slang/ast/symbols/ClassSymbols.h>
#include <slang/ast/types/AllTypes.h>
#include <slang/ast/types/Type.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/frontend/slang_source_span.hpp"
#include "lyra/hir/range_bounds.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/lowering/ast_to_hir/expression/slang_atoms.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// LRM 11.4.14.4: the elements a `with` names, as the front end bound them: a
// select of one element, or of a run named by its two ends or by a start and a
// width. Its expressions are the source's, lowered as any operand is.
template <ExprLowerer Lowerer>
auto LowerStreamRange(
    Lowerer& lowerer, WalkFrame frame, const slang::ast::Expression& with,
    bool over_a_queue) -> diag::Result<hir::StreamRange> {
  const WalkFrame bound_frame =
      over_a_queue ? frame.WithinQueueSelect() : frame;
  const auto lower =
      [&](const slang::ast::Expression& bound) -> diag::Result<hir::ExprId> {
    auto lowered = lowerer.LowerExpr(bound, bound_frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    return frame.Exprs().Add(*std::move(lowered));
  };
  if (with.kind == slang::ast::ExpressionKind::ElementSelect) {
    auto element =
        lower(with.as<slang::ast::ElementSelectExpression>().selector());
    if (!element) return std::unexpected(std::move(element.error()));
    return hir::StreamRange{*element};
  }
  const auto& range = with.as<slang::ast::RangeSelectExpression>();
  auto left = lower(range.left());
  if (!left) return std::unexpected(std::move(left.error()));
  auto right = lower(range.right());
  if (!right) return std::unexpected(std::move(right.error()));
  switch (range.getSelectionKind()) {
    case slang::ast::RangeSelectionKind::Simple:
      return hir::StreamRange{
          hir::RangeConstantBounds{.left_bound = *left, .right_bound = *right}};
    case slang::ast::RangeSelectionKind::IndexedUp:
      return hir::StreamRange{
          hir::RangeIndexedUpBounds{.base_index = *left, .width = *right}};
    case slang::ast::RangeSelectionKind::IndexedDown:
      return hir::StreamRange{
          hir::RangeIndexedDownBounds{.base_index = *left, .width = *right}};
  }
  throw InternalError("LowerStreamRange: unknown slang RangeSelectionKind");
}

}  // namespace

template <ExprLowerer Lowerer>
auto LowerConcatExpr(
    Lowerer& lowerer, WalkFrame frame,
    const slang::ast::ConcatenationExpression& cc, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto& unit_lowerer = lowerer.Owner();
  auto type_id = unit_lowerer.InternExpressionType(cc, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  const hir::Type& result_ty = unit_lowerer.Unit().types.Get(*type_id);
  if (!result_ty.IsIntegral() && !result_ty.Is<hir::StringType>() &&
      !result_ty.Is<hir::QueueType>() &&
      !result_ty.Is<hir::UnpackedArrayType>() &&
      !result_ty.Is<hir::DynamicArrayType>()) {
    // LRM 11.4.12, 10.10.
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "a concatenation whose result is not a string, a packed value, or an "
        "unpacked array is not yet supported");
  }
  std::vector<hir::ExprId> operand_ids;
  operand_ids.reserve(cc.operands().size());
  for (const auto* op : cc.operands()) {
    // LRM 11.4.12.1: a zero-multiplier replication contributes no bits to
    // the enclosing concatenation. Slang types such Replication nodes as
    // `void`; recognize that exact AST shape so we drop only the documented
    // zero-rep case and let any other unexpected void surface as an error
    // through the normal type-kind check downstream.
    if (op->kind == slang::ast::ExpressionKind::Replication &&
        op->type->isVoid()) {
      continue;
    }
    auto operand_or = lowerer.LowerExpr(*op, frame);
    if (!operand_or) return std::unexpected(std::move(operand_or.error()));
    operand_ids.push_back(frame.Exprs().Add(*std::move(operand_or)));
  }
  return hir::Expr{
      .type = *type_id,
      .data = hir::ConcatExpr{.operands = std::move(operand_ids)},
      .span = span,
  };
}

template <ExprLowerer Lowerer>
auto LowerStreamingConcatExpr(
    Lowerer& lowerer, WalkFrame frame,
    const slang::ast::StreamingConcatenationExpression& sc,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  auto type_id = lowerer.Owner().InternExpressionType(sc, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  std::vector<hir::StreamOperand> operands;
  operands.reserve(sc.streams().size());
  for (const auto& stream : sc.streams()) {
    const slang::ast::Expression& operand = *stream.operand;
    auto value = lowerer.LowerExpr(operand, frame);
    if (!value) return std::unexpected(std::move(value.error()));
    hir::StreamOperand lowered{
        .value = frame.Exprs().Add(*std::move(value)), .with = std::nullopt};
    if (stream.withExpr != nullptr) {
      auto range = LowerStreamRange(
          lowerer, frame, *stream.withExpr, operand.type->isQueue());
      if (!range) return std::unexpected(std::move(range.error()));
      lowered.with = *std::move(range);
    }
    operands.push_back(std::move(lowered));
  }
  return hir::Expr{
      .type = *type_id,
      .data =
          hir::StreamingConcatExpr{
              .operands = std::move(operands), .block_bits = sc.getSliceSize()},
      .span = span,
  };
}

template <ExprLowerer Lowerer>
auto LowerReplicationExpr(
    Lowerer& lowerer, WalkFrame frame,
    const slang::ast::ReplicationExpression& rp, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto& unit_lowerer = lowerer.Owner();
  auto type_id = unit_lowerer.InternExpressionType(rp, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  const hir::Type& result_ty = unit_lowerer.Unit().types.Get(*type_id);
  if (!result_ty.IsIntegral() && !result_ty.Is<hir::StringType>()) {
    // LRM 11.4.12.1.
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "a replication whose result is neither a string nor a packed value is "
        "not yet supported");
  }
  auto count_or = lowerer.LowerExpr(rp.count(), frame);
  if (!count_or) return std::unexpected(std::move(count_or.error()));
  const hir::ExprId count_id = frame.Exprs().Add(*std::move(count_or));
  auto concat_or = lowerer.LowerExpr(rp.concat(), frame);
  if (!concat_or) return std::unexpected(std::move(concat_or.error()));
  const hir::ExprId concat_id = frame.Exprs().Add(*std::move(concat_or));
  return hir::Expr{
      .type = *type_id,
      .data = hir::ReplicationExpr{.count = count_id, .concat = concat_id},
      .span = span,
  };
}

template <ExprLowerer Lowerer>
auto LowerAssignmentPatternFromElements(
    Lowerer& lowerer, WalkFrame frame,
    const slang::ast::AssignmentPatternExpressionBase& ap,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  auto type_id = lowerer.Owner().InternExpressionType(ap, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  std::vector<hir::ExprId> element_ids;
  element_ids.reserve(ap.elements().size());
  for (const auto* elem : ap.elements()) {
    // Where the pattern is a target, each member is the lvalue that takes its
    // share, and HIR carries just that.
    if (const slang::ast::Expression* lvalue = ArgLValue(*elem)) {
      elem = lvalue;
    }
    auto lowered = lowerer.LowerExpr(*elem, frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    element_ids.push_back(frame.Exprs().Add(*std::move(lowered)));
  }
  return hir::Expr{
      .type = *type_id,
      .data = hir::AssignmentPatternExpr{.elements = std::move(element_ids)},
      .span = span,
  };
}

template <ExprLowerer Lowerer>
auto LowerAssociativeAssignmentPattern(
    Lowerer& lowerer, WalkFrame frame,
    const slang::ast::StructuredAssignmentPatternExpression& ap,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  auto type_id = lowerer.Owner().InternExpressionType(ap, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  std::vector<hir::AssociativeAssignmentPatternExpr::Entry> entries;
  entries.reserve(ap.indexSetters.size());
  for (const auto& setter : ap.indexSetters) {
    auto key_or = lowerer.LowerExpr(*setter.index, frame);
    if (!key_or) return std::unexpected(std::move(key_or.error()));
    const hir::ExprId key_id = frame.Exprs().Add(*std::move(key_or));
    auto value_or = lowerer.LowerExpr(*setter.expr, frame);
    if (!value_or) return std::unexpected(std::move(value_or.error()));
    const hir::ExprId value_id = frame.Exprs().Add(*std::move(value_or));
    entries.push_back({.key = key_id, .value = value_id});
  }
  std::optional<hir::ExprId> default_id;
  if (ap.defaultSetter != nullptr) {
    auto default_or = lowerer.LowerExpr(*ap.defaultSetter, frame);
    if (!default_or) return std::unexpected(std::move(default_or.error()));
    default_id = frame.Exprs().Add(*std::move(default_or));
  }
  return hir::Expr{
      .type = *type_id,
      .data =
          hir::AssociativeAssignmentPatternExpr{
              .entries = std::move(entries),
              .default_value = default_id,
          },
      .span = span,
  };
}

// Whether a pattern's target is an array whose elements a key or a position
// names, packed or not.
auto TargetsAFixedArray(const slang::ast::AssignmentPatternExpressionBase& ap)
    -> bool {
  const auto kind = ap.type->getCanonicalType().kind;
  return kind == slang::ast::SymbolKind::FixedSizeUnpackedArrayType ||
         kind == slang::ast::SymbolKind::PackedArrayType;
}

// An array every element of which takes `value`: the keyed form with no key,
// which stays one expression however many elements there are.
template <ExprLowerer Lowerer>
auto LowerArrayOfOneValue(
    Lowerer& lowerer, WalkFrame frame,
    const slang::ast::AssignmentPatternExpressionBase& ap,
    const slang::ast::Expression& value, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto type_id = lowerer.Owner().InternExpressionType(ap, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  auto lowered = lowerer.LowerExpr(value, frame);
  if (!lowered) return std::unexpected(std::move(lowered.error()));
  return hir::Expr{
      .type = *type_id,
      .data =
          hir::AssignmentPatternKeyedExpr{
              .entries = {},
              .default_value = frame.Exprs().Add(*std::move(lowered))},
      .span = span,
  };
}

template <ExprLowerer Lowerer>
auto LowerSimpleAssignmentPattern(
    Lowerer& lowerer, WalkFrame frame,
    const slang::ast::SimpleAssignmentPatternExpression& ap,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  // LRM 10.9.1: a `default` or type key reaching a subarray descends into it,
  // and the front end states the outcome as a positional pattern holding the
  // one value it matched at every position. A pattern the source wrote binds
  // each of its elements apart, so one expression at every position is that
  // descent.
  const auto elements = ap.elements();
  if (TargetsAFixedArray(ap) && elements.size() > 1 &&
      std::ranges::all_of(elements, [&](const slang::ast::Expression* each) {
        return each == elements.front();
      })) {
    return LowerArrayOfOneValue(lowerer, frame, ap, *elements.front(), span);
  }
  // LRM 10.9.1 positional assignment pattern `'{a, b, ...}`. It is the same
  // list of elements on either side of an assignment: values that build the
  // aggregate, or the destinations one is taken apart into (LRM 10.9).
  return LowerAssignmentPatternFromElements(lowerer, frame, ap, span);
}

// The value every element no index key names takes (LRM 10.9.1), or none where
// the keys name them all. The front end has matched it already -- a type key,
// the default sized to the element, or the descent into a subarray or a
// structure -- and states it once, at each of those positions.
auto ValueOfUnkeyedElements(
    const slang::ast::StructuredAssignmentPatternExpression& ap)
    -> const slang::ast::Expression* {
  for (const slang::ast::Expression* element : ap.elements()) {
    const bool keyed = std::ranges::any_of(
        ap.indexSetters,
        [&](const auto& setter) { return setter.expr == element; });
    if (!keyed) {
      return element;
    }
  }
  return nullptr;
}

template <ExprLowerer Lowerer>
auto LowerStructuredAssignmentPattern(
    Lowerer& lowerer, WalkFrame frame,
    const slang::ast::StructuredAssignmentPatternExpression& ap,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  // LRM 10.9.1 structured assignment pattern `'{key: value, ...}`. An
  // associative literal keeps its keyed pairs and its own optional default
  // (LRM 7.9.11): its keys are arbitrary values with no positional meaning, and
  // its default outlives the build, because a read of an absent key returns it.
  const auto target_kind = ap.type->getCanonicalType().kind;
  if (target_kind == slang::ast::SymbolKind::AssociativeArrayType) {
    return LowerAssociativeAssignmentPattern(lowerer, frame, ap, span);
  }
  // An array target keeps its keys. Its elements share one type, so what a
  // default fills them with repeats and stays one expression however many
  // elements there are -- written out instead, a 32768-element array reaches
  // the target language as a four-megabyte expression that no compiler will
  // accept. A struct's members do not share a type, so its defaulted members
  // are as many distinct values as there are members and an element list is
  // what they are.
  //
  // Whether the array is packed decides how a key resolves to a position and
  // how the repeat is spelled, neither of which is what the pattern says, so
  // that question belongs to HIR-to-MIR.
  if (TargetsAFixedArray(ap)) {
    auto type_id = lowerer.Owner().InternExpressionType(ap, span);
    if (!type_id) return std::unexpected(std::move(type_id.error()));
    std::optional<hir::ExprId> default_id;
    if (const slang::ast::Expression* unkeyed = ValueOfUnkeyedElements(ap)) {
      auto value = lowerer.LowerExpr(*unkeyed, frame);
      if (!value) return std::unexpected(std::move(value.error()));
      default_id = frame.Exprs().Add(*std::move(value));
    }
    std::vector<hir::AssignmentPatternKeyedExpr::Entry> entries;
    entries.reserve(ap.indexSetters.size());
    for (const auto& setter : ap.indexSetters) {
      auto index = lowerer.LowerExpr(*setter.index, frame);
      if (!index) return std::unexpected(std::move(index.error()));
      auto value = lowerer.LowerExpr(*setter.expr, frame);
      if (!value) return std::unexpected(std::move(value.error()));
      entries.push_back(
          {.index = frame.Exprs().Add(*std::move(index)),
           .value = frame.Exprs().Add(*std::move(value))});
    }
    return hir::Expr{
        .type = *type_id,
        .data =
            hir::AssignmentPatternKeyedExpr{
                .entries = std::move(entries), .default_value = default_id},
        .span = span,
    };
  }
  return LowerAssignmentPatternFromElements(lowerer, frame, ap, span);
}

template <ExprLowerer Lowerer>
auto LowerReplicatedAssignmentPatternExpr(
    Lowerer& lowerer, WalkFrame frame,
    const slang::ast::ReplicatedAssignmentPatternExpression& rp,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  auto type_id = lowerer.Owner().InternExpressionType(rp, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  auto count_or = lowerer.LowerExpr(rp.count(), frame);
  if (!count_or) return std::unexpected(std::move(count_or.error()));
  const hir::ExprId count_id = frame.Exprs().Add(*std::move(count_or));
  std::vector<hir::ExprId> item_ids;
  item_ids.reserve(rp.elements().size());
  for (const auto* elem : rp.elements()) {
    auto lowered = lowerer.LowerExpr(*elem, frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    item_ids.push_back(frame.Exprs().Add(*std::move(lowered)));
  }
  return hir::Expr{
      .type = *type_id,
      .data =
          hir::AssignmentPatternReplicationExpr{
              .count = count_id,
              .items = std::move(item_ids),
          },
      .span = span,
  };
}

// LRM 7.5.1 `new[N]` / `new[N](other)` dynamic-array constructor. Result type
// is the dynamic array type slang resolved for the expression; the optional
// initializer is `(other)` -- the LRM 7.5.1 source array for the copy-with-
// pad-or-truncate form.
template <ExprLowerer Lowerer>
auto LowerNewArrayExpr(
    Lowerer& lowerer, WalkFrame frame, const slang::ast::NewArrayExpression& na,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  auto type_id = lowerer.Owner().InternExpressionType(na, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  auto size_or = lowerer.LowerExpr(na.sizeExpr(), frame);
  if (!size_or) return std::unexpected(std::move(size_or.error()));
  const hir::ExprId size_id = frame.Exprs().Add(*std::move(size_or));
  std::optional<hir::ExprId> initializer_id;
  if (na.initExpr() != nullptr) {
    auto init_or = lowerer.LowerExpr(*na.initExpr(), frame);
    if (!init_or) return std::unexpected(std::move(init_or.error()));
    initializer_id = frame.Exprs().Add(*std::move(init_or));
  }
  return hir::Expr{
      .type = *type_id,
      .data =
          hir::DynamicArrayNewExpr{
              .size = size_id,
              .initializer = initializer_id,
          },
      .span = span,
  };
}

template <ExprLowerer Lowerer>
auto LowerNewClassExpr(
    Lowerer& lowerer, WalkFrame frame, const slang::ast::NewClassExpression& nc,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  auto& unit_lowerer = lowerer.Owner();
  if (nc.isSuperClass) {
    // A `super.new(...)` is a construction-protocol fact that AST-to-HIR
    // lifts onto the enclosing class declaration; the statement wrapping it
    // in the ctor body lowers as empty. Reaching this expression-position
    // path means the source placed `super.new` somewhere the LRM forbids
    // (LRM 8.7 restricts it to the first ctor statement).
    throw InternalError(
        "AST->HIR super.new: NewClassExpression outside the first-statement "
        "position of a derived constructor body");
  }
  std::vector<hir::ExprId> arguments;
  if (const auto* call = nc.constructorCall(); call != nullptr) {
    const auto& actuals = call->as<slang::ast::CallExpression>().arguments();
    arguments.reserve(actuals.size());
    for (const auto* actual : actuals) {
      auto arg_or = lowerer.LowerExpr(*actual, frame);
      if (!arg_or) return std::unexpected(std::move(arg_or.error()));
      arguments.push_back(frame.Exprs().Add(*std::move(arg_or)));
    }
  }
  const auto& cls = nc.type->getCanonicalType().as<slang::ast::ClassType>();
  auto class_id = unit_lowerer.ResolveClass(cls, span);
  if (!class_id) return std::unexpected(std::move(class_id.error()));
  auto type_id = unit_lowerer.InternExpressionType(nc, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  auto declaring_instance =
      unit_lowerer.DeclaringInstanceFrom(cls, frame, span);
  if (!declaring_instance) {
    return std::unexpected(std::move(declaring_instance.error()));
  }
  return hir::Expr{
      .type = *type_id,
      .data =
          hir::ClassNewExpr{
              .class_id = *class_id,
              .arguments = std::move(arguments),
              .declaring_instance = *std::move(declaring_instance)},
      .span = span,
  };
}

// One concrete instantiation per pass class; the templates are defined here so
// the dispatchers in lower.cpp link against the symbols emitted in this file.
template auto LowerConcatExpr(
    ProcessLowerer&, WalkFrame, const slang::ast::ConcatenationExpression&,
    diag::SourceSpan) -> diag::Result<hir::Expr>;
template auto LowerConcatExpr(
    StructuralScopeLowerer&, WalkFrame,
    const slang::ast::ConcatenationExpression&, diag::SourceSpan)
    -> diag::Result<hir::Expr>;
template auto LowerStreamingConcatExpr(
    ProcessLowerer&, WalkFrame,
    const slang::ast::StreamingConcatenationExpression&, diag::SourceSpan)
    -> diag::Result<hir::Expr>;
template auto LowerStreamingConcatExpr(
    StructuralScopeLowerer&, WalkFrame,
    const slang::ast::StreamingConcatenationExpression&, diag::SourceSpan)
    -> diag::Result<hir::Expr>;
template auto LowerAssignmentPatternFromElements(
    ProcessLowerer&, WalkFrame,
    const slang::ast::AssignmentPatternExpressionBase&, diag::SourceSpan)
    -> diag::Result<hir::Expr>;
template auto LowerAssignmentPatternFromElements(
    StructuralScopeLowerer&, WalkFrame,
    const slang::ast::AssignmentPatternExpressionBase&, diag::SourceSpan)
    -> diag::Result<hir::Expr>;
template auto LowerReplicatedAssignmentPatternExpr(
    ProcessLowerer&, WalkFrame,
    const slang::ast::ReplicatedAssignmentPatternExpression&, diag::SourceSpan)
    -> diag::Result<hir::Expr>;
template auto LowerReplicatedAssignmentPatternExpr(
    StructuralScopeLowerer&, WalkFrame,
    const slang::ast::ReplicatedAssignmentPatternExpression&, diag::SourceSpan)
    -> diag::Result<hir::Expr>;
template auto LowerAssociativeAssignmentPattern(
    ProcessLowerer&, WalkFrame,
    const slang::ast::StructuredAssignmentPatternExpression&, diag::SourceSpan)
    -> diag::Result<hir::Expr>;
template auto LowerAssociativeAssignmentPattern(
    StructuralScopeLowerer&, WalkFrame,
    const slang::ast::StructuredAssignmentPatternExpression&, diag::SourceSpan)
    -> diag::Result<hir::Expr>;
template auto LowerSimpleAssignmentPattern(
    ProcessLowerer&, WalkFrame,
    const slang::ast::SimpleAssignmentPatternExpression&, diag::SourceSpan)
    -> diag::Result<hir::Expr>;
template auto LowerSimpleAssignmentPattern(
    StructuralScopeLowerer&, WalkFrame,
    const slang::ast::SimpleAssignmentPatternExpression&, diag::SourceSpan)
    -> diag::Result<hir::Expr>;
template auto LowerStructuredAssignmentPattern(
    ProcessLowerer&, WalkFrame,
    const slang::ast::StructuredAssignmentPatternExpression&, diag::SourceSpan)
    -> diag::Result<hir::Expr>;
template auto LowerStructuredAssignmentPattern(
    StructuralScopeLowerer&, WalkFrame,
    const slang::ast::StructuredAssignmentPatternExpression&, diag::SourceSpan)
    -> diag::Result<hir::Expr>;
template auto LowerReplicationExpr(
    ProcessLowerer&, WalkFrame, const slang::ast::ReplicationExpression&,
    diag::SourceSpan) -> diag::Result<hir::Expr>;
template auto LowerReplicationExpr(
    StructuralScopeLowerer&, WalkFrame,
    const slang::ast::ReplicationExpression&, diag::SourceSpan)
    -> diag::Result<hir::Expr>;
template auto LowerNewArrayExpr(
    ProcessLowerer&, WalkFrame, const slang::ast::NewArrayExpression&,
    diag::SourceSpan) -> diag::Result<hir::Expr>;
template auto LowerNewArrayExpr(
    StructuralScopeLowerer&, WalkFrame, const slang::ast::NewArrayExpression&,
    diag::SourceSpan) -> diag::Result<hir::Expr>;
template auto LowerNewClassExpr(
    ProcessLowerer&, WalkFrame, const slang::ast::NewClassExpression&,
    diag::SourceSpan) -> diag::Result<hir::Expr>;
template auto LowerNewClassExpr(
    StructuralScopeLowerer&, WalkFrame, const slang::ast::NewClassExpression&,
    diag::SourceSpan) -> diag::Result<hir::Expr>;

}  // namespace lyra::lowering::ast_to_hir
