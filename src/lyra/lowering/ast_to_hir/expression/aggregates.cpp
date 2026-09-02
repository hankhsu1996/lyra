#include "lyra/lowering/ast_to_hir/expression/aggregates.hpp"

#include <cstdint>
#include <expected>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/CallExpression.h>
#include <slang/ast/expressions/LiteralExpressions.h>
#include <slang/ast/expressions/OperatorExpressions.h>
#include <slang/ast/symbols/ClassSymbols.h>
#include <slang/ast/types/AllTypes.h>
#include <slang/ast/types/Type.h>

#include "lyra/diag/diag_code.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/lowering/ast_to_hir/integral_constant.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// LRM 5.7.1: an unbased unsized literal sets every bit of what it is assigned
// to, so what it carries is a bit pattern to repeat rather than a number to
// widen. Slang sizes one at its target wherever the LRM makes the context
// assignment-like, which is why this is spelled out nowhere else.
auto RepeatBitPattern(
    const hir::IntegralConstant& bit, std::uint32_t width, bool four_state,
    bool is_signed) -> hir::IntegralConstant {
  const bool value_bit = (bit.value_words[0] & 1U) != 0;
  const bool state_bit =
      !bit.state_words.empty() && (bit.state_words[0] & 1U) != 0;
  const std::size_t words = (static_cast<std::size_t>(width) + 63U) / 64U;
  const std::uint64_t top_mask = width % 64U == 0
                                     ? ~std::uint64_t{0}
                                     : (std::uint64_t{1} << (width % 64U)) - 1U;

  hir::IntegralConstant filled;
  filled.width = width;
  filled.signedness =
      is_signed ? hir::Signedness::kSigned : hir::Signedness::kUnsigned;
  filled.value_words.assign(words, value_bit ? ~std::uint64_t{0} : 0U);
  filled.value_words.back() &= top_mask;
  if (four_state) {
    filled.state_kind = hir::IntegralStateKind::kFourState;
    filled.state_words.assign(words, state_bit ? ~std::uint64_t{0} : 0U);
    filled.state_words.back() &= top_mask;
  }
  return filled;
}

// The element an array pattern's key designates (LRM 10.9.1). A key names a
// position the way a structure pattern's key names a member, and the front end
// settles which one while binding the pattern -- it accepts no program whose
// key is not a constant -- so the position is read rather than worked out. An
// index runs whichever way its dimension was declared, so it is signed.
auto DesignatedIndex(const slang::ast::Expression& key) -> std::int64_t {
  const slang::ConstantValue* position = key.getConstant();
  const std::optional<std::int64_t> index =
      position != nullptr && *position ? position->integer().as<std::int64_t>()
                                       : std::nullopt;
  if (!index.has_value()) {
    throw InternalError(
        "DesignatedIndex: an array pattern's key designates no element, which "
        "the front end accepts no program for");
  }
  return *index;
}

}  // namespace

template <ExprLowerer Lowerer>
auto LowerConcatExpr(
    Lowerer& lowerer, WalkFrame frame,
    const slang::ast::ConcatenationExpression& cc, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto& unit_lowerer = lowerer.Owner();
  auto type_id = unit_lowerer.InternType(*cc.type, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  const hir::Type& result_ty = unit_lowerer.Unit().types.Get(*type_id);
  if (!result_ty.Is<hir::StringType>() && !result_ty.Is<hir::ScalarBitType>() &&
      !result_ty.Is<hir::PackedArrayType>() &&
      !result_ty.Is<hir::QueueType>() &&
      !result_ty.Is<hir::UnpackedArrayType>() &&
      !result_ty.Is<hir::DynamicArrayType>()) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "concatenation result type is not string, packed, or an unpacked array "
        "(LRM 11.4.12 / 10.10)");
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
auto LowerReplicationExpr(
    Lowerer& lowerer, WalkFrame frame,
    const slang::ast::ReplicationExpression& rp, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto& unit_lowerer = lowerer.Owner();
  auto type_id = unit_lowerer.InternType(*rp.type, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  const hir::Type& result_ty = unit_lowerer.Unit().types.Get(*type_id);
  if (!result_ty.Is<hir::StringType>() && !result_ty.Is<hir::ScalarBitType>() &&
      !result_ty.Is<hir::PackedArrayType>()) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedExpressionForm,
        "replication result type is neither string nor packed "
        "(LRM 11.4.12.1)");
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
  auto type_id = lowerer.Owner().InternType(*ap.type, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  std::vector<hir::ExprId> element_ids;
  element_ids.reserve(ap.elements().size());
  for (const auto* elem : ap.elements()) {
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
  auto type_id = lowerer.Owner().InternType(*ap.type, span);
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

template <ExprLowerer Lowerer>
auto LowerSimpleAssignmentPattern(
    Lowerer& lowerer, WalkFrame frame,
    const slang::ast::SimpleAssignmentPatternExpression& ap,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  // LRM 10.9.1 positional assignment pattern `'{a, b, ...}`. As an rvalue it
  // lowers from its elements; its LHS-destructuring form is not yet supported.
  if (ap.isLValue) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedAssignmentPatternKind,
        "assignment pattern as LHS destructuring is not yet supported");
  }
  return LowerAssignmentPatternFromElements(lowerer, frame, ap, span);
}

// LRM 10.8 excludes a default correspondence from the assignment-like
// contexts, so the expression is self-determined and arrives at whatever width
// it was written -- an unsized literal at its own. LRM 10.9.1 still has it
// reach every unmatched element as an assignment to that element, so the
// sizing the frontend performed for each keyed value has to be stated here for
// the one value that stands for all the rest.
template <ExprLowerer Lowerer>
auto LowerDefaultKeyValue(
    Lowerer& lowerer, WalkFrame frame,
    const slang::ast::StructuredAssignmentPatternExpression& ap,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  const slang::ast::Type& element_type =
      *ap.type->getCanonicalType().getArrayElementType();
  auto element_type_id = lowerer.Owner().InternType(element_type, span);
  if (!element_type_id) {
    return std::unexpected(std::move(element_type_id.error()));
  }
  // LRM 5.7.1: an unbased unsized literal fills every bit of what it reaches,
  // and here it reaches an element rather than the pattern, so the fill is to
  // the element's width.
  const auto* unbased =
      ap.defaultSetter->as_if<slang::ast::UnbasedUnsizedIntegerLiteral>();
  if (unbased != nullptr && element_type.isIntegral()) {
    return hir::Expr{
        .type = *element_type_id,
        .data =
            hir::PrimaryExpr{
                .data =
                    hir::IntegerLiteral{
                        .value = RepeatBitPattern(
                            LowerSVIntToIntegralConstant(unbased->getValue()),
                            element_type.getBitWidth(),
                            element_type.isFourState(),
                            element_type.isSigned())}},
        .span = span};
  }

  auto lowered = lowerer.LowerExpr(*ap.defaultSetter, frame);
  if (!lowered) return std::unexpected(std::move(lowered.error()));
  hir::Expr value = *std::move(lowered);
  return hir::Expr{
      .type = *element_type_id,
      .data =
          hir::ConversionExpr{
              .kind = hir::ConversionKind::kImplicit,
              .operand = frame.Exprs().Add(std::move(value))},
      .span = span};
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
  const bool keyed_array =
      (target_kind == slang::ast::SymbolKind::FixedSizeUnpackedArrayType ||
       target_kind == slang::ast::SymbolKind::PackedArrayType) &&
      ap.memberSetters.empty() && ap.typeSetters.empty() &&
      (ap.defaultSetter != nullptr || !ap.indexSetters.empty());
  if (keyed_array) {
    auto type_id = lowerer.Owner().InternType(*ap.type, span);
    if (!type_id) return std::unexpected(std::move(type_id.error()));
    std::optional<hir::ExprId> default_id;
    if (ap.defaultSetter != nullptr) {
      auto sized = LowerDefaultKeyValue(lowerer, frame, ap, span);
      if (!sized) return std::unexpected(std::move(sized.error()));
      default_id = frame.Exprs().Add(*std::move(sized));
    }
    std::vector<hir::AssignmentPatternKeyedExpr::Entry> entries;
    entries.reserve(ap.indexSetters.size());
    for (const auto& setter : ap.indexSetters) {
      auto value = lowerer.LowerExpr(*setter.expr, frame);
      if (!value) return std::unexpected(std::move(value.error()));
      entries.push_back(
          {.index = DesignatedIndex(*setter.index),
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
  auto type_id = lowerer.Owner().InternType(*rp.type, span);
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
auto LowerNewArrayExprProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::NewArrayExpression& na, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto type_id = proc.Owner().InternType(*na.type, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  auto size_or = proc.LowerExpr(na.sizeExpr(), frame);
  if (!size_or) return std::unexpected(std::move(size_or.error()));
  const hir::ExprId size_id = frame.Exprs().Add(*std::move(size_or));
  std::optional<hir::ExprId> initializer_id;
  if (na.initExpr() != nullptr) {
    auto init_or = proc.LowerExpr(*na.initExpr(), frame);
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
  auto class_ref = unit_lowerer.ResolveClassRef(cls, span);
  if (!class_ref) return std::unexpected(std::move(class_ref.error()));
  auto type_id = unit_lowerer.InternType(*nc.type, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));
  return hir::Expr{
      .type = *type_id,
      .data =
          hir::ClassNewExpr{
              .class_ref = *std::move(class_ref),
              .arguments = std::move(arguments)},
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
template auto LowerNewClassExpr(
    ProcessLowerer&, WalkFrame, const slang::ast::NewClassExpression&,
    diag::SourceSpan) -> diag::Result<hir::Expr>;
template auto LowerNewClassExpr(
    StructuralScopeLowerer&, WalkFrame, const slang::ast::NewClassExpression&,
    diag::SourceSpan) -> diag::Result<hir::Expr>;

}  // namespace lyra::lowering::ast_to_hir
