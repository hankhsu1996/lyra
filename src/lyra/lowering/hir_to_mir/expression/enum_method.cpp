#include "lyra/lowering/hir_to_mir/expression/enum_method.hpp"

#include <cstdint>
#include <format>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/lowering/hir_to_mir/call_operands.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// An enum member value, materialized as a 2-state-known constant at the enum's
// base packed shape. Enum member values are compile-time constants (LRM 6.19),
// so a 4-state base still carries an all-known (zero) state plane.
auto MemberValueConstant(const mir::PackedArrayType& base, std::int64_t v)
    -> mir::IntegralConstant {
  const auto width = static_cast<std::uint32_t>(base.BitWidth());
  const bool four_state = base.IsFourState();
  const std::size_t word_count = (width + 63U) / 64U;
  mir::IntegralConstant c{
      .value_words = std::vector<std::uint64_t>(word_count, 0U),
      .state_words = four_state ? std::vector<std::uint64_t>(word_count, 0U)
                                : std::vector<std::uint64_t>{},
      .width = width,
      .signedness = base.signedness,
      .state_kind = four_state ? mir::IntegralStateKind::kFourState
                               : mir::IntegralStateKind::kTwoState};
  const std::uint64_t high_word =
      (base.signedness == mir::Signedness::kSigned && v < 0) ? ~std::uint64_t{0}
                                                             : 0U;
  for (std::size_t i = 0; i < word_count; ++i) {
    c.value_words[i] = (i == 0) ? static_cast<std::uint64_t>(v) : high_word;
  }
  const std::uint32_t top_bits = width % 64U;
  if (top_bits != 0U && !c.value_words.empty()) {
    const std::uint64_t mask = (std::uint64_t{1} << top_bits) - 1U;
    c.value_words.back() &= mask;
  }
  return c;
}

// A member-value literal typed as the enum itself (the value's runtime carrier
// is the base packed shape).
auto MemberLiteral(
    mir::Block& body, mir::TypeId enum_ty, const mir::PackedArrayType& base,
    std::int64_t v) -> mir::ExprId {
  return body.exprs.Add(
      mir::Expr{
          .data = mir::IntegerLiteral{.value = MemberValueConstant(base, v)},
          .type = enum_ty});
}

// The enum's default value (LRM Table 6-7) typed as the enum: all-X for a
// 4-state base, 0 for 2-state.
auto DefaultLiteral(
    mir::Block& body, mir::TypeId enum_ty, const mir::PackedArrayType& base)
    -> mir::ExprId {
  return body.exprs.Add(
      mir::Expr{
          .data = mir::IntegerLiteral{.value = DefaultIntegralConstant(base)},
          .type = enum_ty});
}

auto IntLit(mir::Block& body, mir::TypeId int_ty, std::int64_t v)
    -> mir::ExprId {
  return body.exprs.Add(mir::MakeIntLiteral(int_ty, v));
}

auto Binary(
    mir::Block& body, mir::BinaryOp op, mir::ExprId lhs, mir::ExprId rhs,
    mir::TypeId type) -> mir::ExprId {
  return body.exprs.Add(
      mir::Expr{
          .data = mir::BinaryExpr{.op = op, .lhs = lhs, .rhs = rhs},
          .type = type});
}

auto Cond(
    mir::Block& body, mir::ExprId c, mir::ExprId t, mir::ExprId e,
    mir::TypeId type) -> mir::ExprId {
  return body.exprs.Add(
      mir::Expr{
          .data =
              mir::ConditionalExpr{
                  .condition = c, .then_value = t, .else_value = e},
          .type = type});
}

// LRM 11.4.5 `===`: a method-style operator, so it is a `CaseEqual` builtin
// call (a bit-exact match yielding a definite 0/1 even for an X/Z operand),
// never a native binary token.
auto CaseEq(
    mir::Block& body, mir::ExprId lhs, mir::ExprId rhs, mir::TypeId bit_ty)
    -> mir::ExprId {
  return body.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{.target = support::BuiltinFn::kCaseEqual},
                  .arguments = {lhs, rhs}},
          .type = bit_ty});
}

// A `value::String` value from a software literal (LRM 6.16). A bare
// `StringLiteral` renders as a C string; the `value::String` constructor call
// wrapping it is what yields a `String` value in an expression position.
auto StringValue(mir::Block& body, mir::TypeId str_ty, std::string text)
    -> mir::ExprId {
  const mir::ExprId raw = body.exprs.Add(
      mir::Expr{
          .data = mir::StringLiteral{.value = std::move(text)},
          .type = str_ty});
  return body.exprs.Add(
      mir::Expr{
          .data = mir::CallExpr{.callee = mir::Construct{}, .arguments = {raw}},
          .type = str_ty});
}

// LRM 6.19.5.5 `name`: a static function `(value) -> String` whose body is a
// case-equality chain over the member table, returning "" for a non-member (or
// X/Z) value.
auto SynthesizeEnumNameCallable(
    mir::CompilationUnit& unit, mir::Class& owner, mir::TypeId enum_ty,
    const mir::PackedArrayType& base,
    const std::vector<mir::EnumMember>& members) -> mir::CallableId {
  const mir::TypeId str_ty = unit.builtins.string;
  const mir::TypeId bit_ty = unit.builtins.bit1;

  mir::CallableCode code = mir::CallableCode::Defined();
  const mir::LocalId value_id =
      code.locals.Add(mir::LocalDecl{.name = "value", .type = enum_ty});
  code.params = {value_id};
  code.result_type = str_ty;

  mir::Block& body = code.Body();
  mir::ExprId acc = StringValue(body, str_ty, std::string{});
  for (std::size_t i = members.size(); i-- > 0;) {
    const mir::ExprId val_ref =
        body.exprs.Add(mir::MakeLocalRefExpr(value_id, enum_ty));
    const mir::ExprId member_lit =
        MemberLiteral(body, enum_ty, base, members[i].value);
    const mir::ExprId cond = CaseEq(body, val_ref, member_lit, bit_ty);
    const mir::ExprId name_lit = StringValue(body, str_ty, members[i].name);
    acc = Cond(body, cond, name_lit, acc, str_ty);
  }
  body.AppendStmt(mir::ReturnStmt{.value = acc});

  return owner.callables.Add(
      mir::CallableDecl{
          .name = std::format("__enum_name_{}", enum_ty.value),
          .code = std::move(code),
          .foreign = std::nullopt,
          .virtual_dispatch = std::nullopt});
}

// LRM 6.19.5.3/4 `next` / `prev`: a static function `(value, step) -> enum`
// that steps `step` members from `value` (negative `step` is `prev`), wrapping
// over the member order; a non-member (or X/Z) value returns the enum default
// (Table 6-7). `next(k)` calls this with `+k`, `prev(k)` with `-k`.
auto SynthesizeEnumStepCallable(
    mir::CompilationUnit& unit, mir::Class& owner, mir::TypeId enum_ty,
    const mir::PackedArrayType& base,
    const std::vector<mir::EnumMember>& members) -> mir::CallableId {
  const mir::TypeId int_ty = unit.builtins.int_type;
  const mir::TypeId bit_ty = unit.builtins.bit1;
  const auto n = static_cast<std::int64_t>(members.size());

  mir::CallableCode code = mir::CallableCode::Defined();
  const mir::LocalId value_id =
      code.locals.Add(mir::LocalDecl{.name = "value", .type = enum_ty});
  const mir::LocalId step_id =
      code.locals.Add(mir::LocalDecl{.name = "step", .type = int_ty});
  code.params = {value_id, step_id};
  code.result_type = enum_ty;
  const mir::LocalId idx_id =
      code.locals.Add(mir::LocalDecl{.name = "idx", .type = int_ty});
  const mir::LocalId newidx_id =
      code.locals.Add(mir::LocalDecl{.name = "newidx", .type = int_ty});

  mir::Block& body = code.Body();

  // idx = index-of(value): (value === v_i) ? i : ... : -1
  mir::ExprId idx_chain = IntLit(body, int_ty, -1);
  for (std::size_t i = members.size(); i-- > 0;) {
    const mir::ExprId val_ref =
        body.exprs.Add(mir::MakeLocalRefExpr(value_id, enum_ty));
    const mir::ExprId member_lit =
        MemberLiteral(body, enum_ty, base, members[i].value);
    const mir::ExprId cond = CaseEq(body, val_ref, member_lit, bit_ty);
    idx_chain = Cond(
        body, cond, IntLit(body, int_ty, static_cast<std::int64_t>(i)),
        idx_chain, int_ty);
  }
  body.AppendStmt(mir::LocalDeclStmt{.target = idx_id, .init = idx_chain});

  // newidx = ((idx + step) % n + n) % n
  const mir::ExprId idx_ref =
      body.exprs.Add(mir::MakeLocalRefExpr(idx_id, int_ty));
  const mir::ExprId step_ref =
      body.exprs.Add(mir::MakeLocalRefExpr(step_id, int_ty));
  const mir::ExprId sum =
      Binary(body, mir::BinaryOp::kAdd, idx_ref, step_ref, int_ty);
  const mir::ExprId mod1 =
      Binary(body, mir::BinaryOp::kMod, sum, IntLit(body, int_ty, n), int_ty);
  const mir::ExprId biased =
      Binary(body, mir::BinaryOp::kAdd, mod1, IntLit(body, int_ty, n), int_ty);
  const mir::ExprId newidx = Binary(
      body, mir::BinaryOp::kMod, biased, IntLit(body, int_ty, n), int_ty);
  body.AppendStmt(mir::LocalDeclStmt{.target = newidx_id, .init = newidx});

  // member-at(newidx): (newidx == i) ? v_i : ... : default
  mir::ExprId member_chain = DefaultLiteral(body, enum_ty, base);
  for (std::size_t i = members.size(); i-- > 0;) {
    const mir::ExprId newidx_ref =
        body.exprs.Add(mir::MakeLocalRefExpr(newidx_id, int_ty));
    const mir::ExprId cond = Binary(
        body, mir::BinaryOp::kEquality, newidx_ref,
        IntLit(body, int_ty, static_cast<std::int64_t>(i)), bit_ty);
    member_chain = Cond(
        body, cond, MemberLiteral(body, enum_ty, base, members[i].value),
        member_chain, enum_ty);
  }

  // return (idx < 0) ? default : member-at(newidx)
  const mir::ExprId idx_ref2 =
      body.exprs.Add(mir::MakeLocalRefExpr(idx_id, int_ty));
  const mir::ExprId not_member = Binary(
      body, mir::BinaryOp::kLessThan, idx_ref2, IntLit(body, int_ty, 0),
      bit_ty);
  const mir::ExprId default_val = DefaultLiteral(body, enum_ty, base);
  const mir::ExprId result =
      Cond(body, not_member, default_val, member_chain, enum_ty);
  body.AppendStmt(mir::ReturnStmt{.value = result});

  return owner.callables.Add(
      mir::CallableDecl{
          .name = std::format("__enum_step_{}", enum_ty.value),
          .code = std::move(code),
          .foreign = std::nullopt,
          .virtual_dispatch = std::nullopt});
}

// The per-enum `name` callable, synthesized on first use and cached so every
// call site in the unit shares one callable.
auto ResolveEnumNameHelper(
    UnitLowerer& unit_lowerer, WalkFrame frame, mir::TypeId enum_tid,
    const mir::PackedArrayType& base,
    const std::vector<mir::EnumMember>& members) -> mir::CallableTarget {
  auto& cache = unit_lowerer.EnumNameHelpers();
  if (const auto it = cache.find(enum_tid.value); it != cache.end()) {
    return it->second;
  }
  const mir::CallableId slot = SynthesizeEnumNameCallable(
      unit_lowerer.Unit(), *frame.current_class, enum_tid, base, members);
  const mir::CallableTarget target{
      .owner = frame.current_class_id, .slot = slot};
  cache.emplace(enum_tid.value, target);
  return target;
}

// The per-enum step callable (shared by `next` and `prev`), synthesized on
// first use and cached the same way.
auto ResolveEnumStepHelper(
    UnitLowerer& unit_lowerer, WalkFrame frame, mir::TypeId enum_tid,
    const mir::PackedArrayType& base,
    const std::vector<mir::EnumMember>& members) -> mir::CallableTarget {
  auto& cache = unit_lowerer.EnumStepHelpers();
  if (const auto it = cache.find(enum_tid.value); it != cache.end()) {
    return it->second;
  }
  const mir::CallableId slot = SynthesizeEnumStepCallable(
      unit_lowerer.Unit(), *frame.current_class, enum_tid, base, members);
  const mir::CallableTarget target{
      .owner = frame.current_class_id, .slot = slot};
  cache.emplace(enum_tid.value, target);
  return target;
}

}  // namespace

template <ExprLowerer Lowerer>
auto LowerEnumConstantMethod(
    Lowerer& lowerer, const hir::CallExpr& c, const hir::BuiltinMethodRef& b,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  const auto& unit_lowerer = lowerer.Owner();
  const auto& unit = unit_lowerer.Unit();
  const auto& hir_exprs = lowerer.HirExprs();
  if (c.arguments.empty() || !c.arguments.front().has_value()) {
    throw InternalError(
        "LowerEnumConstantMethod: missing enum type-bearer argument");
  }
  const mir::TypeId enum_tid =
      unit_lowerer.TranslateType(hir_exprs.Get(*c.arguments.front()).type);
  const auto& enum_ty = std::get<mir::EnumType>(unit.types.Get(enum_tid).data);
  if (enum_ty.members.empty()) {
    throw InternalError("LowerEnumConstantMethod: enum has no members");
  }
  switch (b.method) {
    case support::BuiltinFn::kEnumNum:
      return mir::MakeIntLiteral(
          unit.builtins.int_type,
          static_cast<std::int64_t>(enum_ty.members.size()));
    case support::BuiltinFn::kEnumFirst:
      return mir::Expr{
          .data =
              mir::IntegerLiteral{
                  .value = MemberValueConstant(
                      enum_ty.base, enum_ty.members.front().value)},
          .type = result_type};
    case support::BuiltinFn::kEnumLast:
      return mir::Expr{
          .data =
              mir::IntegerLiteral{
                  .value = MemberValueConstant(
                      enum_ty.base, enum_ty.members.back().value)},
          .type = result_type};
    default:
      throw InternalError(
          "LowerEnumConstantMethod: not a constant enum method");
  }
}

template <ExprLowerer Lowerer>
auto LowerEnumMethodCall(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& c,
    const hir::BuiltinMethodRef& b, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  auto& unit_lowerer = lowerer.Owner();
  auto& unit = unit_lowerer.Unit();
  const auto& hir_exprs = lowerer.HirExprs();
  auto& block = *frame.current_block;

  if (c.arguments.empty() || !c.arguments.front().has_value()) {
    throw InternalError("LowerEnumMethodCall: missing enum receiver argument");
  }
  const hir::Expr& receiver_hir = hir_exprs.Get(*c.arguments.front());
  const mir::TypeId enum_tid = unit_lowerer.TranslateType(receiver_hir.type);

  // The step callable homes on a class the intra-unit call can name; a package
  // namespace has none, so those contexts are not yet supported.
  if (frame.current_class == nullptr) {
    return diag::Fail(
        receiver_hir.span, diag::DiagCode::kUnsupportedExpressionForm,
        "enum name / next / prev in a package context is not yet supported");
  }

  // Copy the shape and member table before lowering the receiver below, which
  // may intern new types and invalidate a reference into the type pool.
  const mir::PackedArrayType base =
      std::get<mir::EnumType>(unit.types.Get(enum_tid).data).base;
  const std::vector<mir::EnumMember> members =
      std::get<mir::EnumType>(unit.types.Get(enum_tid).data).members;
  if (members.empty()) {
    throw InternalError("LowerEnumMethodCall: enum has no members");
  }

  // The lowered receiver value is the first call argument.
  auto recv_or = lowerer.LowerExpr(receiver_hir, frame);
  if (!recv_or) return std::unexpected(std::move(recv_or.error()));
  const mir::ExprId value_id = block.exprs.Add(*std::move(recv_or));

  if (b.method == support::BuiltinFn::kEnumName) {
    const mir::CallableTarget target =
        ResolveEnumNameHelper(unit_lowerer, frame, enum_tid, base, members);
    return mir::Expr{
        .data =
            mir::CallExpr{
                .callee = mir::Direct{.target = target},
                .arguments = {value_id}},
        .type = result_type};
  }

  // next / prev share one step callable; prev negates the step.
  const mir::CallableTarget target =
      ResolveEnumStepHelper(unit_lowerer, frame, enum_tid, base, members);

  const mir::TypeId int_ty = unit.builtins.int_type;
  const bool is_prev = b.method == support::BuiltinFn::kEnumPrev;
  mir::ExprId step_id{};
  // `next` / `prev` take an optional step count (LRM 6.19.5).
  if (const std::optional<hir::ExprId> step = OptionalOperand(c, 1)) {
    auto step_or = lowerer.LowerExpr(hir_exprs.Get(*step), frame);
    if (!step_or) return std::unexpected(std::move(step_or.error()));
    const mir::ExprId raw = block.exprs.Add(*std::move(step_or));
    if (is_prev) {
      const mir::ExprId zero = block.exprs.Add(mir::MakeIntLiteral(int_ty, 0));
      step_id = block.exprs.Add(
          mir::Expr{
              .data =
                  mir::BinaryExpr{
                      .op = mir::BinaryOp::kSub, .lhs = zero, .rhs = raw},
              .type = int_ty});
    } else {
      step_id = raw;
    }
  } else {
    step_id = block.exprs.Add(mir::MakeIntLiteral(int_ty, is_prev ? -1 : 1));
  }

  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = target},
              .arguments = {value_id, step_id}},
      .type = result_type};
}

template auto LowerEnumConstantMethod(
    ProcessLowerer&, const hir::CallExpr&, const hir::BuiltinMethodRef&,
    mir::TypeId) -> diag::Result<mir::Expr>;
template auto LowerEnumConstantMethod(
    const StructuralScopeLowerer&, const hir::CallExpr&,
    const hir::BuiltinMethodRef&, mir::TypeId) -> diag::Result<mir::Expr>;
template auto LowerEnumMethodCall(
    ProcessLowerer&, WalkFrame, const hir::CallExpr&,
    const hir::BuiltinMethodRef&, mir::TypeId) -> diag::Result<mir::Expr>;
template auto LowerEnumMethodCall(
    const StructuralScopeLowerer&, WalkFrame, const hir::CallExpr&,
    const hir::BuiltinMethodRef&, mir::TypeId) -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
