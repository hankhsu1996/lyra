#include "lyra/lowering/ast_to_hir/expression/calls.hpp"

#include <cstddef>
#include <expected>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/SystemSubroutine.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/CallExpression.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/lowering/ast_to_hir/expression/slang_atoms.hpp"
#include "lyra/lowering/ast_to_hir/module_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// Maps a frontend ReturnConvention to the builtin HIR TypeId that represents
// it. Local to the calls subsystem (system subroutines are the only consumer).
auto MakeReturnConventionType(
    const hir::BuiltinHirTypes& builtins, support::ReturnConvention conv)
    -> hir::TypeId {
  switch (conv) {
    case support::ReturnConvention::kVoid:
      return builtins.void_type;
    case support::ReturnConvention::kInt32:
      return builtins.int32;
    case support::ReturnConvention::kInteger:
      return builtins.integer;
    case support::ReturnConvention::kString:
      return builtins.string;
    case support::ReturnConvention::kTime64:
      return builtins.time;
    case support::ReturnConvention::kRealTime:
      return builtins.realtime;
  }
  throw InternalError("MakeReturnConventionType: unknown ReturnConvention");
}

}  // namespace

auto LowerCallExprProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::CallExpression& call, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto& module = proc.Module();
  std::vector<std::optional<hir::ExprId>> arg_ids;
  arg_ids.reserve(call.arguments().size());
  std::optional<hir::TypeId> receiver_type;
  for (std::size_t i = 0; i < call.arguments().size(); ++i) {
    // LRM 13.5: slang models an `output` / `inout` actual as an
    // AssignmentExpression whose right side is an EmptyArgument placeholder;
    // the actual lvalue is the left side. HIR carries just that lvalue -- the
    // copy-in / copy-out is synthesized at HIR-to-MIR from the formal's
    // direction.
    const slang::ast::Expression* arg = call.arguments()[i];
    if (arg->kind == slang::ast::ExpressionKind::Assignment) {
      const auto& as = arg->as<slang::ast::AssignmentExpression>();
      if (as.right().kind == slang::ast::ExpressionKind::EmptyArgument) {
        arg = &as.left();
      }
    }
    // LRM 21.3.4.4 form 2d: a standalone EmptyArgument marks a positional
    // elision (`$fread(mem, fd, , count)`). Surface as `std::nullopt` so the
    // per-subroutine HIR-to-MIR handler can decide whether elision is valid
    // at this position; positions stay aligned with slang's arg list.
    if (arg->kind == slang::ast::ExpressionKind::EmptyArgument) {
      arg_ids.emplace_back(std::nullopt);
      continue;
    }
    auto arg_or = proc.LowerExpr(*arg, frame);
    if (!arg_or) return std::unexpected(std::move(arg_or.error()));
    if (i == 0) {
      receiver_type = arg_or->type;
    }
    arg_ids.emplace_back(
        frame.current_procedural_body->AddExpr(*std::move(arg_or)));
  }

  if (call.isSystemCall()) {
    const auto& info =
        std::get<slang::ast::CallExpression::SystemCallInfo>(call.subroutine);
    const std::string_view name = info.subroutine->name;

    if (receiver_type.has_value() &&
        module.Unit().GetType(*receiver_type).IsEnum()) {
      if (auto kind = LowerEnumMethodName(name); kind.has_value()) {
        // `next` / `prev` have an optional `int unsigned step = 1` (LRM
        // 6.19.5.3/4). When the user omits the step, the lowering hands the
        // backend a single-argument call and lets the backend's intrinsic
        // mechanism supply the default (C++ default-argument; LLVM constant
        // 1; etc.). No synthesized literal is injected at the HIR boundary.
        auto type_id = module.InternType(*call.type, span);
        if (!type_id) return std::unexpected(std::move(type_id.error()));
        return hir::Expr{
            .type = *type_id,
            .data =
                hir::CallExpr{
                    .callee = hir::BuiltinMethodRef{.method = *kind},
                    .arguments = std::move(arg_ids),
                },
            .span = span,
        };
      }
    }

    if (receiver_type.has_value() &&
        module.Unit().GetType(*receiver_type).Kind() ==
            hir::TypeKind::kString) {
      if (auto kind = LowerStringMethodName(name); kind.has_value()) {
        // LRM 6.16.1 through 6.16.15 -- string intrinsic methods. The
        // receiver is arguments[0]; remaining arguments are the SV method
        // parameters (e.g. substr's `i, j`).
        auto type_id = module.InternType(*call.type, span);
        if (!type_id) return std::unexpected(std::move(type_id.error()));
        return hir::Expr{
            .type = *type_id,
            .data =
                hir::CallExpr{
                    .callee = hir::BuiltinMethodRef{.method = *kind},
                    .arguments = std::move(arg_ids),
                },
            .span = span,
        };
      }
    }

    if (receiver_type.has_value() &&
        std::holds_alternative<hir::EventType>(
            module.Unit().GetType(*receiver_type).data) &&
        name == "triggered") {
      // LRM 15.5.3: `e.triggered` returns true for the duration of the time
      // slot in which the event was last triggered. Result type is bit (1'b0
      // / 1'b1) -- slang already typed the expression; we just route the
      // call through the named-event method.
      auto type_id = module.InternType(*call.type, span);
      if (!type_id) return std::unexpected(std::move(type_id.error()));
      return hir::Expr{
          .type = *type_id,
          .data =
              hir::CallExpr{
                  .callee =
                      hir::BuiltinMethodRef{
                          .method = hir::EventMethodKind::kTriggered,
                      },
                  .arguments = std::move(arg_ids),
              },
          .span = span,
      };
    }

    if (receiver_type.has_value() &&
        std::holds_alternative<hir::DynamicArrayType>(
            module.Unit().GetType(*receiver_type).data)) {
      if (auto kind = LowerArrayMethodName(name); kind.has_value()) {
        // LRM 7.5.2 / 7.5.3 dynamic-array methods plus LRM 7.12.2 / 7.12.3
        // family. The no-`with` form takes only the receiver. The `with`
        // form (LRM 7.12.4) binds an iterator and a body expression which
        // HIR carries as the optional `WithClause` -- HIR -> MIR turns it
        // into a closure argument.
        auto type_id = module.InternType(*call.type, span);
        if (!type_id) return std::unexpected(std::move(type_id.error()));
        std::optional<hir::WithClause> with_clause;
        if (std::holds_alternative<
                slang::ast::CallExpression::IteratorCallInfo>(info.extraInfo)) {
          const auto& iter_info =
              std::get<slang::ast::CallExpression::IteratorCallInfo>(
                  info.extraInfo);
          const auto& iter_var =
              iter_info.iterVar->as<slang::ast::VariableSymbol>();
          auto iter_type = module.InternType(iter_var.getType(), span);
          if (!iter_type) {
            return std::unexpected(std::move(iter_type.error()));
          }
          const hir::ProceduralVarId iterator_id = proc.AddProceduralVar(
              *frame.current_procedural_body, iter_var, *iter_type);
          auto body_or = proc.LowerExpr(*iter_info.iterExpr, frame);
          if (!body_or) return std::unexpected(std::move(body_or.error()));
          const auto body_expr_id =
              frame.current_procedural_body->AddExpr(*std::move(body_or));
          with_clause =
              hir::WithClause{.iterator = iterator_id, .expr = body_expr_id};
        }
        return hir::Expr{
            .type = *type_id,
            .data =
                hir::CallExpr{
                    .callee = hir::BuiltinMethodRef{.method = *kind},
                    .arguments = std::move(arg_ids),
                    .with_clause = std::move(with_clause),
                },
            .span = span,
        };
      }
    }

    // LRM 7.12.4 `item.index`: slang resolves to a SystemSubroutine with
    // `KnownSystemName::Index`. Only legal inside an array-method `with`
    // body; the call lowers to a `BuiltinMethodRef{IteratorMethodKind::
    // kIndex}` and HIR -> MIR rewrites it to a `ProceduralVarRef` on the
    // closure's `index` parameter binding.
    if (info.subroutine != nullptr &&
        info.subroutine->knownNameId ==
            slang::parsing::KnownSystemName::Index) {
      auto type_id = module.InternType(*call.type, span);
      if (!type_id) return std::unexpected(std::move(type_id.error()));
      return hir::Expr{
          .type = *type_id,
          .data =
              hir::CallExpr{
                  .callee =
                      hir::BuiltinMethodRef{
                          .method = hir::IteratorMethodKind::kIndex},
                  .arguments = std::move(arg_ids),
              },
          .span = span,
      };
    }

    const auto* desc = support::FindSystemSubroutine(name);
    if (desc == nullptr) {
      throw InternalError(
          std::string{"AST->HIR call: unresolved system subroutine '"} +
          std::string{name} + "' after slang resolution");
    }
    const auto frontend_kind = FromSlangSubroutineKind(info.subroutine->kind);
    if (desc->kind != frontend_kind) {
      throw InternalError(
          std::string{"AST->HIR call: registry/frontend kind mismatch for '"} +
          std::string{name} + "'");
    }
    if (!desc->arg_policy.Accepts(arg_ids.size())) {
      throw InternalError(
          std::string{
              "AST->HIR call: arg count outside descriptor policy for '"} +
          std::string{name} + "'");
    }

    const auto result_type =
        MakeReturnConventionType(module.Unit().builtins, desc->result_conv);
    return hir::Expr{
        .type = result_type,
        .data =
            hir::CallExpr{
                .callee = hir::SystemSubroutineRef{.id = desc->id},
                .arguments = std::move(arg_ids),
            },
        .span = span,
    };
  }

  const auto* sym =
      std::get<const slang::ast::SubroutineSymbol*>(call.subroutine);
  if (sym == nullptr) {
    throw InternalError(
        "AST->HIR call: user call missing resolved SubroutineSymbol");
  }
  const auto binding = module.LookupSubroutineBinding(*sym);
  if (!binding.has_value()) {
    throw InternalError(
        "AST->HIR call: resolved user subroutine has no registered HIR "
        "binding");
  }
  const auto hops = frame.HopsTo(binding->owner_frame);
  if (!hops.has_value()) {
    throw InternalError(
        "AST->HIR call: user subroutine owner frame is not on the current "
        "scope stack");
  }
  auto result_type = module.InternType(*call.type, span);
  if (!result_type) return std::unexpected(std::move(result_type.error()));
  return hir::Expr{
      .type = *result_type,
      .data =
          hir::CallExpr{
              .callee =
                  hir::StructuralSubroutineRef{
                      .hops = *hops, .subroutine = binding->subroutine_id},
              .arguments = std::move(arg_ids),
          },
      .span = span,
  };
}

}  // namespace lyra::lowering::ast_to_hir
