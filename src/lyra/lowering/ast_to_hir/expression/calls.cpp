#include "lyra/lowering/ast_to_hir/expression/calls.hpp"

#include <cstddef>
#include <expected>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include <slang/ast/Compilation.h>
#include <slang/ast/Expression.h>
#include <slang/ast/Scope.h>
#include <slang/ast/SystemSubroutine.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/CallExpression.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/symbols/ClassSymbols.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>
#include <slang/numeric/SVInt.h>
#include <slang/syntax/AllSyntax.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/lowering/ast_to_hir/expression/expr_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/expression/query.hpp"
#include "lyra/lowering/ast_to_hir/expression/slang_atoms.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// True when the call's source syntax carries LRM 8.15 super qualification
// (`super.foo()` or `this.super.foo()`). Slang resolves `super.foo` to the
// base's callee symbol but records no semantic flag, so the qualifier
// survives only on the source syntax -- a scoped name for the plain form or
// a member access for `this.super.foo()`. Two callee syntaxes reach here
// depending on whether the source used parentheses; both share the same
// shape: the callee has a `.left` whose kind is `SuperHandle`.
auto CallReceivesSuper(const slang::ast::CallExpression& call) -> bool {
  const auto* syn = call.syntax;
  if (syn == nullptr) return false;
  const slang::syntax::SyntaxNode* callee = syn;
  if (syn->kind == slang::syntax::SyntaxKind::InvocationExpression) {
    callee = syn->as<slang::syntax::InvocationExpressionSyntax>().left;
  }
  if (callee == nullptr) return false;
  const auto is_super =
      [](const slang::syntax::SyntaxNode& left_of_dot) -> bool {
    return left_of_dot.kind == slang::syntax::SyntaxKind::SuperHandle;
  };
  if (callee->kind == slang::syntax::SyntaxKind::ScopedName) {
    return is_super(*callee->as<slang::syntax::ScopedNameSyntax>().left);
  }
  if (callee->kind == slang::syntax::SyntaxKind::MemberAccessExpression) {
    return is_super(
        *callee->as<slang::syntax::MemberAccessExpressionSyntax>().left);
  }
  return false;
}

// True when the call reaches an instance method through its enclosing class
// -- either the source supplied a handle (`h.foo()`) or slang resolved an
// unqualified call to a class-scope subroutine, whose enclosing class is
// the receiver's own type (LRM 8.6 implicit `this`, including LRM 8.15
// `super.foo()`).
auto CallReachesInstanceMethod(
    const slang::ast::CallExpression& call,
    const slang::ast::SubroutineSymbol& sym) -> bool {
  if (call.thisClass() != nullptr) return true;
  const auto* parent = sym.getParentScope();
  return parent != nullptr &&
         parent->asSymbol().kind == slang::ast::SymbolKind::ClassType;
}

// Classifies the three LRM-defined receiver forms of an instance-method
// call into a MethodReceiver arm. Super takes precedence over an
// accompanying `thisClass` because `this.super.foo()` synthesizes a
// `thisClass` for the outer `this` while the super qualifier still applies
// at the syntactic level.
template <ExprLowerer Lowerer>
auto ClassifyMethodReceiver(
    Lowerer& lowerer, WalkFrame frame, const slang::ast::CallExpression& call)
    -> diag::Result<hir::MethodReceiver> {
  if (CallReceivesSuper(call)) return hir::SuperReceiver{};
  if (const auto* this_class = call.thisClass(); this_class != nullptr) {
    auto receiver_or = lowerer.LowerExpr(*this_class, frame);
    if (!receiver_or) return std::unexpected(std::move(receiver_or.error()));
    return hir::HandleReceiver{
        .expr = frame.Exprs().Add(*std::move(receiver_or))};
  }
  return hir::ImplicitSelfReceiver{};
}

// Maps a frontend ReturnConvention to the builtin HIR TypeId that represents
// it. Local to the calls subsystem (system subroutines are the only consumer).
auto MakeReturnConventionType(
    const hir::BuiltinHirTypes& builtins, support::ReturnConvention conv)
    -> hir::TypeId {
  switch (conv) {
    case support::ReturnConvention::kVoid:
      return builtins.void_type;
    case support::ReturnConvention::kInt32:
      return builtins.int_type;
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

// A method of an imported runtime-library class (LRM 9.7 `process`) is
// recognized by the declaring class being a member of the built-in `std`
// package, exactly as the handle type is; the runtime provides the body, so the
// call routes to the library symbol rather than a lowered user method.
auto DetectImportedRuntimeMethod(const slang::ast::SubroutineSymbol& method)
    -> std::optional<support::ImportedRuntimeMethod> {
  const slang::ast::Scope* scope = method.getParentScope();
  if (scope == nullptr) {
    return std::nullopt;
  }
  const slang::ast::Symbol& owner = scope->asSymbol();
  if (owner.kind != slang::ast::SymbolKind::ClassType) {
    return std::nullopt;
  }
  const auto& cls = owner.as<slang::ast::ClassType>();
  const slang::ast::Scope* class_scope = cls.getParentScope();
  if (class_scope == nullptr ||
      class_scope != static_cast<const slang::ast::Scope*>(
                         &class_scope->getCompilation().getStdPackage()) ||
      cls.name != "process") {
    return std::nullopt;
  }
  if (method.name == "self") {
    return support::ImportedRuntimeMethod::kProcessSelf;
  }
  if (method.name == "status") {
    return support::ImportedRuntimeMethod::kProcessStatus;
  }
  if (method.name == "kill") {
    return support::ImportedRuntimeMethod::kProcessKill;
  }
  if (method.name == "await") {
    return support::ImportedRuntimeMethod::kProcessAwait;
  }
  if (method.name == "suspend") {
    return support::ImportedRuntimeMethod::kProcessSuspend;
  }
  if (method.name == "resume") {
    return support::ImportedRuntimeMethod::kProcessResume;
  }
  return std::nullopt;
}

// The package a subroutine is declared directly in (LRM 26.2), or null when the
// subroutine belongs to this compilation unit. A package subroutine's target
// lives across the unit boundary and is reached by name, not through an
// enclosing-scope binding of this unit.
auto EnclosingPackage(const slang::ast::SubroutineSymbol& sym)
    -> const slang::ast::PackageSymbol* {
  const slang::ast::Scope* scope = sym.getParentScope();
  if (scope == nullptr) {
    return nullptr;
  }
  const slang::ast::Symbol& owner = scope->asSymbol();
  if (owner.kind != slang::ast::SymbolKind::Package) {
    return nullptr;
  }
  return &owner.as<slang::ast::PackageSymbol>();
}

}  // namespace

template <ExprLowerer Lowerer>
auto LowerCallExpr(
    Lowerer& lowerer, WalkFrame frame, const slang::ast::CallExpression& call,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  auto& unit_lowerer = lowerer.Owner();

  // The LRM 20.6 / 20.7 queries are resolved before the argument loop: their
  // operand is never evaluated and may be a data type, which has no value form
  // to lower.
  auto query = LowerQueryExpr(lowerer, frame, call, span);
  if (!query) return std::unexpected(std::move(query.error()));
  if (query->has_value()) return *std::move(*query);

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
    auto arg_or = lowerer.LowerExpr(*arg, frame);
    if (!arg_or) return std::unexpected(std::move(arg_or.error()));
    if (i == 0) {
      receiver_type = arg_or->type;
    }
    arg_ids.emplace_back(frame.Exprs().Add(*std::move(arg_or)));
  }

  if (call.isSystemCall()) {
    const auto& info =
        std::get<slang::ast::CallExpression::SystemCallInfo>(call.subroutine);
    const std::string_view name = info.subroutine->name;

    if (receiver_type.has_value() &&
        unit_lowerer.Unit().types.Get(*receiver_type).IsEnum()) {
      if (auto kind = LowerEnumMethodName(name); kind.has_value()) {
        // `next` / `prev` have an optional `int unsigned step = 1` (LRM
        // 6.19.5.3/4). When the user omits the step, the lowering hands the
        // backend a single-argument call and lets the backend's intrinsic
        // mechanism supply the default (C++ default-argument; LLVM constant
        // 1; etc.). No synthesized literal is injected at the HIR boundary.
        auto type_id = unit_lowerer.InternType(*call.type, span);
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
        unit_lowerer.Unit().types.Get(*receiver_type).Kind() ==
            hir::TypeKind::kString) {
      if (auto kind = LowerStringMethodName(name); kind.has_value()) {
        // LRM 6.16.1 through 6.16.15 -- string intrinsic methods. The
        // receiver is arguments[0]; remaining arguments are the SV method
        // parameters (e.g. substr's `i, j`).
        auto type_id = unit_lowerer.InternType(*call.type, span);
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
            unit_lowerer.Unit().types.Get(*receiver_type).data) &&
        name == "triggered") {
      // LRM 15.5.3: `e.triggered` returns true for the duration of the time
      // slot in which the event was last triggered. Result type is bit (1'b0
      // / 1'b1) -- slang already typed the expression; we just route the
      // call through the named-event method.
      auto type_id = unit_lowerer.InternType(*call.type, span);
      if (!type_id) return std::unexpected(std::move(type_id.error()));
      return hir::Expr{
          .type = *type_id,
          .data =
              hir::CallExpr{
                  .callee =
                      hir::BuiltinMethodRef{
                          .method = support::BuiltinFn::kTriggered,
                      },
                  .arguments = std::move(arg_ids),
              },
          .span = span,
      };
    }

    if (receiver_type.has_value() &&
        std::holds_alternative<hir::QueueType>(
            unit_lowerer.Unit().types.Get(*receiver_type).data)) {
      // LRM 7.10.2 queue-native methods. The receiver is arguments[0]; any
      // method parameters (insert's index and item, push's item) follow as the
      // remaining arguments. These methods take no `with` clause and are tried
      // before the array-manipulation family so `size` / `delete` resolve to
      // the queue-native form rather than the LRM 7.12 one.
      if (auto kind = LowerQueueMethodName(name); kind.has_value()) {
        auto type_id = unit_lowerer.InternType(*call.type, span);
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

    // LRM 7.12 array-manipulation family, defined for any unpacked array: a
    // fixed-size unpacked array (LRM 7.12.1 / 7.12.2 operate on "any unpacked
    // array"), a dynamic array, a queue (LRM 7.10.1 gives it the same
    // operations as an unpacked array), and an associative array (LRM 7.12.1 /
    // 7.12.3 / 7.12.5 reduction / locator / map; the ordering family is
    // rejected on it by slang). For dynamic array and queue the LRM 7.5.2 /
    // 7.5.3 / 7.10.2 `size` / `delete` are resolved earlier as native methods,
    // and the associative-array LRM 7.9 methods (`exists` / traversal / `num`)
    // resolve in the block below; only the 7.12 names reach this dispatch. The
    // no-`with` form takes only the receiver; the `with` form (LRM 7.12.4)
    // binds an iterator and a body expression carried as the optional
    // `WithClause`, which HIR -> MIR turns into a closure argument.
    if (receiver_type.has_value() &&
        (std::holds_alternative<hir::UnpackedArrayType>(
             unit_lowerer.Unit().types.Get(*receiver_type).data) ||
         std::holds_alternative<hir::DynamicArrayType>(
             unit_lowerer.Unit().types.Get(*receiver_type).data) ||
         std::holds_alternative<hir::QueueType>(
             unit_lowerer.Unit().types.Get(*receiver_type).data) ||
         std::holds_alternative<hir::AssociativeArrayType>(
             unit_lowerer.Unit().types.Get(*receiver_type).data))) {
      if (auto kind = LowerArrayMethodName(name); kind.has_value()) {
        auto type_id = unit_lowerer.InternType(*call.type, span);
        if (!type_id) return std::unexpected(std::move(type_id.error()));
        std::optional<hir::WithClause> with_clause;
        if (std::holds_alternative<
                slang::ast::CallExpression::IteratorCallInfo>(info.extraInfo)) {
          const auto& iter_info =
              std::get<slang::ast::CallExpression::IteratorCallInfo>(
                  info.extraInfo);
          const auto& iter_var =
              iter_info.iterVar->as<slang::ast::VariableSymbol>();
          // The clause's element and index references resolve to this id while
          // the body is lowered; marking the iterator by identity on the frame
          // distinguishes it from a foreach loop variable (also a slang
          // Iterator symbol) and lets a clause nested in the body name this
          // outer one.
          const hir::WithClauseId clause_id = unit_lowerer.NextWithClauseId();
          auto body_or = lowerer.LowerExpr(
              *iter_info.iterExpr,
              frame.WithIterationClause(iter_var, clause_id));
          if (!body_or) return std::unexpected(std::move(body_or.error()));
          const auto body_expr_id = frame.Exprs().Add(*std::move(body_or));
          with_clause = hir::WithClause{
              .id = clause_id,
              .element_name = std::string{iter_var.name},
              .expr = body_expr_id};
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

    if (receiver_type.has_value() &&
        std::holds_alternative<hir::AssociativeArrayType>(
            unit_lowerer.Unit().types.Get(*receiver_type).data)) {
      if (auto kind = LowerAssociativeMethodName(name); kind.has_value()) {
        // LRM 7.9 associative-array methods. The receiver is arguments[0]; the
        // key (exists / delete-by-index) follows as the next argument. The
        // no-argument `delete` clears the array.
        auto type_id = unit_lowerer.InternType(*call.type, span);
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

    // LRM 20.9 `$isunknown`: 1 if any bit of the operand is X or Z. A
    // type-agnostic value query -- every value type exposes the query -- so it
    // lowers to the generic instance builtin call on its operand, not through a
    // receiver-type-keyed method table.
    if (info.subroutine != nullptr &&
        info.subroutine->knownNameId ==
            slang::parsing::KnownSystemName::IsUnknown) {
      auto type_id = unit_lowerer.InternType(*call.type, span);
      if (!type_id) return std::unexpected(std::move(type_id.error()));
      return hir::Expr{
          .type = *type_id,
          .data =
              hir::CallExpr{
                  .callee =
                      hir::BuiltinMethodRef{
                          .method = support::BuiltinFn::kIsUnknown},
                  .arguments = std::move(arg_ids),
              },
          .span = span,
      };
    }

    // LRM 20.8.1 `$clog2`: ceil(log2) of the operand read as unsigned. A
    // type-agnostic value query -- every value type exposes it -- so it lowers
    // to the generic instance builtin call on its operand. A constant argument
    // is folded by the downstream optimizer, never in lowering.
    if (info.subroutine != nullptr &&
        info.subroutine->knownNameId ==
            slang::parsing::KnownSystemName::Clog2) {
      auto type_id = unit_lowerer.InternType(*call.type, span);
      if (!type_id) return std::unexpected(std::move(type_id.error()));
      return hir::Expr{
          .type = *type_id,
          .data =
              hir::CallExpr{
                  .callee =
                      hir::BuiltinMethodRef{
                          .method = support::BuiltinFn::kClog2},
                  .arguments = std::move(arg_ids),
              },
          .span = span,
      };
    }

    // LRM 7.12.4 `item.index`: slang dresses the iteration index as a method on
    // the iterator (a SystemSubroutine with `KnownSystemName::Index`) whose
    // receiver value is discarded. It is the index iteration parameter -- a
    // value co-equal with the element -- so it lowers to an
    // `IterationBindingRef` value reference, not a call. Its clause is the one
    // whose iterator is the receiver, so a nested clause's `item.index` still
    // names the right clause.
    if (info.subroutine != nullptr &&
        info.subroutine->knownNameId ==
            slang::parsing::KnownSystemName::Index) {
      const auto& receiver = *call.arguments()[0];
      if (receiver.kind != slang::ast::ExpressionKind::NamedValue) {
        throw InternalError("item.index receiver is not a named iterator");
      }
      const auto clause = frame.FindIterationClause(
          receiver.as<slang::ast::NamedValueExpression>().symbol);
      if (!clause) {
        throw InternalError(
            "item.index receiver is not an active with-clause iterator");
      }
      auto type_id = unit_lowerer.InternType(*call.type, span);
      if (!type_id) return std::unexpected(std::move(type_id.error()));
      return hir::Expr{
          .type = *type_id,
          .data =
              hir::PrimaryExpr{
                  .data =
                      hir::IterationBindingRef{
                          .clause = *clause,
                          .role = hir::IterationBindingRole::kIndex}},
          .span = span,
      };
    }

    // `$signed` / `$unsigned` reinterpret the operand under the named
    // signedness. The result type already carries that signedness, so this is a
    // value conversion to the result type, not a runtime call.
    if (info.subroutine != nullptr &&
        (info.subroutine->knownNameId ==
             slang::parsing::KnownSystemName::Signed ||
         info.subroutine->knownNameId ==
             slang::parsing::KnownSystemName::Unsigned)) {
      auto type_id = unit_lowerer.InternType(*call.type, span);
      if (!type_id) return std::unexpected(std::move(type_id.error()));
      return hir::Expr{
          .type = *type_id,
          .data =
              hir::ConversionExpr{
                  .operand = *arg_ids.front(),
                  .kind = hir::ConversionKind::kImplicit},
          .span = span,
      };
    }

    const auto* desc = support::FindSystemSubroutine(name);
    if (desc == nullptr) {
      // slang resolved a system task / function that Lyra's registry does not
      // carry: a legitimate but unimplemented construct, not a compiler bug.
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedExpressionForm,
          std::string{"system task / function '"} + std::string{name} +
              "' is not yet supported");
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

    const auto result_type = MakeReturnConventionType(
        unit_lowerer.Unit().builtins, desc->result_conv);
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

  // A method of an imported runtime-library class routes to the library symbol.
  // A static method carries no receiver; an instance method lowers its handle,
  // present in `thisClass`.
  if (const auto imported = DetectImportedRuntimeMethod(*sym)) {
    auto result_type = unit_lowerer.InternType(*call.type, span);
    if (!result_type) return std::unexpected(std::move(result_type.error()));
    std::optional<hir::ExprId> receiver;
    if (const slang::ast::Expression* this_class = call.thisClass();
        this_class != nullptr) {
      auto receiver_or = lowerer.LowerExpr(*this_class, frame);
      if (!receiver_or) return std::unexpected(std::move(receiver_or.error()));
      receiver = frame.Exprs().Add(*std::move(receiver_or));
    }
    return hir::Expr{
        .type = *result_type,
        .data =
            hir::CallExpr{
                .callee =
                    hir::ImportedMethodRef{
                        .method = *imported, .receiver = receiver},
                .arguments = std::move(arg_ids),
            },
        .span = span,
    };
  }

  // A static class method call (LRM 8.10): the target has no receiver, so it
  // lowers to `StaticMethodCallRef` rather than `MethodCallRef` -- a `handle`-
  // qualified call to a static method still discards the handle because the
  // callee never observes it (LRM 8.10 "no access to non-static members").
  // The declaring class comes from slang's resolved callee, so an inherited
  // static call reaches the base's arena the same way an instance-method
  // call does under LRM 8.13.
  if (sym->flags.has(slang::ast::MethodFlags::Static) &&
      sym->getParentScope() != nullptr &&
      sym->getParentScope()->asSymbol().kind ==
          slang::ast::SymbolKind::ClassType) {
    for (const auto* formal : sym->getArguments()) {
      if (formal->direction != slang::ast::ArgumentDirection::In) {
        return diag::Fail(
            span, diag::DiagCode::kUnsupportedClassFeature,
            "static class methods with output / inout / ref arguments are not "
            "yet supported");
      }
    }
    const auto& declaring_class =
        sym->getParentScope()->asSymbol().as<slang::ast::ClassType>();
    auto class_ref = unit_lowerer.ResolveClassRef(declaring_class, span);
    if (!class_ref) return std::unexpected(std::move(class_ref.error()));
    auto static_result_type = unit_lowerer.InternType(*call.type, span);
    if (!static_result_type) {
      return std::unexpected(std::move(static_result_type.error()));
    }
    return hir::Expr{
        .type = *static_result_type,
        .data =
            hir::CallExpr{
                .callee =
                    hir::StaticMethodCallRef{
                        .target = unit_lowerer.MakeClassMethodTarget(
                            *class_ref, *sym)},
                .arguments = std::move(arg_ids),
            },
        .span = span,
    };
  }

  // Instance-method call (LRM 8.6): three source shapes -- `h.foo()`,
  // implicit-`this` `foo()` from inside a class body, and `super.foo()` --
  // all lower to one `MethodCallRef` distinguished by which `MethodReceiver`
  // arm the classifier picks. The declaring class comes from slang's
  // resolved callee, which already accounts for inheritance and super
  // resolution.
  if (CallReachesInstanceMethod(call, *sym)) {
    for (const auto* formal : sym->getArguments()) {
      if (formal->direction != slang::ast::ArgumentDirection::In) {
        return diag::Fail(
            span, diag::DiagCode::kUnsupportedClassFeature,
            "instance methods with output / inout / ref arguments are not yet "
            "supported");
      }
    }
    auto receiver_or = ClassifyMethodReceiver(lowerer, frame, call);
    if (!receiver_or) return std::unexpected(std::move(receiver_or.error()));
    const auto& declaring_class =
        sym->getParentScope()->asSymbol().as<slang::ast::ClassType>();
    auto class_ref = unit_lowerer.ResolveClassRef(declaring_class, span);
    if (!class_ref) return std::unexpected(std::move(class_ref.error()));
    auto method_result_type = unit_lowerer.InternType(*call.type, span);
    if (!method_result_type) {
      return std::unexpected(std::move(method_result_type.error()));
    }
    return hir::Expr{
        .type = *method_result_type,
        .data =
            hir::CallExpr{
                .callee =
                    hir::MethodCallRef{
                        .receiver = *std::move(receiver_or),
                        .target = unit_lowerer.MakeClassMethodTarget(
                            *class_ref, *sym)},
                .arguments = std::move(arg_ids),
            },
        .span = span,
    };
  }

  // A DPI-C import is a bodyless external callable, bound in a separate space
  // from body-bearing subroutines. Resolve it to a ForeignImportRef; its ABI
  // and foreign name were classified once at declaration and are not re-derived
  // here.
  if (const auto import_binding =
          unit_lowerer.LookupForeignImportBinding(*sym)) {
    const auto import_hops = frame.HopsTo(import_binding->owner_frame);
    if (!import_hops.has_value()) {
      throw InternalError(
          "AST->HIR call: DPI import owner frame is not on the current scope "
          "stack");
    }
    auto import_result_type = unit_lowerer.InternType(*call.type, span);
    if (!import_result_type) {
      return std::unexpected(std::move(import_result_type.error()));
    }
    return hir::Expr{
        .type = *import_result_type,
        .data =
            hir::CallExpr{
                .callee =
                    hir::ForeignImportRef{
                        .hops = *import_hops, .id = import_binding->import_id},
                .arguments = std::move(arg_ids),
            },
        .span = span,
    };
  }

  // A subroutine declared in a package belongs to another compilation unit
  // (LRM 26.3). It is reached by name across the unit boundary rather than
  // through an enclosing-scope binding of this unit.
  if (const auto* pkg = EnclosingPackage(*sym)) {
    for (const auto* formal : sym->getArguments()) {
      if (formal->direction != slang::ast::ArgumentDirection::In) {
        return diag::Fail(
            span, diag::DiagCode::kUnsupportedExpressionForm,
            "package subroutine with output / inout / ref arguments is not yet "
            "supported");
      }
    }
    auto result_type = unit_lowerer.InternType(*call.type, span);
    if (!result_type) return std::unexpected(std::move(result_type.error()));
    return hir::Expr{
        .type = *result_type,
        .data =
            hir::CallExpr{
                .callee =
                    hir::ExternalUnitSubroutineRef{
                        .unit_name = std::string{pkg->name},
                        .subroutine_name = std::string{sym->name},
                        .kind = ToHirSubroutineKind(sym->subroutineKind)},
                .arguments = std::move(arg_ids)},
        .span = span,
    };
  }

  const auto binding = unit_lowerer.LookupSubroutineBinding(*sym);
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
  auto result_type = unit_lowerer.InternType(*call.type, span);
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

template auto LowerCallExpr(
    ProcessLowerer& lowerer, WalkFrame frame,
    const slang::ast::CallExpression& call, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;
template auto LowerCallExpr(
    StructuralScopeLowerer& lowerer, WalkFrame frame,
    const slang::ast::CallExpression& call, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;

}  // namespace lyra::lowering::ast_to_hir
