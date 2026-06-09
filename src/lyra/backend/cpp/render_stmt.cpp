#include "lyra/backend/cpp/render_stmt.hpp"

#include <cstddef>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/backend/cpp/formatting.hpp"
#include "lyra/backend/cpp/render_context.hpp"
#include "lyra/backend/cpp/render_expr.hpp"
#include "lyra/backend/cpp/render_type.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/structural_scope.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::backend::cpp {

namespace {

auto RenderForInit(const RenderContext& ctx, const mir::ForInit& init)
    -> diag::Result<std::string> {
  return std::visit(
      Overloaded{
          [&](const mir::ForInitDecl& d) -> diag::Result<std::string> {
            // The induction variable's C++ type is whatever the initializer
            // yields (every integral value is a PackedArray), so `auto` is the
            // exact same type as spelling it out -- and reads as the idiomatic
            // loop counter.
            const auto& lv = ctx.ProceduralScopeAtHops(d.induction_var.hops)
                                 .vars.at(d.induction_var.var.value);
            const auto& init_expr =
                ctx.ProceduralScope().exprs.at(d.init.value);
            auto rendered_or = RenderExpr(ctx, init_expr);
            if (!rendered_or) {
              return std::unexpected(std::move(rendered_or.error()));
            }
            return "auto " + lv.name + " = " + *rendered_or;
          },
          [&](const mir::ForInitExpr& e) -> diag::Result<std::string> {
            const auto& expr = ctx.ProceduralScope().exprs.at(e.expr.value);
            return RenderExpr(ctx, expr);
          },
      },
      init);
}

auto RenderEventEdgeAsRuntime(mir::EventEdge edge) -> std::string_view {
  switch (edge) {
    case mir::EventEdge::kAnyChange:
      return "lyra::runtime::Edge::kAnyChange";
    case mir::EventEdge::kPosedge:
      return "lyra::runtime::Edge::kPosedge";
    case mir::EventEdge::kNegedge:
      return "lyra::runtime::Edge::kNegedge";
    case mir::EventEdge::kBothEdges:
      return "lyra::runtime::Edge::kBothEdges";
  }
  throw InternalError("RenderEventEdgeAsRuntime: unknown EventEdge value");
}

auto RenderProceduralVarDeclStmt(
    const RenderContext& ctx, const mir::ProceduralVarDeclStmt& s,
    std::size_t indent) -> diag::Result<std::string> {
  const auto& lv =
      ctx.ProceduralScopeAtHops(s.target.hops).vars.at(s.target.var.value);
  auto type_or = RenderTypeAsCpp(ctx.Unit(), ctx.StructuralScope(), lv.type);
  if (!type_or) return std::unexpected(std::move(type_or.error()));
  const auto& init_expr = ctx.ProceduralScope().exprs.at(s.init.value);
  auto init_or = RenderExpr(ctx, init_expr);
  if (!init_or) return std::unexpected(std::move(init_or.error()));
  return Indent(indent) + *type_or + " " + lv.name + " = " + *init_or + ";\n";
}

auto RenderExprStmt(
    const RenderContext& ctx, const mir::ExprStmt& s, std::size_t indent)
    -> diag::Result<std::string> {
  const auto& expr = ctx.ProceduralScope().exprs.at(s.expr.value);
  auto rendered_or = RenderExpr(ctx, expr);
  if (!rendered_or) return std::unexpected(std::move(rendered_or.error()));
  return Indent(indent) + *rendered_or + ";\n";
}

auto RenderBlockStmtNode(
    const RenderContext& ctx, const mir::Stmt& stmt, const mir::BlockStmt& s,
    std::size_t indent) -> diag::Result<std::string> {
  const auto& child = stmt.child_procedural_scopes.at(s.scope.value);
  std::string result = Indent(indent) + "{\n";
  auto child_or = RenderNestedProceduralScope(ctx, child, indent + 1);
  if (!child_or) return std::unexpected(std::move(child_or.error()));
  result += *child_or;
  result += Indent(indent) + "}\n";
  return result;
}

auto RenderForkJoinModeLiteral(mir::JoinMode mode) -> std::string_view {
  switch (mode) {
    case mir::JoinMode::kAll:
      return "lyra::runtime::JoinMode::kAll";
    case mir::JoinMode::kAny:
      return "lyra::runtime::JoinMode::kAny";
    case mir::JoinMode::kNone:
      return "lyra::runtime::JoinMode::kNone";
  }
  return "lyra::runtime::JoinMode::kAll";
}

// LRM 9.3.2: a fork is a procedural scope, rendered as a block at the fork
// site. Its block_item_declarations initialize first, in the parent, before any
// branch spawns -- so a branch's by-value capture of one of them is a snapshot.
// Each branch is an anonymous ClosureExpr spawned as a concurrent coroutine,
// rendered inline as a stateless lambda whose parameters are frame-copied -- so
// the spawned coroutine never dangles the way a captured lambda would. The
// first parameter is the enclosing scope instance `self`, through which the
// body reaches module state and Services(); each closure capture adds one more
// parameter (a `T` snapshot for a by-value capture of a fork-scope local, a
// `T&` for a by-reference capture of an enclosing variable). The fork waits per
// the join mode.
auto RenderForkStmtNode(
    const RenderContext& ctx, const mir::Stmt& stmt, const mir::ForkStmt& s,
    std::size_t indent) -> diag::Result<std::string> {
  const std::string vec = ctx.AllocateTemp("fork_branches");
  const std::string& class_name = ctx.StructuralScope().name;
  const std::string receiver_object{ctx.ReceiverObject()};
  const RenderContext fork_ctx =
      ctx.WithProceduralScope(stmt.child_procedural_scopes.at(s.scope.value));
  std::string out = Indent(indent) + "{\n";
  auto locals_or = RenderProceduralScopeStatements(fork_ctx, indent + 1);
  if (!locals_or) return std::unexpected(std::move(locals_or.error()));
  out += *locals_or;
  out += Indent(indent + 1) + "std::vector<lyra::runtime::Coroutine> " + vec +
         ";\n";
  for (const auto branch : s.branches) {
    const auto* closure =
        std::get_if<mir::ClosureExpr>(&fork_ctx.Expr(branch).data);
    if (closure == nullptr) {
      throw InternalError("RenderForkStmtNode: fork branch is not a closure");
    }
    if (closure->body == nullptr) {
      throw InternalError(
          "RenderForkStmtNode: fork branch closure has no body");
    }
    std::string params = class_name + "* self";
    std::string args{receiver_object};
    for (const auto& capture : closure->captures) {
      mir::ProceduralVarId binding{};
      std::string ref_marker;
      std::string arg;
      if (const auto* by_ref = std::get_if<mir::ByReferenceCapture>(&capture)) {
        binding = by_ref->binding;
        ref_marker = "& ";
        auto target_or = RenderExpr(fork_ctx, fork_ctx.Expr(by_ref->target));
        if (!target_or) return std::unexpected(std::move(target_or.error()));
        arg = *std::move(target_or);
      } else {
        const auto& by_value = std::get<mir::ByValueCapture>(capture);
        binding = by_value.binding;
        ref_marker = " ";
        auto value_or = RenderExpr(fork_ctx, fork_ctx.Expr(by_value.value));
        if (!value_or) return std::unexpected(std::move(value_or.error()));
        arg = *std::move(value_or);
      }
      const auto& bind = closure->body->vars.at(binding.value);
      auto type_or =
          RenderTypeAsCpp(ctx.Unit(), ctx.StructuralScope(), bind.type);
      if (!type_or) return std::unexpected(std::move(type_or.error()));
      params += ", " + *type_or + ref_marker + bind.name;
      args += ", " + arg;
    }
    const RenderContext branch_ctx =
        fork_ctx.WithProceduralScope(*closure->body)
            .WithCoroutine(true)
            .WithReceiver("self");
    auto body_or = RenderProceduralScopeStatements(branch_ctx, indent + 2);
    if (!body_or) return std::unexpected(std::move(body_or.error()));
    out += Indent(indent + 1) + vec + ".push_back([](" + params +
           ") -> lyra::runtime::Coroutine {\n";
    out += *body_or;
    out += Indent(indent + 2) + "co_return;\n";
    out += Indent(indent + 1) + "}(" + args + "));\n";
  }
  out += Indent(indent + 1) + "co_await lyra::runtime::Fork(" +
         ctx.ServicesRef() + ", std::move(" + vec + "), " +
         std::string(RenderForkJoinModeLiteral(s.mode)) + ");\n";
  out += Indent(indent) + "}\n";
  return out;
}

auto RenderIfStmtNode(
    const RenderContext& ctx, const mir::Stmt& stmt, const mir::IfStmt& s,
    std::size_t indent) -> diag::Result<std::string> {
  const auto& cond_expr = ctx.ProceduralScope().exprs.at(s.condition.value);
  const auto& then_scope = stmt.child_procedural_scopes.at(s.then_scope.value);
  auto cond_or = RenderConditionAsBool(ctx, cond_expr);
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  auto then_or = RenderNestedProceduralScope(ctx, then_scope, indent + 1);
  if (!then_or) return std::unexpected(std::move(then_or.error()));
  std::string result;
  result += Indent(indent) + "if (" + *cond_or + ") {\n";
  result += *then_or;
  result += Indent(indent) + "}";
  if (s.else_scope.has_value()) {
    const auto& else_scope =
        stmt.child_procedural_scopes.at(s.else_scope->value);
    auto else_or = RenderNestedProceduralScope(ctx, else_scope, indent + 1);
    if (!else_or) return std::unexpected(std::move(else_or.error()));
    result += " else {\n";
    result += *else_or;
    result += Indent(indent) + "}";
  }
  result += "\n";
  return result;
}

auto RenderConstructOwnedObjectStmt(
    const RenderContext& ctx, const mir::ConstructOwnedObjectStmt& s,
    std::size_t indent) -> diag::Result<std::string> {
  const auto& var = ctx.StructuralScope().GetStructuralVar(s.target);
  const auto& target_scope =
      ctx.StructuralScope().GetChildStructuralScope(s.scope_id);
  std::string trailing_args;
  for (const auto arg_id : s.args) {
    auto arg_or = RenderExpr(ctx, ctx.Expr(arg_id));
    if (!arg_or) return std::unexpected(std::move(arg_or.error()));
    trailing_args += ", " + *arg_or;
  }
  const std::string make = "std::make_unique<" + target_scope.name +
                           ">(this, \"" + target_scope.name + "\"" +
                           trailing_args + ")";
  if (mir::IsVectorOfOwningObjectType(ctx.Unit(), var.type)) {
    return Indent(indent) + ctx.MemberPrefix() + var.name + ".push_back(" +
           make + ");\n";
  }
  if (mir::IsOwningObjectType(ctx.Unit(), var.type)) {
    return Indent(indent) + ctx.MemberPrefix() + var.name + " = " + make +
           ";\n";
  }
  throw InternalError(
      "ConstructOwnedObjectStmt target is not an owning object var");
}

// Materializes an external-unit member by recursing on its type, mirroring the
// type walk in RenderTypeAsCpp: each vector layer emits a counted loop that
// grows the vector and recurses into the new element; the owning-pointer leaf
// assigns a freshly constructed child whose parent is `this` and whose name is
// the member's compile-time label. `dims` carries one element count per vector
// layer, so a scalar (no vector layer) renders just the leaf.
auto RenderExternalUnitFill(
    const RenderContext& ctx, const std::string& lvalue, mir::TypeId type,
    const std::string& unit_name, const std::string& label,
    const std::vector<std::uint32_t>& dims, std::size_t depth,
    std::size_t indent) -> std::string {
  if (const auto* vec =
          std::get_if<mir::VectorType>(&ctx.Unit().GetType(type).data)) {
    const std::string idx = "i" + std::to_string(depth);
    std::string out;
    out += Indent(indent) + "for (std::size_t " + idx + " = 0; " + idx + " < " +
           std::to_string(dims.at(depth)) + "; ++" + idx + ") {\n";
    out += Indent(indent + 1) + lvalue + ".emplace_back();\n";
    out += RenderExternalUnitFill(
        ctx, lvalue + "[" + idx + "]", vec->element, unit_name, label, dims,
        depth + 1, indent + 1);
    out += Indent(indent) + "}\n";
    return out;
  }
  return Indent(indent) + lvalue + " = std::make_unique<" + unit_name +
         ">(this, \"" + label + "\");\n";
}

auto RenderConstructExternalUnitStmt(
    const RenderContext& ctx, const mir::ConstructExternalUnitStmt& s,
    std::size_t indent) -> diag::Result<std::string> {
  const auto& var = ctx.StructuralScope().GetStructuralVar(s.target);
  return RenderExternalUnitFill(
      ctx, ctx.MemberPrefix() + var.name, var.type, s.unit_name, var.name,
      s.dims, 0, indent);
}

// Emits a `ChildRef` literal for a by-name owned-child hop: the LRM name plus,
// for an indexed loop block or instance array, the per-dimension indices in a
// temporary `std::array` whose lifetime spans the surrounding navigation call.
auto RenderChildRefLiteral(
    const std::string& name, const std::vector<std::uint32_t>& indices)
    -> std::string {
  std::string out = "lyra::runtime::ChildRef{.name = \"" + name + "\"";
  if (!indices.empty()) {
    out += ", .indices = std::array<std::size_t, " +
           std::to_string(indices.size()) + ">{";
    for (std::size_t i = 0; i < indices.size(); ++i) {
      if (i != 0) out += ", ";
      out += std::to_string(indices[i]);
    }
    out += "}";
  }
  out += "}";
  return out;
}

// A downward cross-unit reference resolves by name across the unit boundary
// (emission_model.md). The navigation is already resolved in MIR -- `steps` are
// the owned children to fetch with `GetChild` in order, `signal` is the leaf to
// fetch with `GetSignal` -- so this renders each step mechanically and casts to
// the slot's own cell type (`Var<T>*` for an observable scalar, `T*`
// otherwise), a representation fixed by the slot's type.
auto RenderResolveCrossUnitRefStmt(
    const RenderContext& ctx, const mir::ResolveCrossUnitRefStmt& s,
    std::size_t indent) -> diag::Result<std::string> {
  const auto& slot = ctx.StructuralScope().GetCrossUnitRef(s.slot);

  auto elem_or = RenderTypeAsCpp(ctx.Unit(), ctx.StructuralScope(), slot.type);
  if (!elem_or) return std::unexpected(std::move(elem_or.error()));
  const std::string cast_type =
      IsObservableScalarType(ctx.Unit().GetType(slot.type))
          ? "lyra::runtime::Var<" + *elem_or + ">*"
          : *elem_or + "*";

  std::string nav = ctx.MemberPrefix();
  for (const auto& step : slot.steps) {
    nav += "GetChild(" + RenderChildRefLiteral(step.name, step.indices) + ")->";
  }
  nav += "GetSignal(\"" + slot.signal + "\")";

  return Indent(indent) + ctx.MemberPrefix() +
         CrossUnitRefSlotName(s.slot.value) + " = static_cast<" + cast_type +
         ">(" + nav + ");\n";
}

auto RenderForStmtNode(
    const RenderContext& ctx, const mir::Stmt& stmt, const mir::ForStmt& s,
    std::size_t indent) -> diag::Result<std::string> {
  std::string init;
  for (std::size_t i = 0; i < s.init.size(); ++i) {
    if (i != 0) init += ", ";
    auto init_or = RenderForInit(ctx, s.init[i]);
    if (!init_or) return std::unexpected(std::move(init_or.error()));
    init += *init_or;
  }
  std::string cond;
  if (s.condition.has_value()) {
    const auto& cond_expr = ctx.ProceduralScope().exprs.at(s.condition->value);
    auto cond_or = RenderConditionAsBool(ctx, cond_expr);
    if (!cond_or) return std::unexpected(std::move(cond_or.error()));
    cond = *std::move(cond_or);
  }
  std::string step;
  for (std::size_t i = 0; i < s.step.size(); ++i) {
    if (i != 0) step += ", ";
    const auto& step_expr = ctx.ProceduralScope().exprs.at(s.step[i].value);
    auto step_or = RenderExpr(ctx, step_expr);
    if (!step_or) return std::unexpected(std::move(step_or.error()));
    step += *step_or;
  }
  const auto& scope = stmt.child_procedural_scopes.at(s.scope.value);
  auto body_or = RenderNestedProceduralScope(ctx, scope, indent + 1);
  if (!body_or) return std::unexpected(std::move(body_or.error()));
  std::string result =
      Indent(indent) + "for (" + init + "; " + cond + "; " + step + ") {\n";
  result += *body_or;
  result += Indent(indent) + "}\n";
  return result;
}

auto RenderWhileStmtNode(
    const RenderContext& ctx, const mir::Stmt& stmt, const mir::WhileStmt& s,
    std::size_t indent) -> diag::Result<std::string> {
  const auto& cond_expr = ctx.ProceduralScope().exprs.at(s.condition.value);
  auto cond_or = RenderConditionAsBool(ctx, cond_expr);
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  const auto& scope = stmt.child_procedural_scopes.at(s.scope.value);
  auto body_or = RenderNestedProceduralScope(ctx, scope, indent + 1);
  if (!body_or) return std::unexpected(std::move(body_or.error()));
  std::string result =
      Indent(indent) + "while (" + *std::move(cond_or) + ") {\n";
  result += *body_or;
  result += Indent(indent) + "}\n";
  return result;
}

auto RenderDoWhileStmtNode(
    const RenderContext& ctx, const mir::Stmt& stmt, const mir::DoWhileStmt& s,
    std::size_t indent) -> diag::Result<std::string> {
  const auto& cond_expr = ctx.ProceduralScope().exprs.at(s.condition.value);
  auto cond_or = RenderConditionAsBool(ctx, cond_expr);
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  const auto& scope = stmt.child_procedural_scopes.at(s.scope.value);
  auto body_or = RenderNestedProceduralScope(ctx, scope, indent + 1);
  if (!body_or) return std::unexpected(std::move(body_or.error()));
  std::string result = Indent(indent) + "do {\n";
  result += *body_or;
  result += Indent(indent) + "} while (" + *std::move(cond_or) + ");\n";
  return result;
}

// The Observable pointer a sensitivity leaf subscribes to: a plain structural
// var yields the address of its own field; an ExternalRef member subscribes
// through its resolved cell (AsObservable); a downward slot is already a
// resolved `Var<T>*`, so the slot name is the pointer.
auto RenderSensitivityRefPtr(
    const RenderContext& ctx, const mir::SensitivityRef& ref)
    -> diag::Result<std::string> {
  return std::visit(
      Overloaded{
          [&](const mir::StructuralVarRef& r) -> diag::Result<std::string> {
            auto name = RenderStructuralVarName(ctx, r);
            if (!name) return std::unexpected(std::move(name.error()));
            const auto& var =
                ctx.StructuralScopeAtHops(r.hops).GetStructuralVar(r.var);
            if (std::holds_alternative<mir::ExternalRefType>(
                    ctx.Unit().GetType(var.type).data)) {
              return *name + ".AsObservable()";
            }
            return "&" + *name;
          },
          [&](const mir::CrossUnitVarRef& r) -> diag::Result<std::string> {
            return ctx.MemberPrefix() + CrossUnitRefSlotName(r.id.value);
          },
      },
      ref);
}

auto RenderSensitivityWaitStmt(
    const RenderContext& ctx, const mir::SensitivityWaitStmt& s,
    std::size_t indent) -> diag::Result<std::string> {
  std::string result = Indent(indent) + "co_await lyra::runtime::WaitAny({";
  for (std::size_t i = 0; i < s.reads.size(); ++i) {
    const auto& read = s.reads[i];
    auto ptr_or = RenderSensitivityRefPtr(ctx, read.ref);
    if (!ptr_or) return std::unexpected(std::move(ptr_or.error()));
    if (i != 0) result += ", ";
    // bit_range follows slang's `(lo_bit, hi_bit)` inclusive convention; the
    // runtime Trigger takes `(lsb_offset, width)`.
    const auto lsb = read.bit_range.first;
    const auto width = read.bit_range.second - read.bit_range.first + 1;
    result += "{" + *ptr_or + ", " +
              std::string{RenderEventEdgeAsRuntime(read.edge_kind)} + ", " +
              std::to_string(lsb) + "ULL, " + std::to_string(width) + "ULL}";
  }
  result += "});\n";
  return result;
}

}  // namespace

auto RenderStmt(
    const RenderContext& ctx, const mir::Stmt& stmt, std::size_t indent)
    -> diag::Result<std::string> {
  std::string out;
  if (stmt.label.has_value()) {
    out += Indent(indent) + *stmt.label + ":\n";
  }
  auto rendered_or = std::visit(
      Overloaded{
          [&](const mir::EmptyStmt&) -> diag::Result<std::string> {
            return Indent(indent) + ";\n";
          },
          [&](const mir::DelayStmt& s) -> diag::Result<std::string> {
            return Indent(indent) + "co_await lyra::runtime::Delay(" +
                   ctx.ServicesRef() + ", " + std::to_string(s.duration) +
                   ", kTimePrecisionPower);\n";
          },
          [&](const mir::ProceduralVarDeclStmt& s)
              -> diag::Result<std::string> {
            return RenderProceduralVarDeclStmt(ctx, s, indent);
          },
          [&](const mir::ExprStmt& s) -> diag::Result<std::string> {
            return RenderExprStmt(ctx, s, indent);
          },
          [&](const mir::BlockStmt& s) -> diag::Result<std::string> {
            return RenderBlockStmtNode(ctx, stmt, s, indent);
          },
          [&](const mir::ForkStmt& s) -> diag::Result<std::string> {
            return RenderForkStmtNode(ctx, stmt, s, indent);
          },
          [&](const mir::IfStmt& s) -> diag::Result<std::string> {
            return RenderIfStmtNode(ctx, stmt, s, indent);
          },
          [&](const mir::ConstructOwnedObjectStmt& s)
              -> diag::Result<std::string> {
            return RenderConstructOwnedObjectStmt(ctx, s, indent);
          },
          [&](const mir::ConstructExternalUnitStmt& s)
              -> diag::Result<std::string> {
            return RenderConstructExternalUnitStmt(ctx, s, indent);
          },
          [&](const mir::ResolveCrossUnitRefStmt& s)
              -> diag::Result<std::string> {
            return RenderResolveCrossUnitRefStmt(ctx, s, indent);
          },
          [&](const mir::ForStmt& s) -> diag::Result<std::string> {
            return RenderForStmtNode(ctx, stmt, s, indent);
          },
          [&](const mir::WhileStmt& s) -> diag::Result<std::string> {
            return RenderWhileStmtNode(ctx, stmt, s, indent);
          },
          [&](const mir::DoWhileStmt& s) -> diag::Result<std::string> {
            return RenderDoWhileStmtNode(ctx, stmt, s, indent);
          },
          [&](const mir::BreakStmt&) -> diag::Result<std::string> {
            return Indent(indent) + "break;\n";
          },
          [&](const mir::ContinueStmt&) -> diag::Result<std::string> {
            return Indent(indent) + "continue;\n";
          },
          [&](const mir::ReturnStmt& s) -> diag::Result<std::string> {
            // A task body is a coroutine, so its early `return` is
            // `co_return`; a task carries no return value (LRM 13.3). A
            // function body is a plain method.
            if (ctx.InCoroutine()) {
              return Indent(indent) + "co_return;\n";
            }
            if (!s.value.has_value()) {
              return Indent(indent) + "return;\n";
            }
            auto value_or = RenderExpr(ctx, ctx.Expr(*s.value));
            if (!value_or) return std::unexpected(std::move(value_or.error()));
            return Indent(indent) + "return " + *value_or + ";\n";
          },
          [&](const mir::SensitivityWaitStmt& s) -> diag::Result<std::string> {
            return RenderSensitivityWaitStmt(ctx, s, indent);
          },
      },
      stmt.data);
  if (!rendered_or) return std::unexpected(std::move(rendered_or.error()));
  out += *rendered_or;
  return out;
}

auto RenderProceduralScopeStatements(
    const RenderContext& ctx, std::size_t indent) -> diag::Result<std::string> {
  const auto& scope = ctx.ProceduralScope();
  // When a scope's whole content is a single begin/end block, the enclosing
  // braces (a function body, a loop or branch body, an outer block) already
  // scope it; render the block's body directly instead of emitting a redundant
  // `{ }`. Recursing collapses a chain of such blocks.
  if (scope.root_stmts.size() == 1) {
    const auto& only = scope.stmts.at(scope.root_stmts.front().value);
    if (const auto* block = std::get_if<mir::BlockStmt>(&only.data)) {
      const auto& inner = only.child_procedural_scopes.at(block->scope.value);
      return RenderProceduralScopeStatements(
          ctx.WithProceduralScope(inner), indent);
    }
  }
  std::string out;
  for (const auto& sid : scope.root_stmts) {
    auto rendered_or = RenderStmt(ctx, scope.stmts.at(sid.value), indent);
    if (!rendered_or) return std::unexpected(std::move(rendered_or.error()));
    out += *rendered_or;
  }
  return out;
}

auto RenderNestedProceduralScope(
    const RenderContext& parent, const mir::ProceduralScope& scope,
    std::size_t indent) -> diag::Result<std::string> {
  const RenderContext child = parent.WithProceduralScope(scope);
  return RenderProceduralScopeStatements(child, indent);
}

}  // namespace lyra::backend::cpp
