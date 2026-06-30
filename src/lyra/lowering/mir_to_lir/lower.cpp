#include "lyra/lowering/mir_to_lir/lower.hpp"

#include <cstddef>
#include <cstdint>
#include <optional>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::mir_to_lir {

namespace {

// Lowers one MIR callable body (a method's code or a class constructor) into a
// LIR function. The body is flattened: each MIR expression node becomes one LIR
// instruction defining a temporary, and the parent reads the child temporaries
// as operands.
class FunctionLowerer {
 public:
  FunctionLowerer(
      const mir::Block& body, const std::vector<mir::LocalId>& params,
      std::string name, mir::TypeId result_type)
      : body_(&body), local_to_value_(body.vars.size(), std::nullopt) {
    fn_.name = std::move(name);
    fn_.result_type = result_type;
    for (const mir::LocalId param : params) {
      const mir::LocalDecl& decl = body.vars.Get(param);
      const lir::ValueId value = fn_.values.Add(
          lir::Local{
              .name = decl.name,
              .type = decl.type,
              .kind = lir::LocalKind::kParam});
      fn_.params.push_back(value);
      local_to_value_[param.value] = value;
    }
  }

  auto Run() -> lir::Function {
    for (const mir::StmtId sid : body_->root_stmts) {
      LowerStmt(body_->stmts.Get(sid));
    }
    if (!terminated_) {
      block_.terminator = lir::Terminator{
          .data =
              lir::ReturnTerm{.value = std::nullopt, .is_coroutine = false}};
    }
    fn_.blocks.push_back(std::move(block_));
    return std::move(fn_);
  }

 private:
  void LowerStmt(const mir::Stmt& stmt) {
    std::visit(
        Overloaded{
            [](const mir::EmptyStmt&) {},
            [&](const mir::ExprStmt& s) { LowerExpr(s.expr); },
            [&](const mir::ReturnStmt& s) {
              std::optional<lir::Operand> value;
              if (s.value.has_value()) {
                value = LowerExpr(*s.value);
              }
              block_.terminator = lir::Terminator{
                  .data = lir::ReturnTerm{
                      .value = std::move(value),
                      .is_coroutine = s.is_coroutine_return}};
              terminated_ = true;
            },
            [](const auto&) {
              throw InternalError(
                  "mir_to_lir: statement kind not yet supported");
            }},
        stmt.data);
  }

  auto LowerExpr(mir::ExprId id) -> lir::Operand {
    const mir::Expr& expr = body_->exprs.Get(id);
    const mir::TypeId type = expr.type;
    return std::visit(
        Overloaded{
            [&](const mir::IntegerLiteral& lit) -> lir::Operand {
              return lir::IntConst{.value = lit.value, .type = type};
            },
            [&](const mir::StringLiteral& lit) -> lir::Operand {
              return lir::StrConst{.value = lit.value, .type = type};
            },
            [&](const mir::LocalRef& ref) -> lir::Operand {
              if (ref.hops.value != 0) {
                throw InternalError(
                    "mir_to_lir: non-local variable reference not yet "
                    "supported");
              }
              const std::optional<lir::ValueId> value =
                  local_to_value_[ref.var.value];
              if (!value.has_value()) {
                throw InternalError(
                    "mir_to_lir: reference to an unlowered local");
              }
              return lir::Use{.value = *value};
            },
            [&](const mir::CallExpr& call) -> lir::Operand {
              std::vector<lir::Operand> args;
              args.reserve(call.arguments.size());
              for (const mir::ExprId arg : call.arguments) {
                args.push_back(LowerExpr(arg));
              }
              const lir::CallTarget target = LowerCallTarget(call.callee);
              return Emit(
                  type,
                  lir::CallInstr{.target = target, .args = std::move(args)});
            },
            [&](const mir::ArrayLiteralExpr& lit) -> lir::Operand {
              std::vector<lir::Operand> elements;
              elements.reserve(lit.elements.size());
              for (const mir::ExprId elem : lit.elements) {
                elements.push_back(LowerExpr(elem));
              }
              return Emit(
                  type, lir::AggregateInstr{.elements = std::move(elements)});
            },
            [](const auto&) -> lir::Operand {
              throw InternalError(
                  "mir_to_lir: expression kind not yet supported");
            }},
        expr.data);
  }

  static auto LowerCallTarget(const mir::Callee& callee) -> lir::CallTarget {
    return std::visit(
        Overloaded{
            [](const mir::Direct& d) -> lir::CallTarget { return d; },
            [](const mir::Construct& c) -> lir::CallTarget { return c; },
            [](const mir::Indirect&) -> lir::CallTarget {
              throw InternalError(
                  "mir_to_lir: indirect (closure) call not yet supported");
            }},
        callee);
  }

  auto Emit(mir::TypeId type, lir::InstrData data) -> lir::Operand {
    const lir::ValueId result = fn_.values.Add(
        lir::Local{.name = {}, .type = type, .kind = lir::LocalKind::kTemp});
    block_.instrs.push_back(
        lir::Instr{.result = result, .data = std::move(data)});
    return lir::Use{.value = result};
  }

  const mir::Block* body_;
  lir::Function fn_;
  lir::BasicBlock block_;
  std::vector<std::optional<lir::ValueId>> local_to_value_;
  bool terminated_ = false;
};

auto LowerClass(const mir::CompilationUnit& unit, const mir::Class& cls)
    -> lir::Class {
  lir::Class out;
  out.name = cls.name;
  out.base = cls.base;

  out.constructor = FunctionLowerer(
                        cls.constructor_block, {mir::LocalId{0}}, "constructor",
                        unit.builtins.void_type)
                        .Run();

  for (std::size_t i = 0; i < cls.methods.size(); ++i) {
    const mir::MethodDecl& method =
        cls.methods.Get(mir::MethodId{static_cast<std::uint32_t>(i)});
    out.methods.push_back(FunctionLowerer(
                              method.code.body, method.code.params, method.name,
                              method.code.result_type)
                              .Run());
  }
  return out;
}

}  // namespace

auto LowerUnit(const mir::CompilationUnit& unit) -> lir::CompilationUnit {
  lir::CompilationUnit out;
  out.source = &unit;
  for (std::size_t i = 0; i < unit.classes.size(); ++i) {
    const mir::ClassId id{static_cast<std::uint32_t>(i)};
    if (!unit.classes.IsDefined(id)) {
      throw InternalError("mir_to_lir: undefined class in unit");
    }
    const lir::ClassId lowered =
        out.classes.Add(LowerClass(unit, unit.GetClass(id)));
    if (id == unit.root) {
      out.root = lowered;
    }
  }
  return out;
}

}  // namespace lyra::lowering::mir_to_lir
