#include "lyra/hir/dump.hpp"

#include <cstddef>
#include <format>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/hir/expr.hpp"
#include "lyra/hir/lvalue.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/value_decl_ref.hpp"
#include "lyra/hir/var_decl.hpp"
#include "lyra/support/internal_error.hpp"
#include "lyra/support/overloaded.hpp"

namespace lyra::hir {

namespace {

class HirDumper {
 public:
  auto Dump(const std::vector<ModuleUnit>& units) -> std::string {
    for (const auto& u : units) {
      DumpUnit(u);
    }
    return std::move(out_);
  }

 private:
  void Line(std::string_view text) {
    out_.append(static_cast<std::size_t>(indent_) * 2, ' ');
    out_.append(text);
    out_.push_back('\n');
  }
  void Indent() {
    ++indent_;
  }
  void Dedent() {
    if (indent_ <= 0) {
      throw support::InternalError("HirDumper: indent underflow");
    }
    --indent_;
  }

  static auto FormatType(const Type& t) -> std::string {
    return std::visit(
        support::Overloaded{
            [](const BuiltinIntType&) -> std::string {
              return "BuiltinIntType";
            },
            [](const BuiltinLogicType&) -> std::string {
              return "BuiltinLogicType";
            },
        },
        t.data);
  }

  static auto FormatVarRef(const VarDeclRef& r) -> std::string {
    return std::format(
        "VarDecl#{}(hops={})", r.local_id.value, r.parent_scope_hops.value);
  }

  static auto FormatValueRef(const ValueDeclRef& v) -> std::string {
    return std::visit(
        support::Overloaded{
            [](const VarDeclRef& r) { return FormatVarRef(r); },
        },
        v);
  }

  static auto FormatLvalue(const Lvalue& l) -> std::string {
    return std::visit(
        support::Overloaded{
            [](const LocalValueRef& r) { return FormatValueRef(r.target); },
        },
        l);
  }

  static auto FormatExprData(const ExprData& data) -> std::string {
    return std::visit(
        support::Overloaded{
            [](const Primary& pr) -> std::string {
              return std::visit(
                  support::Overloaded{
                      [](const IntegerLiteral& lit) -> std::string {
                        return std::format("IntegerLiteral({})", lit.value);
                      },
                      [](const LocalValueRef& r) -> std::string {
                        return FormatValueRef(r.target);
                      },
                  },
                  pr);
            },
        },
        data);
  }

  static auto FormatProcExpr(const Process& p, ExprId id) -> std::string {
    return FormatExprData(p.exprs.at(id.value).data);
  }

  static auto FormatScopeExpr(const StructuralScope& s, ExprId id)
      -> std::string {
    return FormatExprData(s.GetExpr(id).data);
  }

  void DumpUnit(const ModuleUnit& u) {
    Line(std::format("ModuleUnit \"{}\"", u.Name()));
    Indent();

    Line("Types:");
    Indent();
    for (std::size_t i = 0; i < u.Types().size(); ++i) {
      Line(std::format("[{}] {}", i, FormatType(u.Types()[i])));
    }
    Dedent();

    Line("Root:");
    Indent();
    DumpScope(u.RootScope());
    Dedent();

    Dedent();
  }

  void DumpScope(const StructuralScope& s) {
    Line("Scope:");
    Indent();
    for (std::size_t i = 0; i < s.VarDecls().size(); ++i) {
      const auto& v = s.VarDecls()[i];
      Line(
          std::format(
              "VarDecl[{}] \"{}\" : Type[{}]", i, v.name, v.type.value));
    }
    for (const auto& p : s.Processes()) {
      DumpProcess(p);
    }
    for (const auto& g : s.Generates()) {
      DumpGenerate(s, g);
    }
    Dedent();
  }

  void DumpProcess(const Process& p) {
    std::visit(
        support::Overloaded{
            [&](const Initial& init) {
              Line("Process (Initial)");
              Indent();
              DumpStmt(p, init.body);
              Dedent();
            },
        },
        p.data);
  }

  void DumpStmt(const Process& p, StmtId id) {
    const auto& s = p.stmts.at(id.value);
    std::visit(
        support::Overloaded{
            [&](const BlockingAssignment& a) {
              Line(
                  std::format(
                      "Stmt[{}] BlockingAssignment target={} value=Expr[{}]",
                      id.value, FormatLvalue(a.target), a.value.value));
              Indent();
              Line(
                  std::format(
                      "Expr[{}] {}", a.value.value,
                      FormatProcExpr(p, a.value)));
              Dedent();
            },
            [&](const BlockStmt& b) {
              Line(
                  std::format(
                      "Stmt[{}] BlockStmt (count={})", id.value,
                      b.statements.size()));
              Indent();
              for (const auto child : b.statements) {
                DumpStmt(p, child);
              }
              Dedent();
            },
        },
        s.data);
  }

  void DumpGenerate(const StructuralScope& owner, const Generate& g) {
    std::visit(
        support::Overloaded{
            [&](const IfGenerate& ig) {
              Line(
                  std::format(
                      "Generate IfGenerate cond={}",
                      FormatScopeExpr(owner, ig.condition)));
              Indent();
              Line("then_scope:");
              Indent();
              DumpScope(g.child_scopes.at(ig.then_scope.value));
              Dedent();
              if (ig.else_scope.has_value()) {
                Line("else_scope:");
                Indent();
                DumpScope(g.child_scopes.at(ig.else_scope->value));
                Dedent();
              } else {
                Line("else_scope: <none>");
              }
              Dedent();
            },
            [&](const CaseGenerate& cg) {
              Line(
                  std::format(
                      "Generate CaseGenerate cond={}",
                      FormatScopeExpr(owner, cg.condition)));
              Indent();
              for (std::size_t i = 0; i < cg.items.size(); ++i) {
                const auto& item = cg.items[i];
                std::string labels;
                for (std::size_t j = 0; j < item.labels.size(); ++j) {
                  if (j != 0) {
                    labels += ", ";
                  }
                  labels += FormatScopeExpr(owner, item.labels[j]);
                }
                Line(std::format("item[{}] labels=[{}]", i, labels));
                Indent();
                DumpScope(g.child_scopes.at(item.scope.value));
                Dedent();
              }
              if (cg.default_scope.has_value()) {
                Line("default_scope:");
                Indent();
                DumpScope(g.child_scopes.at(cg.default_scope->value));
                Dedent();
              } else {
                Line("default_scope: <none>");
              }
              Dedent();
            },
        },
        g.data);
  }

  std::string out_;
  int indent_ = 0;
};

}  // namespace

auto DumpHir(const std::vector<ModuleUnit>& units) -> std::string {
  HirDumper dumper;
  return dumper.Dump(units);
}

}  // namespace lyra::hir
