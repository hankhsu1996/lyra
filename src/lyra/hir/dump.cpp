#include "lyra/hir/dump.hpp"

#include <cstddef>
#include <format>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/local_var.hpp"
#include "lyra/hir/member_var.hpp"
#include "lyra/hir/module_unit.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/support/system_subroutine.hpp"

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
      throw InternalError("HirDumper: indent underflow");
    }
    --indent_;
  }

  static auto FormatBitAtom(BitAtom a) -> std::string_view {
    switch (a) {
      case BitAtom::kBit:
        return "bit";
      case BitAtom::kLogic:
        return "logic";
      case BitAtom::kReg:
        return "reg";
    }
    throw InternalError("HirDumper::FormatBitAtom: unknown BitAtom");
  }

  static auto FormatSignedness(Signedness s) -> std::string_view {
    return s == Signedness::kSigned ? "signed" : "unsigned";
  }

  static auto FormatPackedForm(PackedArrayForm f) -> std::string_view {
    switch (f) {
      case PackedArrayForm::kExplicit:
        return "explicit";
      case PackedArrayForm::kByte:
        return "byte";
      case PackedArrayForm::kShortInt:
        return "shortint";
      case PackedArrayForm::kInt:
        return "int";
      case PackedArrayForm::kLongInt:
        return "longint";
      case PackedArrayForm::kInteger:
        return "integer";
      case PackedArrayForm::kTime:
        return "time";
    }
    throw InternalError("HirDumper::FormatPackedForm: unknown PackedArrayForm");
  }

  static auto FormatPackedDims(const std::vector<PackedRange>& dims)
      -> std::string {
    if (dims.empty()) {
      return "[]";
    }
    std::string out;
    for (const auto& d : dims) {
      out += std::format("[{}:{}]", d.left, d.right);
    }
    return out;
  }

  static auto FormatUnpackedDims(const std::vector<UnpackedRange>& dims)
      -> std::string {
    if (dims.empty()) {
      return "[]";
    }
    std::string out;
    for (const auto& d : dims) {
      out += std::format("[{}:{}]", d.left, d.right);
    }
    return out;
  }

  static auto FormatType(const Type& t) -> std::string {
    return std::visit(
        Overloaded{
            [](const PackedArrayType& p) -> std::string {
              return std::format(
                  "PackedArray(atom={}, signed={}, dims={}, form={})",
                  FormatBitAtom(p.atom), FormatSignedness(p.signedness),
                  FormatPackedDims(p.dims), FormatPackedForm(p.form));
            },
            [](const UnpackedArrayType& u) -> std::string {
              return std::format(
                  "UnpackedArray(elem=Type[{}], dims={})", u.element_type.value,
                  FormatUnpackedDims(u.dims));
            },
            [](const DynamicArrayType& d) -> std::string {
              return std::format(
                  "DynamicArray(elem=Type[{}])", d.element_type.value);
            },
            [](const QueueType& q) -> std::string {
              if (q.max_bound.has_value()) {
                return std::format(
                    "Queue(elem=Type[{}], max={})", q.element_type.value,
                    *q.max_bound);
              }
              return std::format("Queue(elem=Type[{}])", q.element_type.value);
            },
            [](const AssociativeArrayType& a) -> std::string {
              if (a.key_type.has_value()) {
                return std::format(
                    "AssociativeArray(elem=Type[{}], key=Type[{}])",
                    a.element_type.value, a.key_type->value);
              }
              return std::format(
                  "AssociativeArray(elem=Type[{}], key=wildcard)",
                  a.element_type.value);
            },
            [](const StringType&) -> std::string { return "StringType"; },
            [](const EventType&) -> std::string { return "EventType"; },
            [](const RealType&) -> std::string { return "RealType"; },
            [](const ShortRealType&) -> std::string { return "ShortRealType"; },
            [](const RealTimeType&) -> std::string { return "RealTimeType"; },
            [](const ChandleType&) -> std::string { return "ChandleType"; },
            [](const VoidType&) -> std::string { return "VoidType"; },
        },
        t.data);
  }

  static auto FormatBinaryOp(BinaryOp op) -> std::string {
    switch (op) {
      case BinaryOp::kAdd:
        return "Add";
      case BinaryOp::kLessThan:
        return "LessThan";
    }
    throw InternalError("HirDumper: unknown BinaryOp");
  }

  static auto FormatValueRef(const ValueRef& v) -> std::string {
    return std::visit(
        Overloaded{
            [](const MemberVarRef& r) -> std::string {
              return std::format(
                  "MemberVar[{}](hops={})", r.target.value,
                  r.parent_scope_hops.value);
            },
            [](const LocalVarRef& r) -> std::string {
              return std::format("LocalVar[{}]", r.target.value);
            },
            [](const LoopVarRef& r) -> std::string {
              return std::format(
                  "LoopVar[{}](hops={})", r.target.value,
                  r.parent_scope_hops.value);
            },
        },
        v);
  }

  static auto FormatPrimary(const Primary& p) -> std::string {
    return std::visit(
        Overloaded{
            [](const IntegerLiteral& lit) -> std::string {
              return std::format("IntegerLiteral({})", lit.value);
            },
            [](const StringLiteral& lit) -> std::string {
              return std::format("StringLiteral(\"{}\")", lit.value);
            },
            [](const RefExpr& r) -> std::string {
              return std::format("RefExpr {}", FormatValueRef(r.target));
            },
        },
        p);
  }

  [[nodiscard]] auto FormatSubroutineRef(const SubroutineRef& callee) const
      -> std::string {
    return std::visit(
        Overloaded{
            [this](const UserSubroutineRef& u) -> std::string {
              const auto& owner = ResolveScope(u.parent_scope_hops);
              const auto& decl = owner.GetSubroutine(u.id);
              return std::format(
                  "UserSubroutine[{}](hops={}) \"{}\"", u.id.value,
                  u.parent_scope_hops.value, decl.name);
            },
            [](const SystemSubroutineRef& s) -> std::string {
              const auto& desc = support::LookupSystemSubroutine(s.id);
              return std::format(
                  "SystemSubroutine[{}] \"{}\"", s.id.value, desc.name);
            },
        },
        callee);
  }

  [[nodiscard]] auto ResolveScope(ParentScopeHops hops) const
      -> const StructuralScope& {
    if (hops.value >= scope_stack_.size()) {
      throw InternalError(
          "HirDumper::ResolveScope: ParentScopeHops out of range");
    }
    return *scope_stack_[scope_stack_.size() - 1 - hops.value];
  }

  [[nodiscard]] auto FormatExprData(const ExprData& data) const -> std::string {
    return std::visit(
        Overloaded{
            [](const PrimaryExpr& p) -> std::string {
              return FormatPrimary(p.data);
            },
            [](const BinaryExpr& b) -> std::string {
              return std::format(
                  "BinaryExpr op={} lhs=Expr[{}] rhs=Expr[{}] type=Type[{}]",
                  FormatBinaryOp(b.op), b.lhs.value, b.rhs.value, b.type.value);
            },
            [](const AssignExpr& a) -> std::string {
              return std::format(
                  "AssignExpr lhs=Expr[{}] rhs=Expr[{}] type=Type[{}]",
                  a.lhs.value, a.rhs.value, a.type.value);
            },
            [this](const CallExpr& c) -> std::string {
              std::string args;
              for (std::size_t i = 0; i < c.arguments.size(); ++i) {
                if (i != 0) {
                  args += ", ";
                }
                args += std::format("Expr[{}]", c.arguments[i].value);
              }
              return std::format(
                  "CallExpr callee={} args=[{}] type=Type[{}]",
                  FormatSubroutineRef(c.callee), args, c.result_type.value);
            },
        },
        data);
  }

  [[nodiscard]] auto FormatProcExpr(const Process& p, ExprId id) const
      -> std::string {
    return FormatExprData(p.exprs.at(id.value).data);
  }

  [[nodiscard]] auto FormatScopeExpr(const StructuralScope& s, ExprId id) const
      -> std::string {
    return FormatExprData(s.GetExpr(id).data);
  }

  void DumpUnit(const ModuleUnit& u) {
    Line(std::format("ModuleUnit \"{}\"", u.name));
    Indent();

    Line("Types:");
    Indent();
    for (std::size_t i = 0; i < u.types.size(); ++i) {
      Line(std::format("[{}] {}", i, FormatType(u.types[i])));
    }
    Dedent();

    Line("Root:");
    Indent();
    DumpScope(u.root_scope);
    Dedent();

    Dedent();
  }

  void DumpScope(const StructuralScope& s) {
    scope_stack_.push_back(&s);
    Line("Scope:");
    Indent();
    for (std::size_t i = 0; i < s.member_vars.size(); ++i) {
      const auto& v = s.member_vars[i];
      Line(
          std::format(
              "MemberVar[{}] \"{}\" : Type[{}]", i, v.name, v.type.value));
    }
    for (std::size_t i = 0; i < s.loop_var_decls.size(); ++i) {
      const auto& lv = s.loop_var_decls[i];
      Line(
          std::format(
              "LoopVarDecl[{}] \"{}\" : Type[{}]", i, lv.name, lv.type.value));
    }
    for (std::size_t i = 0; i < s.subroutines.size(); ++i) {
      const auto& d = s.subroutines[i];
      Line(
          std::format(
              "Subroutine[{}] {} \"{}\" : Type[{}]", i,
              d.kind == SubroutineKind::kTask ? "task" : "function", d.name,
              d.result_type.value));
    }
    if (!s.exprs.empty()) {
      Line("Exprs:");
      Indent();
      for (std::size_t i = 0; i < s.exprs.size(); ++i) {
        Line(std::format("Expr[{}] {}", i, FormatExprData(s.exprs[i].data)));
      }
      Dedent();
    }
    for (const auto& p : s.processes) {
      DumpProcess(p);
    }
    for (const auto& g : s.generates) {
      DumpGenerate(s, g);
    }
    Dedent();
    scope_stack_.pop_back();
  }

  void DumpProcess(const Process& p) {
    switch (p.kind) {
      case ProcessKind::kInitial:
        Line("Process (Initial)");
        break;
    }
    Indent();
    if (!p.local_vars.empty()) {
      Line("Locals:");
      Indent();
      for (std::size_t i = 0; i < p.local_vars.size(); ++i) {
        const auto& lv = p.local_vars[i];
        Line(
            std::format(
                "LocalVar[{}] \"{}\" : Type[{}]", i, lv.name, lv.type.value));
      }
      Dedent();
    }
    if (!p.exprs.empty()) {
      Line("Exprs:");
      Indent();
      for (std::size_t i = 0; i < p.exprs.size(); ++i) {
        Line(std::format("Expr[{}] {}", i, FormatExprData(p.exprs[i].data)));
      }
      Dedent();
    }
    DumpStmt(p, p.body);
    Dedent();
  }

  void DumpStmt(const Process& p, StmtId id) {
    const auto& s = p.stmts.at(id.value);
    std::visit(
        Overloaded{
            [&](const VarDeclStmt& v) {
              Line(
                  std::format(
                      "Stmt[{}] VarDeclStmt local=LocalVar[{}]", id.value,
                      v.local_var.value));
            },
            [&](const ExprStmt& e) {
              Line(
                  std::format(
                      "Stmt[{}] ExprStmt expr=Expr[{}]", id.value,
                      e.expr.value));
              Indent();
              Line(
                  std::format(
                      "Expr[{}] {}", e.expr.value, FormatProcExpr(p, e.expr)));
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
        Overloaded{
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
            [&](const LoopGenerate& lg) {
              Line(
                  std::format(
                      "Generate LoopGenerate loop_var=LoopVar[{}] "
                      "initial=Expr[{}] stop=Expr[{}] iter=Expr[{}]",
                      lg.loop_var.value, lg.initial.value, lg.stop.value,
                      lg.iter.value));
              Indent();
              Line("body_scope:");
              Indent();
              DumpScope(g.child_scopes.at(lg.body_scope.value));
              Dedent();
              Dedent();
            },
        },
        g.data);
  }

  std::string out_;
  int indent_ = 0;
  std::vector<const StructuralScope*> scope_stack_;
};

}  // namespace

auto DumpHir(const std::vector<ModuleUnit>& units) -> std::string {
  HirDumper dumper;
  return dumper.Dump(units);
}

}  // namespace lyra::hir
