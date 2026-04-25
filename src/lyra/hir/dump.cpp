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
#include "lyra/hir/module_unit.hpp"
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

  static auto FormatBitAtom(BitAtom a) -> std::string_view {
    switch (a) {
      case BitAtom::kBit:
        return "bit";
      case BitAtom::kLogic:
        return "logic";
      case BitAtom::kReg:
        return "reg";
    }
    throw support::InternalError("HirDumper::FormatBitAtom: unknown BitAtom");
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
    throw support::InternalError(
        "HirDumper::FormatPackedForm: unknown PackedArrayForm");
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
        support::Overloaded{
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
    switch (p.kind) {
      case ProcessKind::kInitial:
        Line("Process (Initial)");
        break;
    }
    Indent();
    DumpStmt(p, p.body);
    Dedent();
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
