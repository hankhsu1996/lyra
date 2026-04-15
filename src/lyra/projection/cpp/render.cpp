#include "lyra/projection/cpp/render.hpp"

#include <format>
#include <string>
#include <variant>

#include "lyra/common/constant_arena.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/operators.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/xir/arena.hpp"
#include "lyra/xir/expression.hpp"
#include "lyra/xir/statement.hpp"

namespace lyra::projection::cpp {

namespace {

auto ToCppUnaryToken(common::UnaryOp op) -> const char* {
  switch (op) {
    case common::UnaryOp::kPlus:
      return "+";
    case common::UnaryOp::kMinus:
      return "-";
    case common::UnaryOp::kLogicalNot:
      return "!";
    case common::UnaryOp::kBitwiseNot:
      return "~";
    default:
      return "?";
  }
}

auto ToCppBinaryToken(common::BinaryOp op) -> const char* {
  switch (op) {
    case common::BinaryOp::kAdd:
      return "+";
    case common::BinaryOp::kSubtract:
      return "-";
    case common::BinaryOp::kMultiply:
      return "*";
    case common::BinaryOp::kEqual:
      return "==";
    case common::BinaryOp::kNotEqual:
      return "!=";
    case common::BinaryOp::kLogicalAnd:
      return "&&";
    case common::BinaryOp::kLogicalOr:
      return "||";
    default:
      return "?";
  }
}

// Assumes the HIR-to-XIR lowering gate has already restricted the type set
// to integral <= 32 bits. Any type reaching this function outside that set
// is a pipeline bug.
auto MapTypeToCpp(TypeId type_id, const TypeArena& type_arena) -> std::string {
  const auto& type = type_arena[type_id];
  if (type.Kind() == TypeKind::kIntegral) {
    return "int32_t";
  }
  throw common::InternalError(
      "MapTypeToCpp", std::format(
                          "unexpected type kind {} passed lowering gate",
                          static_cast<int>(type.Kind())));
}

auto FormatIntegralConstant(const IntegralConstant& ic, bool is_signed)
    -> std::string {
  if (ic.value.empty()) return "0";
  uint64_t raw = ic.value[0];
  if (is_signed) {
    return std::format("{}", static_cast<int32_t>(raw));
  }
  return std::format("{}", static_cast<uint32_t>(raw));
}

auto ProjectExpr(
    xir::ExprId expr_id, const xir::Arena& arena,
    const xir::CompilationUnit& cu, const TypeArena& type_arena,
    const ConstantArena& constant_arena) -> std::unique_ptr<ProjExpr> {
  const auto& expr = arena[expr_id];
  return std::visit(
      [&](const auto& data) -> std::unique_ptr<ProjExpr> {
        using T = std::decay_t<decltype(data)>;

        if constexpr (std::is_same_v<T, xir::ConstInt>) {
          const auto& constant = constant_arena[data.value];
          const auto& type = type_arena[expr.type];
          bool is_signed =
              type.Kind() == TypeKind::kIntegral && type.AsIntegral().is_signed;
          std::string text;
          if (auto* ic = std::get_if<IntegralConstant>(&constant.value)) {
            text = FormatIntegralConstant(*ic, is_signed);
          } else {
            text = "0";
          }
          return std::make_unique<ProjExpr>(
              ProjExprData{ProjConstant{std::move(text)}});

        } else if constexpr (std::is_same_v<T, xir::ReadVariable>) {
          return std::make_unique<ProjExpr>(
              ProjExprData{ProjVarRef{cu.variables[data.var.value].name}});

        } else if constexpr (std::is_same_v<T, xir::UnaryExpr>) {
          auto operand =
              ProjectExpr(data.operand, arena, cu, type_arena, constant_arena);
          return std::make_unique<ProjExpr>(ProjExprData{
              ProjUnary{ToCppUnaryToken(data.op), std::move(operand)}});

        } else if constexpr (std::is_same_v<T, xir::BinaryExpr>) {
          auto lhs =
              ProjectExpr(data.lhs, arena, cu, type_arena, constant_arena);
          auto rhs =
              ProjectExpr(data.rhs, arena, cu, type_arena, constant_arena);
          return std::make_unique<ProjExpr>(ProjExprData{ProjBinary{
              ToCppBinaryToken(data.op), std::move(lhs), std::move(rhs)}});
        }
      },
      expr.data);
}

auto ProjectStmt(
    xir::StmtId stmt_id, const xir::Arena& arena,
    const xir::CompilationUnit& cu, const TypeArena& type_arena,
    const ConstantArena& constant_arena) -> std::unique_ptr<ProjStmt> {
  const auto& stmt = arena[stmt_id];
  return std::visit(
      [&](const auto& data) -> std::unique_ptr<ProjStmt> {
        using T = std::decay_t<decltype(data)>;

        if constexpr (std::is_same_v<T, xir::WriteVariable>) {
          auto value =
              ProjectExpr(data.value, arena, cu, type_arena, constant_arena);
          return std::make_unique<ProjStmt>(ProjStmtData{ProjWrite{
              cu.variables[data.target.value].name, std::move(value)}});

        } else if constexpr (std::is_same_v<T, xir::Block>) {
          std::vector<std::unique_ptr<ProjStmt>> children;
          children.reserve(data.children.size());
          for (auto child_id : data.children) {
            children.push_back(
                ProjectStmt(child_id, arena, cu, type_arena, constant_arena));
          }
          return std::make_unique<ProjStmt>(
              ProjStmtData{ProjBlock{std::move(children)}});

        } else if constexpr (std::is_same_v<T, xir::IfThenElse>) {
          auto cond = ProjectExpr(
              data.condition, arena, cu, type_arena, constant_arena);
          auto then_br = ProjectStmt(
              data.then_branch, arena, cu, type_arena, constant_arena);
          std::unique_ptr<ProjStmt> else_br;
          if (data.else_branch.has_value()) {
            else_br = ProjectStmt(
                *data.else_branch, arena, cu, type_arena, constant_arena);
          }
          return std::make_unique<ProjStmt>(ProjStmtData{
              ProjIf{std::move(cond), std::move(then_br), std::move(else_br)}});
        }
      },
      stmt.data);
}

void RenderExpr(const ProjExpr& expr, std::string& out) {
  std::visit(
      [&](const auto& data) {
        using T = std::decay_t<decltype(data)>;

        if constexpr (std::is_same_v<T, ProjConstant>) {
          out += data.text;
        } else if constexpr (std::is_same_v<T, ProjVarRef>) {
          out += data.name;
        } else if constexpr (std::is_same_v<T, ProjUnary>) {
          out += "(";
          out += data.op_token;
          RenderExpr(*data.operand, out);
          out += ")";
        } else if constexpr (std::is_same_v<T, ProjBinary>) {
          out += "(";
          RenderExpr(*data.lhs, out);
          out += " ";
          out += data.op_token;
          out += " ";
          RenderExpr(*data.rhs, out);
          out += ")";
        }
      },
      expr.data);
}

void RenderStmt(const ProjStmt& stmt, std::string& out, int indent) {
  auto pad = std::string(static_cast<size_t>(indent * 2), ' ');
  std::visit(
      [&](const auto& data) {
        using T = std::decay_t<decltype(data)>;

        if constexpr (std::is_same_v<T, ProjWrite>) {
          out += pad;
          out += data.target_name;
          out += " = ";
          RenderExpr(*data.value, out);
          out += ";\n";
        } else if constexpr (std::is_same_v<T, ProjBlock>) {
          for (const auto& child : data.children) {
            RenderStmt(*child, out, indent);
          }
        } else if constexpr (std::is_same_v<T, ProjIf>) {
          out += pad;
          out += "if (";
          RenderExpr(*data.condition, out);
          out += ") {\n";
          RenderStmt(*data.then_branch, out, indent + 1);
          if (data.else_branch) {
            out += pad;
            out += "} else {\n";
            RenderStmt(*data.else_branch, out, indent + 1);
          }
          out += pad;
          out += "}\n";
        }
      },
      stmt.data);
}

}  // namespace

auto ProjectPerCU(
    const xir::CompilationUnit& cu, const TypeArena& type_arena,
    const ConstantArena& constant_arena) -> Result<PerCUProjection> {
  if (cu.processes.size() != 1) {
    return std::unexpected(
        Diagnostic::HostError(
            std::format(
                "ProjectPerCU prototype: expected 1 process, got {}",
                cu.processes.size())));
  }

  PerCUProjection proj;
  proj.class_name = cu.name;

  for (const auto& var : cu.variables) {
    proj.fields.push_back(
        ProjField{
            .type_text = MapTypeToCpp(var.type, type_arena),
            .name = var.name,
            .initializer = "0",
        });
  }

  for (const auto& proc : cu.processes) {
    auto body =
        ProjectStmt(proc.body, cu.arena, cu, type_arena, constant_arena);
    proj.processes.push_back(
        ProjProcess{
            .name = proc.name,
            .body = std::move(body),
        });
  }

  return proj;
}

auto ProjectHost(const xir::CompilationUnit& cu) -> HostProjection {
  if (cu.processes.size() != 1) {
    throw common::InternalError(
        "ProjectHost",
        std::format(
            "single-process prototype requires exactly 1 process, got {}",
            cu.processes.size()));
  }
  return HostProjection{
      .cu_header_name = cu.name + ".hpp",
      .qualified_class_name = cu.name,
      .status_field_name = "status",
      .entry_process_name = cu.processes[0].name,
  };
}

auto RenderPerCUHeader(const PerCUProjection& proj) -> std::string {
  std::string out;
  out += "#pragma once\n";
  out += "#include <cstdint>\n";
  out += "\n";
  out += std::format("struct {} {{\n", proj.class_name);

  for (const auto& field : proj.fields) {
    out += std::format(
        "  {} {} = {};\n", field.type_text, field.name, field.initializer);
  }

  if (!proj.fields.empty() && !proj.processes.empty()) {
    out += "\n";
  }

  for (const auto& proc : proj.processes) {
    out += std::format("  void {}() {{\n", proc.name);
    if (proc.body) {
      RenderStmt(*proc.body, out, 2);
    }
    out += "  }\n";
  }

  out += "};\n";

  return out;
}

auto RenderInternalEntry(const HostProjection& host) -> std::string {
  std::string out;
  out += "#pragma once\n";
  out += std::format("#include \"../{}\"\n", host.cu_header_name);
  out += "#include \"../lyra_projection_runtime.hpp\"\n";
  out += "\n";
  out += "namespace lyra::projection::generated::internal {\n";
  out += "\n";
  out += std::format(
      "inline void {}_entry(void* self) {{\n", host.qualified_class_name);
  out += std::format(
      "  static_cast<{}*>(self)->{}();\n", host.qualified_class_name,
      host.entry_process_name);
  out += "}\n";
  out += "\n";
  out += std::format(
      "inline int RunProjectedTop({}& unit) {{\n", host.qualified_class_name);
  out += std::format(
      "  return lyra::projection::cpp::RunSingleInitial("
      "&unit, &{}_entry);\n",
      host.qualified_class_name);
  out += "}\n";
  out += "\n";
  out += "}  // namespace lyra::projection::generated::internal\n";

  return out;
}

auto RenderHostEntry(const HostProjection& host) -> std::string {
  std::string out;
  out += std::format("#include \"{}\"\n", host.cu_header_name);
  out += std::format(
      "#include \".lyra_internal/{}_entry.hpp\"\n", host.qualified_class_name);
  out += "\n";
  out += "int main() {\n";
  out += std::format("  {} unit;\n", host.qualified_class_name);
  out +=
      "  int rc = lyra::projection::generated::internal::"
      "RunProjectedTop(unit);\n";
  out += "  if (rc != 0) return rc;\n";
  out += std::format("  return unit.{};\n", host.status_field_name);
  out += "}\n";

  return out;
}

}  // namespace lyra::projection::cpp
