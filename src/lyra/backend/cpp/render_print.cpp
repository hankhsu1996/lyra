#include "render_print.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <string>
#include <variant>

#include "formatting.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/runtime_print.hpp"
#include "lyra/mir/type.hpp"
#include "render_context.hpp"
#include "render_expr.hpp"
#include "string_literal.hpp"

namespace lyra::backend::cpp {

namespace {

auto BoolLiteral(bool b) -> std::string_view {
  return b ? "true" : "false";
}

auto RenderRuntimePrintKind(mir::PrintKind k) -> std::string_view {
  switch (k) {
    case mir::PrintKind::kDisplay:
      return "lyra::runtime::PrintKind::kDisplay";
    case mir::PrintKind::kWrite:
      return "lyra::runtime::PrintKind::kWrite";
    case mir::PrintKind::kFDisplay:
      return "lyra::runtime::PrintKind::kFDisplay";
    case mir::PrintKind::kFWrite:
      return "lyra::runtime::PrintKind::kFWrite";
  }
  throw InternalError("RenderRuntimePrintKind: unknown PrintKind");
}

auto RenderRuntimeFormatKind(mir::FormatKind k) -> std::string_view {
  switch (k) {
    case mir::FormatKind::kDecimal:
      return "lyra::runtime::FormatKind::kDecimal";
    case mir::FormatKind::kHex:
      return "lyra::runtime::FormatKind::kHex";
    case mir::FormatKind::kBinary:
      return "lyra::runtime::FormatKind::kBinary";
    case mir::FormatKind::kOctal:
      return "lyra::runtime::FormatKind::kOctal";
    case mir::FormatKind::kString:
      return "lyra::runtime::FormatKind::kString";
  }
  throw InternalError("RenderRuntimeFormatKind: unknown FormatKind");
}

auto RenderRuntimeFormatSpecInit(const mir::FormatSpec& spec) -> std::string {
  return std::format(
      "lyra::runtime::FormatSpec{{.kind = {}, .width = {}, .precision = {}, "
      ".zero_pad = {}, .left_align = {}, .timeunit_power = {}}}",
      RenderRuntimeFormatKind(spec.kind), spec.modifiers.width,
      spec.modifiers.precision, BoolLiteral(spec.modifiers.zero_pad),
      BoolLiteral(spec.modifiers.left_align), spec.timeunit_power);
}

auto RenderRuntimePrintLiteral(
    const mir::RuntimePrintLiteral& lit, std::size_t indent) -> std::string {
  return Indent(indent) + "lyra::runtime::LyraPrintLiteral(*engine_, " +
         RenderCStringLiteral(lit.text) + ", " +
         std::format("{}", lit.text.size()) + ");\n";
}

auto RenderRuntimePrintValue(
    const RenderContext& ctx, const mir::RuntimePrintValue& v,
    std::size_t indent) -> std::string {
  const std::string word_name = ctx.AllocateTemp("lyra_print_word");
  const std::string spec_name = ctx.AllocateTemp("lyra_print_spec");
  const std::string view_name = ctx.AllocateTemp("lyra_print_value");

  const auto& type = ctx.Unit().GetType(v.type);
  std::string out;
  out += Indent(indent) + "{\n";

  if (type.IsPackedArray()) {
    const auto& pa = type.AsPackedArray();
    const std::uint64_t bit_width = pa.BitWidth();
    if (bit_width > 64) {
      throw InternalError(
          "RenderRuntimePrintValue: wide integrals not implemented");
    }
    const bool is_signed = pa.signedness == mir::Signedness::kSigned;

    out += Indent(indent + 1) + "const std::uint64_t " + word_name +
           " = static_cast<std::uint64_t>(" +
           RenderExpr(ctx, ctx.Expr(v.value)) + ");\n";
    out += Indent(indent + 1) + "const lyra::runtime::FormatSpec " + spec_name +
           " = " + RenderRuntimeFormatSpecInit(v.spec) + ";\n";
    out += Indent(indent + 1) + "const auto " + view_name +
           " = lyra::runtime::RuntimeValueView::Integral(&" + word_name +
           ", nullptr, 1, " + std::format("{}", bit_width) + ", " +
           std::string{BoolLiteral(is_signed)} + ");\n";
  } else if (type.Kind() == mir::TypeKind::kString) {
    out += Indent(indent + 1) + "const std::string " + word_name + " = " +
           RenderExpr(ctx, ctx.Expr(v.value)) + ";\n";
    out += Indent(indent + 1) + "const lyra::runtime::FormatSpec " + spec_name +
           " = " + RenderRuntimeFormatSpecInit(v.spec) + ";\n";
    out += Indent(indent + 1) + "const auto " + view_name +
           " = lyra::runtime::RuntimeValueView::String(" + word_name +
           ".data(), " + word_name + ".size());\n";
  } else {
    throw InternalError(
        "RenderRuntimePrintValue: unsupported display operand type");
  }

  out += Indent(indent + 1) + "lyra::runtime::LyraPrintValue(*engine_, &" +
         spec_name + ", &" + view_name + ");\n";
  out += Indent(indent) + "}\n";
  return out;
}

auto RenderRuntimePrintItem(
    const RenderContext& ctx, const mir::RuntimePrintItem& item,
    std::size_t indent) -> std::string {
  return std::visit(
      Overloaded{
          [&](const mir::RuntimePrintLiteral& lit) -> std::string {
            return RenderRuntimePrintLiteral(lit, indent);
          },
          [&](const mir::RuntimePrintValue& v) -> std::string {
            return RenderRuntimePrintValue(ctx, v, indent);
          },
      },
      item);
}

}  // namespace

auto RenderRuntimePrintSeqStmt(
    const RenderContext& ctx, const mir::RuntimePrintSeqStmt& stmt,
    std::size_t indent) -> std::string {
  if (stmt.descriptor.has_value()) {
    throw InternalError(
        "RenderRuntimePrintSeqStmt: descriptor present but file output is "
        "not implemented");
  }
  const std::string kind_literal{RenderRuntimePrintKind(stmt.kind)};

  std::string out;
  out += Indent(indent) + "lyra::runtime::LyraPrintStart(*engine_, " +
         kind_literal + ");\n";
  for (const mir::RuntimePrintItem& item : stmt.items) {
    out += RenderRuntimePrintItem(ctx, item, indent);
  }
  out += Indent(indent) + "lyra::runtime::LyraPrintEnd(*engine_, " +
         kind_literal + ");\n";
  return out;
}

}  // namespace lyra::backend::cpp
