#include "lyra/backend/cpp/render_print.hpp"

#include <cstdint>
#include <format>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/backend/cpp/render_context.hpp"
#include "lyra/backend/cpp/render_expr.hpp"
#include "lyra/backend/cpp/string_literal.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/kind.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/runtime_print.hpp"
#include "lyra/mir/type.hpp"

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

auto RenderFormatSpecInit(const mir::FormatSpec& spec) -> std::string {
  return std::format(
      "lyra::runtime::FormatSpec{{.kind = {}, .width = {}, .precision = {}, "
      ".zero_pad = {}, .left_align = {}, .timeunit_power = {}}}",
      RenderRuntimeFormatKind(spec.kind), spec.modifiers.width,
      spec.modifiers.precision, BoolLiteral(spec.modifiers.zero_pad),
      BoolLiteral(spec.modifiers.left_align), spec.timeunit_power);
}

auto RenderRuntimeValueViewInit(
    const RenderContext& ctx, const mir::RuntimePrintValue& v)
    -> diag::Result<std::string> {
  const auto& type = ctx.Unit().GetType(v.type);

  // `%s` operands are typed as packed bit vectors by slang but reach the
  // runtime as a string_view. Dispatch on format kind, not just type.
  if (v.spec.kind == mir::FormatKind::kString) {
    auto operand_or = RenderExprAsNative(ctx, ctx.Expr(v.value));
    if (!operand_or) return std::unexpected(std::move(operand_or.error()));
    return std::format(
        "lyra::runtime::RuntimeValueView::String({})", *operand_or);
  }

  if (type.IsPackedArray()) {
    const auto& pa = type.AsPackedArray();
    const std::uint64_t bit_width = pa.BitWidth();
    const bool is_signed = pa.signedness == mir::Signedness::kSigned;

    if (pa.form == mir::PackedArrayForm::kInt ||
        pa.form == mir::PackedArrayForm::kInteger) {
      auto operand_or = RenderExprAsNative(ctx, ctx.Expr(v.value));
      if (!operand_or) return std::unexpected(std::move(operand_or.error()));
      return std::format(
          "lyra::runtime::RuntimeValueView::NarrowIntegral("
          "static_cast<std::uint64_t>({}), {}, {})",
          *operand_or, bit_width, BoolLiteral(is_signed));
    }

    if (pa.form == mir::PackedArrayForm::kExplicit) {
      auto view_or = RenderPackedExprAsView(ctx, ctx.Expr(v.value));
      if (!view_or) return std::unexpected(std::move(view_or.error()));
      if (pa.atom == mir::BitAtom::kBit) {
        return std::format(
            "lyra::runtime::RuntimeValueView::FromBitView({}, {})", *view_or,
            BoolLiteral(is_signed));
      }
      return std::format(
          "lyra::runtime::RuntimeValueView::FromLogicView({}, {})", *view_or,
          BoolLiteral(is_signed));
    }
    throw InternalError(
        "RenderRuntimeValueViewInit: unsupported PackedArrayForm");
  }
  if (type.Kind() == mir::TypeKind::kString) {
    auto operand_or = RenderExprAsNative(ctx, ctx.Expr(v.value));
    if (!operand_or) return std::unexpected(std::move(operand_or.error()));
    return std::format(
        "lyra::runtime::RuntimeValueView::String({})", *operand_or);
  }
  throw InternalError(
      "RenderRuntimeValueViewInit: unsupported display operand type");
}

auto RenderPrintItemInit(
    const RenderContext& ctx, const mir::RuntimePrintItem& item)
    -> diag::Result<std::string> {
  return std::visit(
      Overloaded{
          [&](const mir::RuntimePrintLiteral& lit)
              -> diag::Result<std::string> {
            return std::format(
                "lyra::runtime::PrintLiteralItem{{{}, {}}}",
                RenderCStringLiteral(lit.text), lit.text.size());
          },
          [&](const mir::RuntimePrintValue& v) -> diag::Result<std::string> {
            auto view_or = RenderRuntimeValueViewInit(ctx, v);
            if (!view_or) return std::unexpected(std::move(view_or.error()));
            return std::format(
                "lyra::runtime::PrintValueItem{{{}, {}}}",
                RenderFormatSpecInit(v.spec), *view_or);
          },
      },
      item);
}

}  // namespace

auto RenderRuntimeCallExpr(
    const RenderContext& ctx, const mir::RuntimeCallExpr& expr)
    -> diag::Result<std::string> {
  const mir::RuntimePrintCall& call = expr.print;
  if (call.descriptor.has_value()) {
    return diag::Unsupported(
        diag::DiagCode::kCppEmitExpressionFormNotImplemented,
        "file-output runtime print is not yet implemented in cpp emit",
        diag::UnsupportedCategory::kFeature);
  }
  const std::string_view kind_literal = RenderRuntimePrintKind(call.kind);

  // Empty items: pass an explicit empty span. Avoids relying on the
  // `std::array<T, 0>` to `std::span<const T>` conversion path, which is
  // legal but a fragile spelling to depend on.
  if (call.items.empty()) {
    return std::format(
        "lyra::runtime::LyraPrint(*services_, {}, "
        "std::span<const lyra::runtime::PrintItem>{{}})",
        kind_literal);
  }

  // The PrintItem array is built inline via `std::array<PrintItem, N>{...}`;
  // each element is one of the variant alternatives (PrintLiteralItem /
  // PrintValueItem) and the variant's converting constructor wraps it. The
  // array rvalue lives for the full expression, covering the LyraPrint call.
  std::vector<std::string> item_inits;
  item_inits.reserve(call.items.size());
  for (const mir::RuntimePrintItem& item : call.items) {
    auto init_or = RenderPrintItemInit(ctx, item);
    if (!init_or) return std::unexpected(std::move(init_or.error()));
    item_inits.push_back(*std::move(init_or));
  }

  std::string out = std::format(
      "lyra::runtime::LyraPrint(*services_, {}, "
      "std::array<lyra::runtime::PrintItem, {}>{{",
      kind_literal, call.items.size());
  for (std::size_t i = 0; i < item_inits.size(); ++i) {
    if (i != 0) {
      out += ", ";
    }
    out += item_inits[i];
  }
  out += "})";
  return out;
}

}  // namespace lyra::backend::cpp
