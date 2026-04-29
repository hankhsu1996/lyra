#include "render_print.hpp"

#include <cstdint>
#include <format>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

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

auto RenderFormatSpecInit(const mir::FormatSpec& spec) -> std::string {
  return std::format(
      "lyra::runtime::FormatSpec{{.kind = {}, .width = {}, .precision = {}, "
      ".zero_pad = {}, .left_align = {}, .timeunit_power = {}}}",
      RenderRuntimeFormatKind(spec.kind), spec.modifiers.width,
      spec.modifiers.precision, BoolLiteral(spec.modifiers.zero_pad),
      BoolLiteral(spec.modifiers.left_align), spec.timeunit_power);
}

// Render a value operand into an inline `RuntimeValueView` constructor call.
// `int`/`integer` (which render as native std::int32_t) take the inline-word
// `NarrowIntegral` factory. Explicit packed `bit`/`logic`/`reg` (which render
// as runtime Bit<...>/Logic<...>) take the view-based factories so the print
// path goes through the runtime type's own `View()` API rather than casting
// the value to uint64_t.
auto RenderRuntimeValueViewInit(
    const RenderContext& ctx, const mir::RuntimePrintValue& v) -> std::string {
  const auto& type = ctx.Unit().GetType(v.type);
  const std::string operand = RenderExpr(ctx, ctx.Expr(v.value));

  if (type.IsPackedArray()) {
    const auto& pa = type.AsPackedArray();
    const std::uint64_t bit_width = pa.BitWidth();
    if (bit_width > 64) {
      throw InternalError(
          "RenderRuntimeValueViewInit: wide integrals not implemented");
    }
    const bool is_signed = pa.signedness == mir::Signedness::kSigned;

    if (pa.form == mir::PackedArrayForm::kInt ||
        pa.form == mir::PackedArrayForm::kInteger) {
      return std::format(
          "lyra::runtime::RuntimeValueView::NarrowIntegral("
          "static_cast<std::uint64_t>({}), {}, {})",
          operand, bit_width, BoolLiteral(is_signed));
    }

    if (pa.form == mir::PackedArrayForm::kExplicit) {
      const char* factory =
          pa.IsFourState() ? "lyra::runtime::RuntimeValueView::FromLogicView"
                           : "lyra::runtime::RuntimeValueView::FromBitView";
      return std::format(
          "{}({}.View(), {})", factory, operand, BoolLiteral(is_signed));
    }
    throw InternalError(
        "RenderRuntimeValueViewInit: unsupported PackedArrayForm");
  }
  if (type.Kind() == mir::TypeKind::kString) {
    return std::format("lyra::runtime::RuntimeValueView::String({})", operand);
  }
  throw InternalError(
      "RenderRuntimeValueViewInit: unsupported display operand type");
}

auto RenderPrintItemInit(
    const RenderContext& ctx, const mir::RuntimePrintItem& item)
    -> std::string {
  return std::visit(
      Overloaded{
          [&](const mir::RuntimePrintLiteral& lit) -> std::string {
            return std::format(
                "lyra::runtime::PrintLiteralItem{{{}, {}}}",
                RenderCStringLiteral(lit.text), lit.text.size());
          },
          [&](const mir::RuntimePrintValue& v) -> std::string {
            return std::format(
                "lyra::runtime::PrintValueItem{{{}, {}}}",
                RenderFormatSpecInit(v.spec),
                RenderRuntimeValueViewInit(ctx, v));
          },
      },
      item);
}

}  // namespace

auto RenderRuntimeCallExpr(
    const RenderContext& ctx, const mir::RuntimeCallExpr& expr) -> std::string {
  const mir::RuntimePrintCall& call = expr.print;
  if (call.descriptor.has_value()) {
    throw InternalError(
        "RenderRuntimeCallExpr: descriptor present but file output is not "
        "implemented");
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
    item_inits.push_back(RenderPrintItemInit(ctx, item));
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
