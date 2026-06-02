#include "lyra/backend/cpp/render_print.hpp"

#include <format>
#include <optional>
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
#include "lyra/mir/runtime_diagnostic.hpp"
#include "lyra/mir/runtime_finish.hpp"
#include "lyra/mir/runtime_print.hpp"
#include "lyra/mir/runtime_submit.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::backend::cpp {

namespace {

auto BoolLiteral(bool b) -> std::string_view {
  return b ? "true" : "false";
}

auto RenderPrintKindLiteral(value::PrintKind k) -> std::string_view {
  switch (k) {
    case value::PrintKind::kDisplay:
      return "lyra::value::PrintKind::kDisplay";
    case value::PrintKind::kWrite:
      return "lyra::value::PrintKind::kWrite";
    case value::PrintKind::kFDisplay:
      return "lyra::value::PrintKind::kFDisplay";
    case value::PrintKind::kFWrite:
      return "lyra::value::PrintKind::kFWrite";
  }
  throw InternalError("RenderPrintKindLiteral: unknown PrintKind");
}

auto RenderFormatKindLiteral(value::FormatKind k) -> std::string_view {
  switch (k) {
    case value::FormatKind::kDecimal:
      return "lyra::value::FormatKind::kDecimal";
    case value::FormatKind::kHex:
      return "lyra::value::FormatKind::kHex";
    case value::FormatKind::kBinary:
      return "lyra::value::FormatKind::kBinary";
    case value::FormatKind::kOctal:
      return "lyra::value::FormatKind::kOctal";
    case value::FormatKind::kString:
      return "lyra::value::FormatKind::kString";
    case value::FormatKind::kChar:
      return "lyra::value::FormatKind::kChar";
    case value::FormatKind::kRealDecimal:
      return "lyra::value::FormatKind::kRealDecimal";
    case value::FormatKind::kRealExponential:
      return "lyra::value::FormatKind::kRealExponential";
    case value::FormatKind::kRealGeneral:
      return "lyra::value::FormatKind::kRealGeneral";
    case value::FormatKind::kAssignmentPattern:
      return "lyra::value::FormatKind::kAssignmentPattern";
  }
  throw InternalError("RenderFormatKindLiteral: unknown FormatKind");
}

auto RenderFormatSpecInit(const mir::FormatSpec& spec) -> std::string {
  return std::format(
      "lyra::value::FormatSpec{{.kind = {}, .width = {}, .precision = {}, "
      ".zero_pad = {}, .left_align = {}, .timeunit_power = {}}}",
      RenderFormatKindLiteral(spec.kind), spec.modifiers.width,
      spec.modifiers.precision, BoolLiteral(spec.modifiers.zero_pad),
      BoolLiteral(spec.modifiers.left_align), spec.timeunit_power);
}

auto RenderRuntimeValueViewInit(
    const RenderContext& ctx, const mir::RuntimePrintValue& v)
    -> diag::Result<std::string> {
  const auto& type = ctx.Unit().GetType(v.type);

  // The view init is purely type-driven: a RuntimePrintValue's operand type
  // determines which RuntimeValueView constructor to call. The format spec
  // is forwarded unchanged to the runtime, which interprets it within the
  // operand-type-specific formatter. Any spec/operand mismatch (e.g. `%s`
  // on a bit vector) must be resolved upstream of the backend -- either at
  // HIR -> MIR via an explicit ConversionExpr, or rejected as unsupported.

  if (type.IsIntegralPacked()) {
    auto operand_or = RenderExpr(ctx, ctx.Expr(v.value));
    if (!operand_or) return std::unexpected(std::move(operand_or.error()));
    return std::format(
        "lyra::value::RuntimeValueView::FromPackedArray({})", *operand_or);
  }
  if (type.Kind() == mir::TypeKind::kString) {
    auto operand_or = RenderExpr(ctx, ctx.Expr(v.value));
    if (!operand_or) return std::unexpected(std::move(operand_or.error()));
    return std::format(
        "lyra::value::RuntimeValueView::String(({}).View())", *operand_or);
  }
  if (type.Kind() == mir::TypeKind::kReal ||
      type.Kind() == mir::TypeKind::kRealTime) {
    auto operand_or = RenderExpr(ctx, ctx.Expr(v.value));
    if (!operand_or) return std::unexpected(std::move(operand_or.error()));
    return std::format(
        "lyra::value::RuntimeValueView::Real64({})", *operand_or);
  }
  if (type.Kind() == mir::TypeKind::kShortReal) {
    auto operand_or = RenderExpr(ctx, ctx.Expr(v.value));
    if (!operand_or) return std::unexpected(std::move(operand_or.error()));
    return std::format(
        "lyra::value::RuntimeValueView::Real32({})", *operand_or);
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
                "lyra::value::PrintLiteralItem{{{}, {}}}",
                RenderCStringLiteral(lit.text), lit.text.size());
          },
          [&](const mir::RuntimePrintValue& v) -> diag::Result<std::string> {
            auto view_or = RenderRuntimeValueViewInit(ctx, v);
            if (!view_or) return std::unexpected(std::move(view_or.error()));
            return std::format(
                "lyra::value::PrintValueItem{{{}, {}}}",
                RenderFormatSpecInit(v.spec), *view_or);
          },
      },
      item);
}

}  // namespace

namespace {

auto RenderRuntimePrintCall(
    const RenderContext& ctx, const mir::RuntimePrintCall& call)
    -> diag::Result<std::string> {
  if (call.descriptor.has_value()) {
    return diag::Unsupported(
        diag::DiagCode::kCppEmitExpressionFormNotImplemented,
        "file-output runtime print is not yet implemented in cpp emit",
        diag::UnsupportedCategory::kFeature);
  }
  const std::string_view kind_literal = RenderPrintKindLiteral(call.kind);

  // Empty items: pass an explicit empty span. Avoids relying on the
  // `std::array<T, 0>` to `std::span<const T>` conversion path, which is
  // legal but a fragile spelling to depend on.
  if (call.items.empty()) {
    return std::format(
        "lyra::runtime::LyraPrint(*services_, {}, "
        "std::span<const lyra::value::PrintItem>{{}})",
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
      "std::array<lyra::value::PrintItem, {}>{{",
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

auto RenderRuntimeFinishCall(const mir::RuntimeFinishCall& call)
    -> std::string {
  return std::format(
      "co_await lyra::runtime::Finish(*services_, {})", call.level);
}

auto RenderRuntimeDiagnosticSeverity(mir::DiagnosticSeverity s)
    -> std::string_view {
  switch (s) {
    case mir::DiagnosticSeverity::kInfo:
      return "lyra::runtime::Severity::kInfo";
    case mir::DiagnosticSeverity::kWarning:
      return "lyra::runtime::Severity::kWarning";
    case mir::DiagnosticSeverity::kError:
      return "lyra::runtime::Severity::kError";
  }
  throw InternalError("RenderRuntimeDiagnosticSeverity: unknown severity");
}

auto RenderDiagnosticOriginInit(
    const std::optional<mir::DiagnosticOrigin>& origin) -> std::string {
  if (!origin.has_value()) {
    return "std::optional<lyra::runtime::SourceLocation>{}";
  }
  return std::format(
      "std::optional<lyra::runtime::SourceLocation>{{lyra::runtime::"
      "SourceLocation{{.file = {}, .line = {}, .col = {}}}}}",
      RenderCStringLiteral(origin->file), origin->line, origin->col);
}

auto RenderRuntimeDiagnosticCall(
    const RenderContext& ctx, const mir::RuntimeDiagnosticCall& call)
    -> diag::Result<std::string> {
  const std::string_view sev_literal =
      RenderRuntimeDiagnosticSeverity(call.severity);
  const std::string origin_init = RenderDiagnosticOriginInit(call.origin);

  if (call.items.empty()) {
    return std::format(
        "lyra::runtime::LyraDiagnostic(*services_, {}, {}, "
        "std::span<const lyra::value::PrintItem>{{}})",
        sev_literal, origin_init);
  }

  std::vector<std::string> item_inits;
  item_inits.reserve(call.items.size());
  for (const mir::RuntimePrintItem& item : call.items) {
    auto init_or = RenderPrintItemInit(ctx, item);
    if (!init_or) return std::unexpected(std::move(init_or.error()));
    item_inits.push_back(*std::move(init_or));
  }

  std::string out = std::format(
      "lyra::runtime::LyraDiagnostic(*services_, {}, {}, "
      "std::array<lyra::value::PrintItem, {}>{{",
      sev_literal, origin_init, call.items.size());
  for (std::size_t i = 0; i < item_inits.size(); ++i) {
    if (i != 0) {
      out += ", ";
    }
    out += item_inits[i];
  }
  out += "})";
  return out;
}

}  // namespace

auto RenderRuntimeCallExpr(
    const RenderContext& ctx, const mir::RuntimeCallExpr& expr)
    -> diag::Result<std::string> {
  return std::visit(
      Overloaded{
          [&](const mir::RuntimePrintCall& pc) -> diag::Result<std::string> {
            return RenderRuntimePrintCall(ctx, pc);
          },
          [&](const mir::RuntimeDiagnosticCall& dc)
              -> diag::Result<std::string> {
            return RenderRuntimeDiagnosticCall(ctx, dc);
          },
          [&](const mir::RuntimeFinishCall& fc) -> diag::Result<std::string> {
            return RenderRuntimeFinishCall(fc);
          },
          [&](const mir::RuntimeSubmitObservedCall& sc)
              -> diag::Result<std::string> {
            auto closure_or = RenderExpr(ctx, ctx.Expr(sc.closure));
            if (!closure_or) {
              return std::unexpected(std::move(closure_or.error()));
            }
            return std::format(
                "this->SubmitObserved({}, {})", sc.site_id.value, *closure_or);
          },
          [&](const mir::RuntimeSubmitNbaCall& nc)
              -> diag::Result<std::string> {
            auto closure_or = RenderExpr(ctx, ctx.Expr(nc.closure));
            if (!closure_or) {
              return std::unexpected(std::move(closure_or.error()));
            }
            return std::format("services_->SubmitNba({})", *closure_or);
          },
      },
      expr.call);
}

}  // namespace lyra::backend::cpp
