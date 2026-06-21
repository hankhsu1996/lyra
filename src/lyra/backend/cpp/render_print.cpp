#include "lyra/backend/cpp/render_print.hpp"

#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/backend/cpp/render_expr.hpp"
#include "lyra/backend/cpp/scope_view.hpp"
#include "lyra/backend/cpp/string_literal.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/runtime_print.hpp"
#include "lyra/mir/runtime_submit.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::backend::cpp {

namespace {

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

// Each FormatSpec field travels to the runtime as a PackedArray, the value
// model the runtime constructor converts internally. The kind-only overload
// covers the common all-default spec; the full overload spells out all six.
// LRM 21.2.1.3 / 3.14.2: `%t` scales from the enclosing scope's time unit, the
// scope class constant `kTimeUnitPower`, resolved by unqualified C++ name
// lookup -- mirrors how $time reads it, so a `%t` inside a method uses its
// declaration scope's unit.
auto RenderFormatSpecInit(const mir::FormatSpec& spec) -> std::string {
  const auto from_int = [](std::string_view e) {
    return std::format(
        "lyra::value::PackedArray::FromInt({}, 32, true, false)", e);
  };
  const std::string kind_arg =
      from_int(std::format("{}LL", static_cast<std::int64_t>(spec.kind)));
  const bool is_time = spec.kind == value::FormatKind::kTime;
  const bool all_default =
      spec.modifiers.width == -1 && spec.modifiers.precision == -1 &&
      !spec.modifiers.zero_pad && !spec.modifiers.left_align && !is_time;
  if (all_default) {
    return std::format("lyra::value::FormatSpec({})", kind_arg);
  }
  return std::format(
      "lyra::value::FormatSpec({}, {}, {}, {}, {}, {})", kind_arg,
      from_int(std::format("{}LL", spec.modifiers.width)),
      from_int(std::format("{}LL", spec.modifiers.precision)),
      from_int(spec.modifiers.zero_pad ? "1LL" : "0LL"),
      from_int(spec.modifiers.left_align ? "1LL" : "0LL"),
      from_int(is_time ? "kTimeUnitPower" : "0LL"));
}

auto RenderPrintValueArg(const ScopeView& view, const mir::RuntimePrintValue& v)
    -> diag::Result<std::string> {
  const auto& type = view.Unit().GetType(v.type);

  // The argument is type-driven: the `PrintValue` overload set selects the
  // right `Formatter<T>` from the operand's C++ type, so the backend only
  // hands it the bare operand. Any spec/operand mismatch (e.g. `%s` on a bit
  // vector) must be resolved upstream of the backend -- at HIR -> MIR via an
  // explicit ConversionExpr, or rejected.
  auto operand_or = RenderExpr(view, view.Expr(v.value));
  if (!operand_or) return std::unexpected(std::move(operand_or.error()));

  if (type.IsIntegralPacked() || type.Kind() == mir::TypeKind::kReal ||
      type.Kind() == mir::TypeKind::kRealTime ||
      type.Kind() == mir::TypeKind::kShortReal ||
      type.Kind() == mir::TypeKind::kString ||
      type.Kind() == mir::TypeKind::kUnpackedArray ||
      type.Kind() == mir::TypeKind::kDynamicArray ||
      type.Kind() == mir::TypeKind::kQueue ||
      type.Kind() == mir::TypeKind::kAssociativeArray) {
    return *operand_or;
  }
  throw InternalError("RenderPrintValueArg: unsupported display operand type");
}

auto RenderPrintItemInit(
    const ScopeView& view, const mir::RuntimePrintItem& item)
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
            auto arg_or = RenderPrintValueArg(view, v);
            if (!arg_or) return std::unexpected(std::move(arg_or.error()));
            return std::format(
                "lyra::value::PrintValueItem({}, {})", *arg_or,
                RenderFormatSpecInit(v.spec));
          },
      },
      item);
}

}  // namespace

namespace {

auto RenderRuntimePrintCall(
    const ScopeView& view, const mir::RuntimePrintCall& call)
    -> diag::Result<std::string> {
  const std::string_view kind_literal = RenderPrintKindLiteral(call.kind);

  // Descriptor-bearing calls emit LyraFPrint and prepend the descriptor
  // expression; descriptor-less calls keep the simpler LyraPrint shape.
  std::string descriptor_arg;
  std::string_view call_target = "lyra::runtime::LyraPrint";
  if (call.descriptor.has_value()) {
    auto desc_or = RenderExpr(view, view.Expr(*call.descriptor));
    if (!desc_or) return std::unexpected(std::move(desc_or.error()));
    descriptor_arg = std::format("{}, ", *desc_or);
    call_target = "lyra::runtime::LyraFPrint";
  }

  // Empty items: pass an explicit empty span. Avoids relying on the
  // `std::array<T, 0>` to `std::span<const T>` conversion path, which is
  // legal but a fragile spelling to depend on.
  if (call.items.empty()) {
    return std::format(
        "{}({}, {}, {}std::span<const lyra::value::PrintItem>{{}})",
        call_target, "self->Services()", kind_literal, descriptor_arg);
  }

  // The PrintItem array is built inline via `std::array<PrintItem, N>{...}`;
  // each element is one of the variant alternatives (PrintLiteralItem /
  // PrintValueItem) and the variant's converting constructor wraps it. The
  // array rvalue lives for the full expression, covering the LyraPrint call.
  std::vector<std::string> item_inits;
  item_inits.reserve(call.items.size());
  for (const mir::RuntimePrintItem& item : call.items) {
    auto init_or = RenderPrintItemInit(view, item);
    if (!init_or) return std::unexpected(std::move(init_or.error()));
    item_inits.push_back(*std::move(init_or));
  }

  std::string out = std::format(
      "{}({}, {}, {}std::array<lyra::value::PrintItem, {}>{{", call_target,
      "self->Services()", kind_literal, descriptor_arg, call.items.size());
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
    const ScopeView& view, const mir::RuntimeCallExpr& expr)
    -> diag::Result<std::string> {
  return std::visit(
      Overloaded{
          [&](const mir::RuntimePrintCall& pc) -> diag::Result<std::string> {
            return RenderRuntimePrintCall(view, pc);
          },
          [&](const mir::RuntimeSubmitObservedCall& sc)
              -> diag::Result<std::string> {
            auto closure_or = RenderExpr(view, view.Expr(sc.closure));
            if (!closure_or) {
              return std::unexpected(std::move(closure_or.error()));
            }
            return std::format(
                "{}SubmitObserved({}, {})", "self->", sc.site_id.value,
                *closure_or);
          },
          [&](const mir::RuntimeSubmitNbaCall& nc)
              -> diag::Result<std::string> {
            auto closure_or = RenderExpr(view, view.Expr(nc.closure));
            if (!closure_or) {
              return std::unexpected(std::move(closure_or.error()));
            }
            return std::format(
                "{}.SubmitNba({})", "self->Services()", *closure_or);
          },
          [&](const mir::RuntimeSubmitPostponedCall& pc)
              -> diag::Result<std::string> {
            // LRM 21.2.2: defer the print to the postponed region via the
            // matching $strobe-family runtime entry. The lambda captures
            // block locals and the receiver by value -- NBAs do not touch
            // the locals and the snapshot is frame-death-safe for `initial`
            // The closure captures every referenced local by value (`[=]`)
            // including `self`, so it can reach scope members and Services()
            // at fire time. LRM 21.3.2 cancellation on $fclose is the
            // file-sink entry's responsibility.
            const std::string_view capture{"[=]"};
            auto print_or = RenderRuntimePrintCall(view, pc.print);
            if (!print_or) {
              return std::unexpected(std::move(print_or.error()));
            }
            if (!pc.print.descriptor.has_value()) {
              return std::format(
                  "lyra::runtime::LyraSubmitStrobe({}, {}() {{ {}; }})",
                  "self->Services()", capture, *print_or);
            }
            auto desc_or = RenderExpr(view, view.Expr(*pc.print.descriptor));
            if (!desc_or) {
              return std::unexpected(std::move(desc_or.error()));
            }
            return std::format(
                "lyra::runtime::LyraSubmitFStrobe({}, {}, {}() {{ {}; }})",
                "self->Services()", *desc_or, capture, *print_or);
          },
          [&](const mir::RuntimeScanCall& ss) -> diag::Result<std::string> {
            auto source_or = RenderExpr(view, view.Expr(ss.source));
            if (!source_or) {
              return std::unexpected(std::move(source_or.error()));
            }
            auto format_or = RenderExpr(view, view.Expr(ss.format));
            if (!format_or) {
              return std::unexpected(std::move(format_or.error()));
            }
            // Slots render as ScanTarget variant literals. Each carries a
            // pointer to a block-local temp in the surrounding closure
            // body plus the per-slot type metadata the parser needs to
            // materialize a fresh value of the lvalue's shape.
            std::string slot_pieces;
            for (std::size_t k = 0; k < ss.slots.size(); ++k) {
              auto piece_or = std::visit(
                  Overloaded{
                      [&](const mir::IntegralScanSlot& s)
                          -> diag::Result<std::string> {
                        auto temp_or = RenderLhsExpr(view, view.Expr(s.temp));
                        if (!temp_or) {
                          return std::unexpected(std::move(temp_or.error()));
                        }
                        return std::format(
                            "lyra::runtime::IntegralScanTarget{{.dest = &{}, "
                            ".bit_width = {}U, .is_signed = {}, "
                            ".is_four_state = {}}}",
                            *temp_or, s.bit_width,
                            s.is_signed ? "true" : "false",
                            s.is_four_state ? "true" : "false");
                      },
                      [&](const mir::StringScanSlot& s)
                          -> diag::Result<std::string> {
                        auto temp_or = RenderLhsExpr(view, view.Expr(s.temp));
                        if (!temp_or) {
                          return std::unexpected(std::move(temp_or.error()));
                        }
                        return std::format(
                            "lyra::runtime::StringScanTarget{{.dest = &{}}}",
                            *temp_or);
                      }},
                  ss.slots[k]);
              if (!piece_or) {
                return std::unexpected(std::move(piece_or.error()));
              }
              if (k != 0) {
                slot_pieces += ", ";
              }
              slot_pieces += *piece_or;
            }
            switch (ss.source_kind) {
              case support::ScanSourceKind::kString:
                return std::format(
                    "lyra::runtime::LyraSScanf({}, {}, {{{}}})", *source_or,
                    *format_or, slot_pieces);
              case support::ScanSourceKind::kFile:
                return std::format(
                    "lyra::runtime::LyraFScanf({}, {}, {}, {{{}}})",
                    "self->Services()", *source_or, *format_or, slot_pieces);
            }
            throw InternalError(
                "RuntimeScanCall: unknown ScanSourceKind in render");
          },
      },
      expr.call);
}

}  // namespace lyra::backend::cpp
