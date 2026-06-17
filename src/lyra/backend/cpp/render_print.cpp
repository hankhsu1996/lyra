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
#include "lyra/diag/diagnostic.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/runtime_diagnostic.hpp"
#include "lyra/mir/runtime_file_io.hpp"
#include "lyra/mir/runtime_finish.hpp"
#include "lyra/mir/runtime_print.hpp"
#include "lyra/mir/runtime_submit.hpp"
#include "lyra/mir/runtime_timescale.hpp"
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
    case value::FormatKind::kTime:
      return "lyra::value::FormatKind::kTime";
  }
  throw InternalError("RenderFormatKindLiteral: unknown FormatKind");
}

auto RenderFormatSpecInit(const mir::FormatSpec& spec) -> std::string {
  // Emit only the fields that differ from value::FormatSpec's in-class
  // defaults; an omitted field designated-initializes to that same default, so
  // the spec is identical but far terser than spelling out all six.
  std::vector<std::string> fields;
  if (spec.kind != value::FormatKind::kDecimal) {
    fields.push_back(
        std::format(".kind = {}", RenderFormatKindLiteral(spec.kind)));
  }
  if (spec.modifiers.width != -1) {
    fields.push_back(std::format(".width = {}", spec.modifiers.width));
  }
  if (spec.modifiers.precision != -1) {
    fields.push_back(std::format(".precision = {}", spec.modifiers.precision));
  }
  if (spec.modifiers.zero_pad) {
    fields.emplace_back(".zero_pad = true");
  }
  if (spec.modifiers.left_align) {
    fields.emplace_back(".left_align = true");
  }
  // LRM 21.2.1.3 / 3.14.2: `%t` scales from the enclosing scope's time unit.
  // The unit is the scope class constant, resolved by unqualified C++ name
  // lookup to the lexically enclosing design element -- mirrors how $time
  // reads it, so a `%t` inside a subroutine uses its declaration scope's unit.
  if (spec.kind == value::FormatKind::kTime) {
    fields.emplace_back(".timeunit_power = kTimeUnitPower");
  }
  std::string joined;
  for (const auto& field : fields) {
    if (!joined.empty()) joined += ", ";
    joined += field;
  }
  return std::format("lyra::value::FormatSpec{{{}}}", joined);
}

auto RenderPrintValueArg(
    const RenderContext& ctx, const mir::RuntimePrintValue& v)
    -> diag::Result<std::string> {
  const auto& type = ctx.Unit().GetType(v.type);

  // The argument is type-driven: the `PrintValue` overload set selects the
  // right `Formatter<T>` from the operand's C++ type, so the backend only
  // hands it the bare operand. Any spec/operand mismatch (e.g. `%s` on a bit
  // vector) must be resolved upstream of the backend -- at HIR -> MIR via an
  // explicit ConversionExpr, or rejected.
  auto operand_or = RenderExpr(ctx, ctx.Expr(v.value));
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
            auto arg_or = RenderPrintValueArg(ctx, v);
            if (!arg_or) return std::unexpected(std::move(arg_or.error()));
            return std::format(
                "lyra::value::PrintValue({}, {})", *arg_or,
                RenderFormatSpecInit(v.spec));
          },
      },
      item);
}

}  // namespace

namespace {

auto RenderRuntimePrintCall(
    const RenderContext& ctx, const mir::RuntimePrintCall& call)
    -> diag::Result<std::string> {
  const std::string_view kind_literal = RenderPrintKindLiteral(call.kind);

  // Descriptor-bearing calls emit LyraFPrint and prepend the descriptor
  // expression; descriptor-less calls keep the simpler LyraPrint shape.
  std::string descriptor_arg;
  std::string_view call_target = "lyra::runtime::LyraPrint";
  if (call.descriptor.has_value()) {
    auto desc_or = RenderExpr(ctx, ctx.Expr(*call.descriptor));
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
    auto init_or = RenderPrintItemInit(ctx, item);
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

auto RenderRuntimeFinishCall(const mir::RuntimeFinishCall& call)
    -> std::string {
  return std::format(
      "co_await lyra::runtime::Finish(self->Services(), {})", call.level);
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
        "lyra::runtime::LyraDiagnostic({}, {}, {}, "
        "std::span<const lyra::value::PrintItem>{{}})",
        "self->Services()", sev_literal, origin_init);
  }

  std::vector<std::string> item_inits;
  item_inits.reserve(call.items.size());
  for (const mir::RuntimePrintItem& item : call.items) {
    auto init_or = RenderPrintItemInit(ctx, item);
    if (!init_or) return std::unexpected(std::move(init_or.error()));
    item_inits.push_back(*std::move(init_or));
  }

  std::string out = std::format(
      "lyra::runtime::LyraDiagnostic({}, {}, {}, "
      "std::array<lyra::value::PrintItem, {}>{{",
      "self->Services()", sev_literal, origin_init, call.items.size());
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
                "{}SubmitObserved({}, {})", "self->", sc.site_id.value,
                *closure_or);
          },
          [&](const mir::RuntimeSubmitNbaCall& nc)
              -> diag::Result<std::string> {
            auto closure_or = RenderExpr(ctx, ctx.Expr(nc.closure));
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
            // procedural locals and the receiver by value -- NBAs do not touch
            // the locals and the snapshot is frame-death-safe for `initial`
            // The closure captures every referenced local by value (`[=]`)
            // including `self`, so it can reach scope members and Services()
            // at fire time. LRM 21.3.2 cancellation on $fclose is the
            // file-sink entry's responsibility.
            const std::string_view capture{"[=]"};
            auto print_or = RenderRuntimePrintCall(ctx, pc.print);
            if (!print_or) {
              return std::unexpected(std::move(print_or.error()));
            }
            if (!pc.print.descriptor.has_value()) {
              return std::format(
                  "lyra::runtime::LyraSubmitStrobe({}, {}() {{ {}; }})",
                  "self->Services()", capture, *print_or);
            }
            auto desc_or = RenderExpr(ctx, ctx.Expr(*pc.print.descriptor));
            if (!desc_or) {
              return std::unexpected(std::move(desc_or.error()));
            }
            return std::format(
                "lyra::runtime::LyraSubmitFStrobe({}, {}, {}() {{ {}; }})",
                "self->Services()", *desc_or, capture, *print_or);
          },
          [&](const mir::RuntimeFileOpenCall& fo) -> diag::Result<std::string> {
            auto name_or = RenderExpr(ctx, ctx.Expr(fo.name));
            if (!name_or) return std::unexpected(std::move(name_or.error()));
            if (fo.mode.has_value()) {
              auto mode_or = RenderExpr(ctx, ctx.Expr(*fo.mode));
              if (!mode_or) return std::unexpected(std::move(mode_or.error()));
              return std::format(
                  "lyra::runtime::LyraFOpen({}, {}, {})", "self->Services()",
                  *name_or, *mode_or);
            }
            return std::format(
                "lyra::runtime::LyraFOpen({}, {}, std::nullopt)",
                "self->Services()", *name_or);
          },
          [&](const mir::RuntimeFileCloseCall& fc)
              -> diag::Result<std::string> {
            auto desc_or = RenderExpr(ctx, ctx.Expr(fc.descriptor));
            if (!desc_or) return std::unexpected(std::move(desc_or.error()));
            return std::format(
                "lyra::runtime::LyraFClose({}, {})", "self->Services()",
                *desc_or);
          },
          [&](const mir::RuntimeFileGetcCall& fg) -> diag::Result<std::string> {
            auto fd_or = RenderExpr(ctx, ctx.Expr(fg.fd));
            if (!fd_or) return std::unexpected(std::move(fd_or.error()));
            return std::format(
                "lyra::runtime::LyraFGetc({}, {})", "self->Services()", *fd_or);
          },
          [&](const mir::RuntimeFileUngetcCall& fu)
              -> diag::Result<std::string> {
            auto c_or = RenderExpr(ctx, ctx.Expr(fu.c));
            if (!c_or) return std::unexpected(std::move(c_or.error()));
            auto fd_or = RenderExpr(ctx, ctx.Expr(fu.fd));
            if (!fd_or) return std::unexpected(std::move(fd_or.error()));
            return std::format(
                "lyra::runtime::LyraFUngetc({}, {}, {})", "self->Services()",
                *c_or, *fd_or);
          },
          [&](const mir::RuntimeFileGetsCall& fg) -> diag::Result<std::string> {
            // str_dest is a procedural-local temp introduced by the
            // statement-position desugaring; emitting its name straight
            // binds to LyraFGets's `value::String&` formal.
            auto dest_or = RenderExpr(ctx, ctx.Expr(fg.str_dest));
            if (!dest_or) return std::unexpected(std::move(dest_or.error()));
            auto fd_or = RenderExpr(ctx, ctx.Expr(fg.fd));
            if (!fd_or) return std::unexpected(std::move(fd_or.error()));
            return std::format(
                "lyra::runtime::LyraFGets({}, {}, {})", "self->Services()",
                *dest_or, *fd_or);
          },
          [&](const mir::RuntimeFileReadCall& fr) -> diag::Result<std::string> {
            auto fd_or = RenderExpr(ctx, ctx.Expr(fr.fd));
            if (!fd_or) return std::unexpected(std::move(fd_or.error()));
            return std::visit(
                Overloaded{
                    [&](const mir::RuntimeFileReadCall::PackedTarget& it)
                        -> diag::Result<std::string> {
                      auto dest_or = RenderExpr(ctx, ctx.Expr(it.dest));
                      if (!dest_or) {
                        return std::unexpected(std::move(dest_or.error()));
                      }
                      return std::format(
                          "lyra::runtime::LyraFRead({}, {}, {})",
                          "self->Services()", *dest_or, *fd_or);
                    },
                    [&](const mir::RuntimeFileReadCall::UnpackedTarget& ut)
                        -> diag::Result<std::string> {
                      auto dest_or = RenderExpr(ctx, ctx.Expr(ut.dest));
                      if (!dest_or) {
                        return std::unexpected(std::move(dest_or.error()));
                      }
                      auto render_opt =
                          [&](const std::optional<mir::ExprId>& slot)
                          -> diag::Result<std::string> {
                        if (!slot.has_value()) {
                          return std::string{"std::optional<std::int64_t>{}"};
                        }
                        auto rendered = RenderExpr(ctx, ctx.Expr(*slot));
                        if (!rendered) {
                          return std::unexpected(std::move(rendered.error()));
                        }
                        return std::format(
                            "std::optional<std::int64_t>{{"
                            "static_cast<std::int64_t>(({}).ToInt64())}}",
                            *rendered);
                      };
                      auto start_or = render_opt(ut.start);
                      if (!start_or) {
                        return std::unexpected(std::move(start_or.error()));
                      }
                      auto count_or = render_opt(ut.count);
                      if (!count_or) {
                        return std::unexpected(std::move(count_or.error()));
                      }
                      return std::format(
                          "lyra::runtime::LyraFRead({}, {}, {}, "
                          "{}, {}, {}LL, {}LL)",
                          "self->Services()", *dest_or, *fd_or, *start_or,
                          *count_or, ut.declared_left, ut.declared_right);
                    },
                },
                fr.target);
          },
          [&](const mir::RuntimeFileSeekCall& fs) -> diag::Result<std::string> {
            auto fd_or = RenderExpr(ctx, ctx.Expr(fs.fd));
            if (!fd_or) return std::unexpected(std::move(fd_or.error()));
            auto off_or = RenderExpr(ctx, ctx.Expr(fs.offset));
            if (!off_or) return std::unexpected(std::move(off_or.error()));
            auto op_or = RenderExpr(ctx, ctx.Expr(fs.operation));
            if (!op_or) return std::unexpected(std::move(op_or.error()));
            return std::format(
                "lyra::runtime::LyraFSeek({}, {}, {}, {})", "self->Services()",
                *fd_or, *off_or, *op_or);
          },
          [&](const mir::RuntimeFileRewindCall& fr)
              -> diag::Result<std::string> {
            auto fd_or = RenderExpr(ctx, ctx.Expr(fr.fd));
            if (!fd_or) return std::unexpected(std::move(fd_or.error()));
            return std::format(
                "lyra::runtime::LyraFRewind({}, {})", "self->Services()",
                *fd_or);
          },
          [&](const mir::RuntimeFileTellCall& ft) -> diag::Result<std::string> {
            auto fd_or = RenderExpr(ctx, ctx.Expr(ft.fd));
            if (!fd_or) return std::unexpected(std::move(fd_or.error()));
            return std::format(
                "lyra::runtime::LyraFTell({}, {})", "self->Services()", *fd_or);
          },
          [&](const mir::RuntimeFileEofCall& fe) -> diag::Result<std::string> {
            auto fd_or = RenderExpr(ctx, ctx.Expr(fe.fd));
            if (!fd_or) return std::unexpected(std::move(fd_or.error()));
            return std::format(
                "lyra::runtime::LyraFEof({}, {})", "self->Services()", *fd_or);
          },
          [&](const mir::RuntimeFileErrorCall& fe)
              -> diag::Result<std::string> {
            auto fd_or = RenderExpr(ctx, ctx.Expr(fe.fd));
            if (!fd_or) return std::unexpected(std::move(fd_or.error()));
            auto dest_or = RenderExpr(ctx, ctx.Expr(fe.str_dest));
            if (!dest_or) return std::unexpected(std::move(dest_or.error()));
            return std::format(
                "lyra::runtime::LyraFError({}, {}, {})", "self->Services()",
                *fd_or, *dest_or);
          },
          [&](const mir::RuntimeFileFlushCall& ff)
              -> diag::Result<std::string> {
            if (!ff.descriptor.has_value()) {
              return std::format(
                  "lyra::runtime::LyraFFlush({}, std::nullopt)",
                  "self->Services()");
            }
            auto fd_or = RenderExpr(ctx, ctx.Expr(*ff.descriptor));
            if (!fd_or) return std::unexpected(std::move(fd_or.error()));
            return std::format(
                "lyra::runtime::LyraFFlush({}, {})", "self->Services()",
                *fd_or);
          },
          [&](const mir::RuntimeScanCall& ss) -> diag::Result<std::string> {
            auto source_or = RenderExpr(ctx, ctx.Expr(ss.source));
            if (!source_or) {
              return std::unexpected(std::move(source_or.error()));
            }
            auto format_or = RenderExpr(ctx, ctx.Expr(ss.format));
            if (!format_or) {
              return std::unexpected(std::move(format_or.error()));
            }
            // Slots render as ScanTarget variant literals. Each carries a
            // pointer to a procedural-local temp in the surrounding closure
            // body plus the per-slot type metadata the parser needs to
            // materialize a fresh value of the lvalue's shape.
            std::string slot_pieces;
            for (std::size_t k = 0; k < ss.slots.size(); ++k) {
              auto piece_or = std::visit(
                  Overloaded{
                      [&](const mir::IntegralScanSlot& s)
                          -> diag::Result<std::string> {
                        auto temp_or = RenderLhsExpr(
                            ctx, ctx.Expr(s.temp), std::string_view{});
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
                        auto temp_or = RenderLhsExpr(
                            ctx, ctx.Expr(s.temp), std::string_view{});
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
          [&](const mir::RuntimeSFormatCall& sf) -> diag::Result<std::string> {
            if (sf.items.empty()) {
              return std::format(
                  "lyra::runtime::LyraSFormat({}, "
                  "std::span<const lyra::value::PrintItem>{{}})",
                  "self->Services()");
            }
            std::vector<std::string> item_pieces;
            item_pieces.reserve(sf.items.size());
            for (const mir::RuntimePrintItem& item : sf.items) {
              auto piece_or = RenderPrintItemInit(ctx, item);
              if (!piece_or) {
                return std::unexpected(std::move(piece_or.error()));
              }
              item_pieces.push_back(*std::move(piece_or));
            }
            std::string body;
            for (std::size_t k = 0; k < item_pieces.size(); ++k) {
              if (k != 0) {
                body += ", ";
              }
              body += item_pieces[k];
            }
            return std::format(
                "lyra::runtime::LyraSFormat({}, "
                "std::array<lyra::value::PrintItem, {}>{{{{{}}}}})",
                "self->Services()", sf.items.size(), body);
          },
          [&](const mir::RuntimeTimeCall& tc) -> diag::Result<std::string> {
            // The scaling target is the enclosing scope's time unit, read
            // from its emitted `kTimeUnitPower` constant; unqualified lookup
            // resolves it to the design element that lexically contains the
            // call (LRM 20.3.1 / 3.14.2). The runtime scales against the
            // engine's design-global tick.
            switch (tc.kind) {
              case support::TimeKind::kTime:
                return std::format(
                    "lyra::runtime::SimTimeInUnit({}, kTimeUnitPower)",
                    "self->Services()");
              case support::TimeKind::kStime:
                return std::format(
                    "lyra::runtime::STimeInUnit({}, kTimeUnitPower)",
                    "self->Services()");
              case support::TimeKind::kRealtime:
                return std::format(
                    "lyra::runtime::RealTimeInUnit({}, kTimeUnitPower)",
                    "self->Services()");
            }
            throw InternalError("RenderRuntimeCallExpr: unknown TimeKind");
          },
          [&](const mir::RuntimeSetTimeFormatCall& tf)
              -> diag::Result<std::string> {
            if (!tf.args.has_value()) {
              return std::string{"self->Services().ResetTimeFormat()"};
            }
            auto units = RenderExpr(ctx, ctx.Expr(tf.args->units));
            if (!units) return std::unexpected(std::move(units.error()));
            auto precision = RenderExpr(ctx, ctx.Expr(tf.args->precision));
            if (!precision) {
              return std::unexpected(std::move(precision.error()));
            }
            auto suffix = RenderExpr(ctx, ctx.Expr(tf.args->suffix));
            if (!suffix) return std::unexpected(std::move(suffix.error()));
            auto width = RenderExpr(ctx, ctx.Expr(tf.args->min_width));
            if (!width) return std::unexpected(std::move(width.error()));
            return std::format(
                "{}.SetTimeFormat({}, {}, {}, {})", "self->Services()", *units,
                *precision, *suffix, *width);
          },
          [&](const mir::RuntimePrintTimescaleCall& pt)
              -> diag::Result<std::string> {
            // Unit and precision come from the enclosing scope class constants
            // (LRM 20.4.2, current-scope form); the name is baked at lowering.
            return std::format(
                "lyra::runtime::LyraPrintTimescale({}, {}, "
                "kTimeUnitPower, kTimePrecisionPower)",
                "self->Services()", RenderCStringLiteral(pt.scope_name));
          },
      },
      expr.call);
}

}  // namespace lyra::backend::cpp
