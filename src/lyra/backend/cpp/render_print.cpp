#include "lyra/backend/cpp/render_print.hpp"

#include <format>
#include <string>
#include <utility>
#include <variant>

#include "lyra/backend/cpp/render_expr.hpp"
#include "lyra/backend/cpp/scope_view.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/mir/expr.hpp"

namespace lyra::backend::cpp {

auto RenderRuntimeCallExpr(
    const ScopeView& view, const mir::RuntimeCallExpr& expr)
    -> diag::Result<std::string> {
  return std::visit(
      Overloaded{
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
