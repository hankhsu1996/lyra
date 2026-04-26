#include "lyra/diag/render.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <variant>

#include <fmt/color.h>
#include <fmt/core.h>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/diag/source_manager.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/support/internal_error.hpp"
#include "lyra/support/overloaded.hpp"

namespace lyra::diag {

namespace {

constexpr auto kToolStyle =
    fmt::fg(fmt::terminal_color::white) | fmt::emphasis::bold;

auto KindLabel(DiagKind kind) -> std::string_view {
  switch (kind) {
    case DiagKind::kError:
      return "error:";
    case DiagKind::kUnsupported:
      return "unsupported:";
    case DiagKind::kHostError:
      return "error:";
    case DiagKind::kWarning:
      return "warning:";
    case DiagKind::kNote:
      return "note:";
  }
  throw support::InternalError("diag::KindLabel: invalid DiagKind");
}

auto KindStyle(DiagKind kind) -> fmt::text_style {
  switch (kind) {
    case DiagKind::kError:
    case DiagKind::kUnsupported:
    case DiagKind::kHostError:
      return fmt::fg(fmt::terminal_color::bright_red) | fmt::emphasis::bold;
    case DiagKind::kWarning:
      return fmt::fg(fmt::terminal_color::bright_magenta) | fmt::emphasis::bold;
    case DiagKind::kNote:
      return fmt::fg(fmt::terminal_color::bright_cyan) | fmt::emphasis::bold;
  }
  throw support::InternalError("diag::KindStyle: invalid DiagKind");
}

auto Style(const RenderOptions& opts, fmt::text_style s) -> fmt::text_style {
  return opts.use_color ? s : fmt::text_style{};
}

struct ResolvedLine {
  std::uint32_t line_number = 0;
  std::uint32_t column = 0;
  std::string_view line_text;
};

auto ResolveLine(const SourceManager& mgr, const SourceSpan& span)
    -> std::optional<ResolvedLine> {
  if (!span.file_id) {
    return std::nullopt;
  }
  const FileInfo* file = mgr.GetFile(span.file_id);
  if (file == nullptr) {
    return std::nullopt;
  }

  const auto loc = mgr.OffsetToLineCol(span.file_id, span.begin);
  if (loc.line == 0) {
    return std::nullopt;
  }

  const std::string_view content = file->content;
  const auto& starts = file->line_starts;
  const std::uint32_t line_start = starts[loc.line - 1];
  const std::size_t line_end_pos = content.find('\n', line_start);
  const auto line_end = line_end_pos == std::string_view::npos
                            ? static_cast<std::uint32_t>(content.size())
                            : static_cast<std::uint32_t>(line_end_pos);

  return ResolvedLine{
      .line_number = loc.line,
      .column = loc.col - 1,
      .line_text = content.substr(line_start, line_end - line_start),
  };
}

auto SpanOf(const DiagSpan& diag_span) -> std::optional<SourceSpan> {
  std::optional<SourceSpan> out;
  std::visit(
      support::Overloaded{
          [&](const SourceSpan& s) { out = s; },
          [&](UnknownSpan) {},
      },
      diag_span);
  return out;
}

void AppendHeader(
    std::string& out, DiagKind kind, const DiagSpan& diag_span,
    const std::string& message, const SourceManager* mgr,
    const RenderOptions& opts, fmt::text_style msg_style) {
  std::string location;
  if (auto span = SpanOf(diag_span); span && mgr != nullptr) {
    location = FormatSourceLocation(*span, *mgr);
  }

  const auto label = KindLabel(kind);
  const auto label_style = Style(opts, KindStyle(kind));
  const auto applied_msg_style = Style(opts, msg_style);

  if (location.empty()) {
    out += fmt::format(
        "{}: {} {}\n", fmt::styled("lyra", Style(opts, kToolStyle)),
        fmt::styled(label, label_style),
        fmt::styled(message, applied_msg_style));
  } else {
    out += fmt::format(
        "{}: {} {}\n", fmt::styled(location, Style(opts, fmt::emphasis::bold)),
        fmt::styled(label, label_style),
        fmt::styled(message, applied_msg_style));
  }
}

void AppendSnippet(
    std::string& out, SourceSpan span, const SourceManager& mgr,
    const RenderOptions& opts) {
  auto resolved = ResolveLine(mgr, span);
  if (!resolved) {
    return;
  }

  const auto line_end_offset =
      resolved->column + static_cast<std::uint32_t>(resolved->line_text.size());
  auto span_end = span.end > span.begin ? span.end : span.begin + 1;
  span_end =
      std::min(span_end, span.begin - resolved->column + line_end_offset);
  std::uint32_t marker_width = span_end - span.begin;
  if (marker_width == 0) {
    marker_width = 1;
  }

  const auto gutter_style = Style(opts, fmt::fg(fmt::terminal_color::white));
  const auto marker_style = Style(opts, fmt::fg(fmt::terminal_color::green));

  std::string line_num_str = std::to_string(resolved->line_number);
  std::size_t field_width = std::max(line_num_str.size(), std::size_t{4});
  std::string num_field(field_width - line_num_str.size(), ' ');
  num_field += line_num_str;
  std::string blank_field(field_width, ' ');

  out += fmt::format(
      " {} {}\n", fmt::styled(num_field + " |", gutter_style),
      resolved->line_text);
  std::string marker = "^" + std::string(marker_width - 1, '~');
  out += fmt::format(
      " {} {}{}\n", fmt::styled(blank_field + " |", gutter_style),
      std::string(resolved->column, ' '), fmt::styled(marker, marker_style));
}

void AppendPrimary(
    std::string& out, const PrimaryDiagItem& item, const SourceManager* mgr,
    const RenderOptions& opts) {
  AppendHeader(
      out, item.kind, item.span, item.message, mgr, opts, fmt::emphasis::bold);
  if (!opts.show_source_snippet || mgr == nullptr) {
    return;
  }
  if (auto span = SpanOf(item.span)) {
    AppendSnippet(out, *span, *mgr, opts);
  }
}

void AppendNote(
    std::string& out, const NoteDiagItem& note, const SourceManager* mgr,
    const RenderOptions& opts) {
  AppendHeader(
      out, DiagKind::kNote, note.span, note.message, mgr, opts,
      fmt::text_style{});
}

}  // namespace

auto RenderDiagnostic(
    const Diagnostic& diag, const SourceManager* source_manager,
    const RenderOptions& opts) -> std::string {
  std::string out;
  AppendPrimary(out, diag.primary, source_manager, opts);
  for (const auto& note : diag.notes) {
    AppendNote(out, note, source_manager, opts);
  }
  return out;
}

auto RenderDiagnostics(
    const DiagnosticSink& sink, const SourceManager* source_manager,
    const RenderOptions& opts) -> std::string {
  std::string out;
  std::uint32_t error_count = 0;
  std::uint32_t warning_count = 0;
  for (const auto& d : sink.Diagnostics()) {
    switch (d.primary.kind) {
      case DiagKind::kError:
      case DiagKind::kUnsupported:
      case DiagKind::kHostError:
        ++error_count;
        break;
      case DiagKind::kWarning:
        ++warning_count;
        break;
      case DiagKind::kNote:
        break;
    }
    out += RenderDiagnostic(d, source_manager, opts);
  }
  if (error_count == 0 && warning_count == 0) {
    return out;
  }
  std::string summary;
  if (warning_count > 0) {
    summary += std::format(
        "{} warning{}", warning_count, warning_count == 1 ? "" : "s");
  }
  if (warning_count > 0 && error_count > 0) {
    summary += " and ";
  }
  if (error_count > 0) {
    summary +=
        std::format("{} error{}", error_count, error_count == 1 ? "" : "s");
  }
  out += std::format("{} generated.\n", summary);
  return out;
}

auto RenderInternalError(std::string_view message) -> std::string {
  return std::format("lyra: internal error: {}\n", message);
}

}  // namespace lyra::diag
