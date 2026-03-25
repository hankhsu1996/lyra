#include "render_diagnostics.hpp"

#include <algorithm>
#include <cstdint>
#include <optional>
#include <string>
#include <variant>

#include <fmt/color.h>
#include <fmt/core.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/source_span.hpp"
#include "text_sink.hpp"

namespace lyra::driver {

namespace {

constexpr auto kToolColor = fmt::terminal_color::white;
constexpr auto kToolStyle = fmt::fg(kToolColor) | fmt::emphasis::bold;

auto DiagKindToString(DiagKind kind) -> const char* {
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
  throw common::InternalError("DiagKindToString", "invalid diagnostic kind");
}

auto DiagKindToStyle(DiagKind kind) -> fmt::text_style {
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
  throw common::InternalError("DiagKindToStyle", "invalid diagnostic kind");
}

struct ResolvedSourceLine {
  uint32_t line_number = 0;
  uint32_t column = 0;
  std::string_view line_text;
};

auto ResolveSourceLine(
    const SourceManager& source_manager, const SourceSpan& span)
    -> std::optional<ResolvedSourceLine> {
  if (!span.file_id) return std::nullopt;

  const FileInfo* file = source_manager.GetFile(span.file_id);
  if (file == nullptr) return std::nullopt;

  const std::string& content = file->content;

  uint32_t line_start = 0;
  uint32_t line_num = 1;
  for (uint32_t i = 0; i < span.begin && i < content.size(); ++i) {
    if (content[i] == '\n') {
      line_start = i + 1;
      ++line_num;
    }
  }

  size_t line_end_pos = content.find('\n', span.begin);
  uint32_t line_end = (line_end_pos == std::string::npos)
                          ? static_cast<uint32_t>(content.size())
                          : static_cast<uint32_t>(line_end_pos);

  return ResolvedSourceLine{
      .line_number = line_num,
      .column = span.begin - line_start,
      .line_text =
          std::string_view(content).substr(line_start, line_end - line_start),
  };
}

struct DiagnosticSnippet {
  uint32_t line_number = 0;
  std::string_view source_line;
  uint32_t column = 0;
  uint32_t marker_width = 1;
};

auto BuildDiagnosticSnippet(
    const SourceSpan& span, const SourceManager& source_manager)
    -> std::optional<DiagnosticSnippet> {
  auto resolved = ResolveSourceLine(source_manager, span);
  if (!resolved) return std::nullopt;

  uint32_t span_end = (span.end > span.begin) ? span.end : span.begin + 1;
  uint32_t line_end_offset =
      resolved->column + static_cast<uint32_t>(resolved->line_text.size());
  span_end =
      std::min(span_end, span.begin - resolved->column + line_end_offset);
  uint32_t marker_width = span_end - span.begin;
  if (marker_width == 0) {
    marker_width = 1;
  }

  return DiagnosticSnippet{
      .line_number = resolved->line_number,
      .source_line = resolved->line_text,
      .column = resolved->column,
      .marker_width = marker_width,
  };
}

void RenderDiagItem(
    TextSink& sink, const DiagItem& item, const SourceManager* source_manager,
    bool is_primary) {
  const char* kind_str = DiagKindToString(item.kind);
  fmt::text_style kind_style = DiagKindToStyle(item.kind);

  std::string location;
  std::optional<SourceSpan> span_opt;

  std::visit(
      common::Overloaded{
          [&](const SourceSpan& span) {
            span_opt = span;
            if (source_manager != nullptr) {
              location = FormatSourceLocation(span, *source_manager);
            }
          },
          [&](UnknownSpan) {},
      },
      item.span);

  if (!location.empty()) {
    sink.Write(
        fmt::format(
            "{}: {} {}\n", fmt::styled(location, fmt::emphasis::bold),
            fmt::styled(kind_str, kind_style),
            fmt::styled(
                item.message,
                is_primary ? fmt::emphasis::bold : fmt::text_style{})));
  } else {
    sink.Write(
        fmt::format(
            "{}: {} {}\n", fmt::styled("lyra", kToolStyle),
            fmt::styled(kind_str, kind_style),
            fmt::styled(
                item.message,
                is_primary ? fmt::emphasis::bold : fmt::text_style{})));
  }

  if (is_primary && span_opt && source_manager != nullptr) {
    if (auto snippet = BuildDiagnosticSnippet(*span_opt, *source_manager)) {
      constexpr auto kGutterStyle = fmt::fg(fmt::terminal_color::white);
      constexpr auto kMarkerStyle = fmt::fg(fmt::terminal_color::green);

      std::string line_num_str = std::to_string(snippet->line_number);
      size_t field_width = std::max(line_num_str.size(), size_t{4});
      std::string num_field(field_width - line_num_str.size(), ' ');
      num_field += line_num_str;
      std::string blank_field(field_width, ' ');

      sink.Write(
          fmt::format(
              " {} {}\n", fmt::styled(num_field + " |", kGutterStyle),
              snippet->source_line));

      std::string marker = "^" + std::string(snippet->marker_width - 1, '~');
      sink.Write(
          fmt::format(
              " {} {}{}\n", fmt::styled(blank_field + " |", kGutterStyle),
              std::string(snippet->column, ' '),
              fmt::styled(marker, kMarkerStyle)));
    }
  }
}

}  // namespace

void RenderDiagnostic(
    TextSink& sink, const Diagnostic& diag,
    const SourceManager* source_manager) {
  RenderDiagItem(sink, diag.primary, source_manager, true);
  for (const auto& note : diag.notes) {
    RenderDiagItem(sink, note, source_manager, false);
  }
}

void RenderDiagnostics(
    TextSink& sink, const DiagnosticSink& diag_sink,
    const SourceManager* source_manager) {
  uint32_t error_count = 0;
  uint32_t warning_count = 0;

  for (const auto& diag : diag_sink.GetDiagnostics()) {
    switch (diag.primary.kind) {
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

    RenderDiagItem(sink, diag.primary, source_manager, true);
    for (const auto& note : diag.notes) {
      RenderDiagItem(sink, note, source_manager, false);
    }
  }

  if (warning_count > 0 || error_count > 0) {
    std::string summary;
    if (warning_count > 0) {
      summary += fmt::format(
          "{} warning{}", warning_count, warning_count == 1 ? "" : "s");
    }
    if (warning_count > 0 && error_count > 0) {
      summary += " and ";
    }
    if (error_count > 0) {
      summary +=
          fmt::format("{} error{}", error_count, error_count == 1 ? "" : "s");
    }
    sink.Write(fmt::format("{} generated.\n", summary));
  }
}

void RenderError(TextSink& sink, std::string_view message) {
  sink.Write(
      fmt::format(
          "{}: {} {}\n", fmt::styled("lyra", kToolStyle),
          fmt::styled(
              "error:",
              fmt::fg(fmt::terminal_color::bright_red) | fmt::emphasis::bold),
          fmt::styled(message, fmt::emphasis::bold)));
}

void RenderWarning(TextSink& sink, std::string_view message) {
  sink.Write(
      fmt::format(
          "{}: {} {}\n", fmt::styled("lyra", kToolStyle),
          fmt::styled(
              "warning:", fmt::fg(fmt::terminal_color::bright_yellow) |
                              fmt::emphasis::bold),
          fmt::styled(message, fmt::emphasis::bold)));
}

void RenderInternalError(
    TextSink& sink, std::string_view context_fn, std::string_view message) {
  sink.Write(
      fmt::format(
          "lyra: internal compiler error: {}: {}\n", context_fn, message));
}

}  // namespace lyra::driver
