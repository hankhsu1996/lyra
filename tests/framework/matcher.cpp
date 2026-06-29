#include "matcher.hpp"

#include <cctype>
#include <cstddef>
#include <format>
#include <optional>
#include <string>
#include <string_view>

#include "sv_injection.hpp"

namespace lyra::test {

auto StripAnsi(std::string_view s) -> std::string {
  std::string out;
  out.reserve(s.size());
  std::size_t i = 0;
  while (i < s.size()) {
    if (s[i] == '\x1b' && i + 1 < s.size() && s[i + 1] == '[') {
      std::size_t j = i + 2;
      while (j < s.size()) {
        char c = s[j];
        if ((c >= '0' && c <= '9') || c == ';') {
          ++j;
          continue;
        }
        break;
      }
      if (j < s.size()) {
        ++j;
      }
      i = j;
      continue;
    }
    out.push_back(s[i]);
    ++i;
  }
  return out;
}

namespace {

// Return the position of the next newline at or after `pos` in `text`. If
// none, returns text.size().
auto NextNewline(std::string_view text, std::size_t pos) -> std::size_t {
  const auto nl = text.find('\n', pos);
  return (nl == std::string_view::npos) ? text.size() : nl;
}

}  // namespace

auto ExtractProbeMarkers(std::string_view stdout_text)
    -> ProbeExtractionResult {
  ProbeExtractionResult out;
  out.residual_stdout.assign(stdout_text);

  const auto begin_idx = stdout_text.find(kProbeMarkerBegin);
  if (begin_idx == std::string_view::npos) {
    return out;
  }
  // Sentinel must be the entire line (allow leading whitespace = none today).
  const bool begin_line_start =
      begin_idx == 0 || stdout_text[begin_idx - 1] == '\n';
  if (!begin_line_start) {
    return out;
  }
  const std::size_t begin_line_end = NextNewline(stdout_text, begin_idx);
  // After-begin newline must match exactly the BEGIN marker text.
  if (stdout_text.substr(begin_idx, begin_line_end - begin_idx) !=
      kProbeMarkerBegin) {
    return out;
  }
  out.begin_found = true;

  const auto end_search_from = (begin_line_end < stdout_text.size())
                                   ? begin_line_end + 1
                                   : begin_line_end;
  const auto end_idx = stdout_text.find(kProbeMarkerEnd, end_search_from);
  if (end_idx == std::string_view::npos) {
    return out;
  }
  const bool end_line_start = end_idx == 0 || stdout_text[end_idx - 1] == '\n';
  if (!end_line_start) {
    return out;
  }
  const std::size_t end_line_end = NextNewline(stdout_text, end_idx);
  if (stdout_text.substr(end_idx, end_line_end - end_idx) != kProbeMarkerEnd) {
    return out;
  }

  // Parse `name=value` lines between BEGIN line and END line.
  std::size_t cursor = (begin_line_end < stdout_text.size())
                           ? begin_line_end + 1
                           : begin_line_end;
  while (cursor < end_idx) {
    const auto line_end = NextNewline(stdout_text, cursor);
    const auto line = stdout_text.substr(cursor, line_end - cursor);
    const auto eq = line.find('=');
    if (eq != std::string_view::npos) {
      out.records.push_back(
          {.name = std::string{line.substr(0, eq)},
           .value = std::string{line.substr(eq + 1)}});
    }
    cursor = (line_end < stdout_text.size()) ? line_end + 1 : line_end;
  }

  out.complete = true;

  // Strip the entire sentinel block (BEGIN line through END line inclusive,
  // including their trailing newlines).
  const std::size_t strip_begin = begin_idx;
  const std::size_t strip_end =
      (end_line_end < stdout_text.size()) ? end_line_end + 1 : end_line_end;
  std::string residual;
  residual.reserve(stdout_text.size() - (strip_end - strip_begin));
  residual.append(stdout_text.substr(0, strip_begin));
  residual.append(stdout_text.substr(strip_end));
  out.residual_stdout = std::move(residual);
  return out;
}

auto CheckOutput(
    const std::string& actual, const ExpectedOutput& expected,
    std::string_view context) -> std::optional<std::string> {
  const std::string cleaned = StripAnsi(actual);
  if (expected.exact.has_value()) {
    if (cleaned != *expected.exact) {
      return std::format(
          "{}: exact mismatch\n--- expected ---\n{}\n--- actual ---\n{}",
          context, *expected.exact, cleaned);
    }
  }
  for (const auto& needle : expected.contains) {
    if (cleaned.find(needle) == std::string::npos) {
      return std::format(
          "{}: expected substring not found: \"{}\"\n--- actual ---\n{}",
          context, needle, cleaned);
    }
  }
  for (const auto& needle : expected.not_contains) {
    if (cleaned.find(needle) != std::string::npos) {
      return std::format(
          "{}: forbidden substring present: \"{}\"\n--- actual ---\n{}",
          context, needle, cleaned);
    }
  }
  return std::nullopt;
}

}  // namespace lyra::test
