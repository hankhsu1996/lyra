#include "sv_injection.hpp"

#include <cctype>
#include <cstddef>
#include <expected>
#include <format>
#include <span>
#include <string>
#include <string_view>
#include <vector>

namespace lyra::test {

namespace {

auto IsIdentChar(char c) -> bool {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
         (c >= '0' && c <= '9') || c == '_';
}

auto IsIdentStart(char c) -> bool {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

auto SkipWhitespace(std::string_view s, std::size_t i) -> std::size_t {
  while (i < s.size() && (s[i] == ' ' || s[i] == '\t')) {
    ++i;
  }
  return i;
}

struct ModuleHit {
  std::size_t name_begin;
  std::size_t name_end;
};

// Find all module-declaration sites by scanning line-anchored occurrences of
// `module <name>` at indentation 0. Returns the byte offsets of the name.
auto FindModuleDeclarations(std::string_view source) -> std::vector<ModuleHit> {
  std::vector<ModuleHit> hits;
  std::size_t i = 0;
  const std::size_t n = source.size();
  while (i < n) {
    i = SkipWhitespace(source, i);
    if (i + 6 <= n && source.substr(i, 6) == "module") {
      const std::size_t after = i + 6;
      if (after < n && (source[after] == ' ' || source[after] == '\t')) {
        std::size_t j = SkipWhitespace(source, after);
        if (j < n && IsIdentStart(source[j])) {
          std::size_t k = j + 1;
          while (k < n && IsIdentChar(source[k])) {
            ++k;
          }
          // Reject `endmodule`, `modulee`, etc. -- our `module` was followed
          // by whitespace then an identifier, so this is a real declaration.
          hits.push_back({j, k});
        }
      }
    }
    // Advance to next line.
    while (i < n && source[i] != '\n') {
      ++i;
    }
    if (i < n) {
      ++i;
    }
  }
  return hits;
}

auto FindLastEndmodule(std::string_view source)
    -> std::expected<std::size_t, std::string> {
  std::size_t last = std::string_view::npos;
  std::size_t i = 0;
  const std::size_t n = source.size();
  while (i < n) {
    std::size_t line_start = i;
    std::size_t after_ws = SkipWhitespace(source, i);
    if (after_ws + 9 <= n && source.substr(after_ws, 9) == "endmodule") {
      const std::size_t after = after_ws + 9;
      const bool is_word_boundary = (after == n) || !IsIdentChar(source[after]);
      if (is_word_boundary) {
        last = line_start;
      }
    }
    while (i < n && source[i] != '\n') {
      ++i;
    }
    if (i < n) {
      ++i;
    }
  }
  if (last == std::string_view::npos) {
    return std::unexpected(std::string{"source has no `endmodule` line"});
  }
  return last;
}

}  // namespace

auto RewriteSourceWithProbes(
    std::string_view source, std::string_view top,
    std::span<const InjectionEntry> entries)
    -> std::expected<std::string, std::string> {
  const auto modules = FindModuleDeclarations(source);
  if (modules.empty()) {
    return std::unexpected(
        std::string{
            "expect.variables: source declares no module; cannot inject "
            "probe block"});
  }
  if (modules.size() != 1) {
    return std::unexpected(
        std::format(
            "expect.variables: source declares {} modules; "
            "only single-module sources are supported in this phase",
            modules.size()));
  }
  const std::string_view actual_name = source.substr(
      modules[0].name_begin, modules[0].name_end - modules[0].name_begin);
  if (actual_name != top) {
    return std::unexpected(
        std::format(
            "expect.variables: module name '{}' does not match --top '{}'",
            std::string{actual_name}, std::string{top}));
  }
  auto endmodule_or = FindLastEndmodule(source);
  if (!endmodule_or) return std::unexpected(endmodule_or.error());

  std::string injection;
  injection += "  final begin\n";
  injection += "    $display(\"";
  injection.append(kProbeMarkerBegin);
  injection += "\");\n";
  for (const auto& e : entries) {
    injection += "    $display(\"";
    injection += e.name;
    injection += '=';
    injection += e.sv_format_specifier;
    injection += "\", ";
    injection += e.name;
    injection += ");\n";
  }
  injection += "    $display(\"";
  injection.append(kProbeMarkerEnd);
  injection += "\");\n";
  injection += "  end\n";

  std::string out;
  out.reserve(source.size() + injection.size());
  out.append(source.substr(0, *endmodule_or));
  out.append(injection);
  out.append(source.substr(*endmodule_or));
  return out;
}

}  // namespace lyra::test
