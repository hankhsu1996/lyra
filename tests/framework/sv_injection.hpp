#pragma once

#include <expected>
#include <span>
#include <string>
#include <string_view>

namespace lyra::test {

struct InjectionEntry {
  std::string name;
  std::string sv_format_specifier;
};

// Rewrite `source` so that it ends with a synthetic `final` block printing
// the sentinel-bracketed marker payload for each entry in `entries`.
//
// The source must contain exactly one `module <top> ... endmodule` block; the
// caller has already validated that `top` matches the case's --top. The final
// block is injected before that module's `endmodule`.
//
// Returns the rewritten source, or a human-readable error describing why the
// rewrite cannot proceed (multiple modules, top mismatch, missing endmodule).
auto RewriteSourceWithProbes(
    std::string_view source, std::string_view top,
    std::span<const InjectionEntry> entries)
    -> std::expected<std::string, std::string>;

inline constexpr std::string_view kProbeMarkerBegin = "__LYRA_VARS_BEGIN__";
inline constexpr std::string_view kProbeMarkerEnd = "__LYRA_VARS_END__";

}  // namespace lyra::test
