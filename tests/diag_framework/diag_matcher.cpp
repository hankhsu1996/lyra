#include "tests/diag_framework/diag_matcher.hpp"

#include <algorithm>
#include <cstddef>
#include <filesystem>
#include <format>
#include <numeric>
#include <optional>
#include <span>
#include <string>
#include <variant>
#include <vector>

#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/render.hpp"
#include "lyra/diag/source_manager.hpp"
#include "lyra/diag/source_span.hpp"

namespace lyra::test {
namespace {

auto Specificity(const ExpectedDiag& e) -> int {
  // code is always present and is the primary identity, so it does not
  // contribute to relative specificity.
  return 0 + (e.kind ? 4 : 0) + (e.category ? 2 : 0) + (e.file ? 2 : 0) +
         (e.line ? 1 : 0);
}

auto FormatExpected(const ExpectedDiag& e) -> std::string {
  std::string s = std::format("code={}", diag::DiagCodeName(e.code));
  if (e.kind) s += std::format(", kind={}", static_cast<int>(*e.kind));
  if (e.category) {
    s += std::format(", category={}", static_cast<int>(*e.category));
  }
  if (e.file) s += std::format(", file={}", *e.file);
  if (e.line) s += std::format(", line={}", *e.line);
  return s;
}

auto MatchExpectedAt(
    std::size_t k, std::span<const diag::Diagnostic> actuals,
    std::span<const ExpectedDiag> expecteds, std::span<const std::size_t> order,
    std::vector<bool>& used, const MatchContext& ctx) -> bool {
  if (k == order.size()) {
    return true;
  }
  const auto& e = expecteds[order[k]];
  for (std::size_t i = 0; i < actuals.size(); ++i) {
    if (used[i]) continue;
    if (!MatchOne(actuals[i], e, ctx)) continue;
    used[i] = true;
    if (MatchExpectedAt(k + 1, actuals, expecteds, order, used, ctx)) {
      return true;
    }
    used[i] = false;
  }
  return false;
}

}  // namespace

auto NormalizeCasePath(
    const std::filesystem::path& abs_path,
    const std::filesystem::path& case_dir) -> std::string {
  // Use lexically_relative to compare path strings directly, without
  // filesystem canonicalization. std::filesystem::relative resolves
  // symlinks asymmetrically when the two arguments traverse different
  // sets of symlinks (e.g. runfiles trees vs. source-tree symlinks),
  // which produces misleading results in a Bazel sandbox.
  auto rel = abs_path.lexically_normal().lexically_relative(
      case_dir.lexically_normal());
  if (!rel.empty() && !rel.native().starts_with("..")) {
    return rel.generic_string();
  }
  std::error_code ec;
  auto canon = std::filesystem::weakly_canonical(abs_path, ec);
  if (ec) return abs_path.generic_string();
  return canon.generic_string();
}

auto MatchOne(
    const diag::Diagnostic& actual, const ExpectedDiag& expected,
    const MatchContext& ctx) -> bool {
  const auto& primary = actual.primary;
  if (primary.code != expected.code) {
    return false;
  }
  if (expected.kind && primary.kind != *expected.kind) {
    return false;
  }
  if (expected.category && primary.category != expected.category) {
    return false;
  }
  if (expected.file || expected.line) {
    if (!std::holds_alternative<diag::SourceSpan>(primary.span)) {
      return false;
    }
    const auto& span = std::get<diag::SourceSpan>(primary.span);
    if (ctx.mgr == nullptr) return false;
    const auto* info = ctx.mgr->GetFile(span.file_id);
    if (info == nullptr) return false;
    if (expected.file) {
      const auto actual_rel = NormalizeCasePath(info->path, ctx.case_dir);
      if (actual_rel != *expected.file) return false;
    }
    if (expected.line) {
      const auto lc = ctx.mgr->OffsetToLineCol(span.file_id, span.begin);
      if (static_cast<int>(lc.line) != *expected.line) return false;
    }
  }
  return true;
}

auto MatchAll(
    std::span<const diag::Diagnostic> actuals,
    std::span<const ExpectedDiag> expecteds, const MatchContext& ctx)
    -> std::optional<std::string> {
  std::vector<std::size_t> order(expecteds.size());
  std::ranges::iota(order, std::size_t{0});
  std::ranges::sort(order, [&](std::size_t a, std::size_t b) {
    const int sa = Specificity(expecteds[a]);
    const int sb = Specificity(expecteds[b]);
    return sa != sb ? sa > sb : a < b;
  });

  std::vector<bool> used(actuals.size(), false);
  if (MatchExpectedAt(0, actuals, expecteds, order, used, ctx)) {
    return std::nullopt;
  }

  std::string msg = "structured diagnostic match failed.\nexpected:\n";
  for (std::size_t i = 0; i < expecteds.size(); ++i) {
    msg += std::format("  [{}] {}\n", i, FormatExpected(expecteds[i]));
  }
  msg += "actual:\n";
  msg += FormatDiagnosticsForDebug(actuals, ctx);
  return msg;
}

auto FormatDiagnosticsForDebug(
    std::span<const diag::Diagnostic> diags, const MatchContext& ctx)
    -> std::string {
  std::string out;
  if (diags.empty()) {
    out += "  (no diagnostics)\n";
    return out;
  }
  const diag::RenderOptions opts{
      .use_color = false, .show_source_snippet = false};
  for (std::size_t i = 0; i < diags.size(); ++i) {
    const auto& primary = diags[i].primary;
    out += std::format("  [{}] kind={}", i, static_cast<int>(primary.kind));
    if (primary.category) {
      out += std::format(", category={}", static_cast<int>(*primary.category));
    }
    out += std::format(", code={}", diag::DiagCodeName(primary.code));
    if (std::holds_alternative<diag::SourceSpan>(primary.span) &&
        ctx.mgr != nullptr) {
      const auto& span = std::get<diag::SourceSpan>(primary.span);
      const auto* info = ctx.mgr->GetFile(span.file_id);
      if (info != nullptr) {
        out += std::format(
            ", file={}", NormalizeCasePath(info->path, ctx.case_dir));
      }
      const auto lc = ctx.mgr->OffsetToLineCol(span.file_id, span.begin);
      out += std::format(", line={}", lc.line);
    }
    out += '\n';
    out += "      ";
    out += diag::RenderDiagnostic(diags[i], ctx.mgr, opts);
    if (out.empty() || out.back() != '\n') out += '\n';
  }
  return out;
}

}  // namespace lyra::test
