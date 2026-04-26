#pragma once

#include <filesystem>
#include <optional>
#include <span>
#include <string>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_manager.hpp"
#include "tests/diag_framework/expected_diag.hpp"

namespace lyra::test {

struct MatchContext {
  const diag::SourceManager* mgr = nullptr;
  std::filesystem::path case_dir;
};

auto NormalizeCasePath(
    const std::filesystem::path& abs_path,
    const std::filesystem::path& case_dir) -> std::string;

auto MatchOne(
    const diag::Diagnostic& actual, const ExpectedDiag& expected,
    const MatchContext& ctx) -> bool;

auto MatchAll(
    std::span<const diag::Diagnostic> actuals,
    std::span<const ExpectedDiag> expecteds, const MatchContext& ctx)
    -> std::optional<std::string>;

auto FormatDiagnosticsForDebug(
    std::span<const diag::Diagnostic> diags, const MatchContext& ctx)
    -> std::string;

}  // namespace lyra::test
