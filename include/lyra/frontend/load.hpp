#pragma once

#include <memory>
#include <optional>
#include <string>
#include <string_view>

#include <slang/ast/Compilation.h>
#include <slang/driver/Driver.h>

#include "lyra/frontend/slang_source_manager.hpp"
#include "lyra/support/force_targets.hpp"

namespace lyra::frontend {

// What the rest of the compiler reads once the front end has run: the
// elaborated design, how its diagnostics name their sources, and what a force
// takes over as the front end was told to read the standard. The driver that
// produced it owns the text every span names a piece of, so it has to outlive
// this.
struct ParseResult {
  std::unique_ptr<slang::ast::Compilation> compilation;
  SlangSourceManager diag_sources;
  support::ForceTargets force_targets;
};

// The language version a build is read under where neither its command line
// nor its declaration names one.
inline constexpr std::string_view kDefaultLanguageVersion = "1800-2023";

// Applies the driver's options, reads everything it was pointed at, and
// elaborates. Returns nullopt when a source could not be read or a syntax
// error stopped the parse; slang's account of either is queued on the driver's
// diagnostic engine for ReportSlangDiagnostics to render.
auto Elaborate(slang::driver::Driver& driver) -> std::optional<ParseResult>;

// Renders everything the front end has to say -- what elaborating each
// declaration found, and what only the whole elaborated design answers --
// through the one engine the warning options configured, so a suppression on
// the command line reaches the diagnostics it names. Returns false when any of
// them carried error severity.
auto ReportSlangDiagnostics(
    slang::driver::Driver& driver, slang::ast::Compilation& compilation,
    std::string& out_text) -> bool;

}  // namespace lyra::frontend
