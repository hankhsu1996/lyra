#pragma once

#include <memory>
#include <optional>
#include <string>

#include <slang/ast/Compilation.h>
#include <slang/driver/Driver.h>

#include "lyra/diag/source_manager.hpp"
#include "lyra/frontend/slang_source_mapper.hpp"

namespace lyra::frontend {

// What the rest of the compiler reads once the front end has run. The driver
// that produced it owns the text every span here points into, so it has to
// outlive this.
struct ParseResult {
  std::unique_ptr<slang::ast::Compilation> compilation;
  diag::SourceManager diag_sources;
  SlangSourceMapper source_mapper;
};

// Applies the driver's options, reads everything it was pointed at, and
// elaborates. Returns nullopt when a source could not be read or a syntax
// error stopped the parse; slang's account of either is queued on the driver's
// diagnostic engine for ReportSlangDiagnostics to render.
auto Elaborate(slang::driver::Driver& driver) -> std::optional<ParseResult>;

// Renders everything slang has to say through the one engine the warning
// options configured, so a suppression on the command line reaches the
// diagnostics it names. Returns false when any of them carried error severity.
auto ReportSlangDiagnostics(
    slang::driver::Driver& driver, slang::ast::Compilation& compilation,
    std::string& out_text) -> bool;

}  // namespace lyra::frontend
