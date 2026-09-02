#include <cstddef>
#include <cstdio>
#include <exception>
#include <format>
#include <span>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include <fmt/core.h>
#include <slang/driver/Driver.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/cli/command_line.hpp"
#include "lyra/cli/commands.hpp"
#include "lyra/cli/design_manifest.hpp"
#include "lyra/compiler/compile.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/render.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/diag/source_manager.hpp"
#include "lyra/driver/cpp_build.hpp"
#include "lyra/driver/dpi_boundary.hpp"
#include "lyra/driver/pch.hpp"

using lyra::cli::CommandKind;
using lyra::cli::Reporter;

auto main(int argc, char** argv) -> int {
  try {
    const std::span<char* const> raw_args(argv, static_cast<std::size_t>(argc));
    const std::string program_path =
        raw_args.empty() ? std::string{} : std::string(raw_args.front());
    auto argv_split = lyra::cli::SplitAtSeparator(raw_args);

    slang::driver::Driver driver;
    driver.addStandardArgs();
    lyra::cli::CliOptions cli_options;
    lyra::cli::RegisterCliOptions(driver.cmdLine, cli_options);

    if (lyra::cli::WantsHelp(argv_split.lyra)) {
      lyra::cli::PrintHelp(driver);
      return 0;
    }

    auto command = lyra::cli::ParseCommandWords(driver, argv_split.lyra);

    const bool use_color = lyra::cli::UseColor(cli_options);
    const Reporter report{lyra::diag::RenderOptions{
        .use_color = use_color, .show_source_snippet = true}};

    if (!command) {
      // An empty message means the parser already printed its own account of
      // what was wrong with the command line.
      if (!command.error().empty()) {
        report(
            lyra::diag::Make(
                lyra::diag::DiagCode::kHostInvalidCliArgs, command.error()));
      }
      return 1;
    }
    driver.setTerminalColorsEnabled(use_color);

    // `cache clear` consults no design, takes no input files, and never reaches
    // the compiler. Dispatch it before a declaration is looked for, so that one
    // elsewhere on the machine can neither be read nor stop the cache being
    // cleared.
    if (*command == CommandKind::kCacheClear) {
      auto cleared_or =
          lyra::driver::pch::Clear(lyra::cli::MakePchOptions(cli_options));
      if (!cleared_or) {
        report(std::move(cleared_or.error()));
        return 1;
      }
      fmt::print(
          "cleared {} precompiled-header file{}\n", *cleared_or,
          *cleared_or == 1 ? "" : "s");
      return 0;
    }

    auto declaration_or =
        lyra::cli::ResolveDesignDeclaration(cli_options, driver);
    if (!declaration_or) {
      report(std::move(declaration_or.error()));
      return 1;
    }
    // The declaration is flattened once into the two things the rest of the run
    // reads: the manifest to apply and re-read, and an absent search kept for
    // the no-input-files note below. That note is the only place absence still
    // speaks -- a named source makes hasFiles() true, so NoSearchNeeded never
    // reaches that branch.
    const auto& declaration = *declaration_or;
    const lyra::cli::DesignManifest* manifest = nullptr;
    std::optional<lyra::cli::ManifestAbsent> absent;
    std::visit(
        lyra::Overloaded{
            [&](const lyra::cli::DesignManifest& m) { manifest = &m; },
            [&](const lyra::cli::ManifestAbsent& a) { absent = a; },
            [&](lyra::cli::NoSearchNeeded) {}},
        declaration);
    if (manifest != nullptr) {
      if (auto applied = lyra::cli::ApplyDesignManifest(*manifest, driver);
          !applied) {
        report(std::move(applied.error()));
        return 1;
      }
    }

    auto parsed = lyra::cli::ResolveCliOptions(
        cli_options, manifest, *command, std::move(argv_split.child));
    if (!parsed) {
      report(
          lyra::diag::Make(
              lyra::diag::DiagCode::kHostInvalidCliArgs, parsed.error()));
      return 1;
    }
    const auto& args = *parsed;

    if (!driver.sourceLoader.hasFiles()) {
      auto diagnostic = lyra::diag::Make(
          lyra::diag::DiagCode::kHostNoInputFiles, "no input files");
      if (absent) {
        diagnostic =
            std::move(diagnostic)
                .WithNote(
                    std::format(
                        "searched for lyra.toml from {} up to {}",
                        absent->started.string(), absent->stopped.string()));
      }
      // A declaration was in effect and still named nothing, which reads as no
      // declaration at all unless the message says which one applied -- and the
      // one that applied may be several directories above the caller.
      if (manifest != nullptr) {
        diagnostic = std::move(diagnostic)
                         .WithNote(
                             std::format(
                                 "design '{}' at {} declares no source files",
                                 manifest->name, manifest->path.string()));
      }
      report(std::move(diagnostic));
      return 1;
    }

    // Classified before compiling anything, so a mistyped path is reported
    // against the command line rather than after a full frontend and lowering
    // pass.
    auto dpi_inputs =
        lyra::driver::ValidateDpiLinkInputs(args.dpi_link_sources);
    if (!dpi_inputs) {
      report(std::move(dpi_inputs.error()));
      return 1;
    }

    lyra::diag::DiagnosticSink sink;
    auto result = lyra::compiler::Compile(
        driver, lyra::compiler::LoweringPolicy{.assertions = args.assertions},
        sink, lyra::cli::LoweringDepth(args));

    // `run` executes the simulation; its stdout/stderr are the simulation's
    // own, so compile-phase warnings must not bleed into them. Surface slang
    // diagnostics for run only when they carry errors (which abort below);
    // other commands always show them. Use `compile`/`dump` to see warnings.
    const bool suppress_compile_warnings =
        args.cmd == CommandKind::kRun && result.slang_ok && !sink.HasErrors();
    if (!suppress_compile_warnings && !result.slang_diagnostics.empty()) {
      fmt::print(stderr, "{}", result.slang_diagnostics);
    }

    const lyra::diag::SourceManager* mgr =
        result.artifacts.parse ? &result.artifacts.parse->diag_sources
                               : nullptr;
    if (sink.HasErrors()) {
      report(sink, mgr);
      return 1;
    }
    if (!result.slang_ok) {
      return 1;
    }

    return lyra::cli::RunCommand(
        lyra::cli::CommandContext{
            .args = &args,
            .artifacts = &result.artifacts,
            .mgr = mgr,
            .dpi_inputs = *dpi_inputs,
            .formatting = args.format ? lyra::driver::SourceFormatting::kOn
                                      : lyra::driver::SourceFormatting::kOff,
            .report = &report,
            .program_path = program_path});
  } catch (const lyra::InternalError& e) {
    fmt::print(stderr, "{}", lyra::diag::RenderInternalError(e.what()));
    return 2;
  } catch (const std::exception& e) {
    fmt::print(stderr, "lyra: error: {}\n", e.what());
    return 2;
  }
}
