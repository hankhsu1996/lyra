#include <algorithm>
#include <argparse/argparse.hpp>
#include <cstdio>
#include <exception>
#include <expected>
#include <filesystem>
#include <format>
#include <optional>
#include <span>
#include <string>
#include <unistd.h>
#include <utility>
#include <vector>

#include <fmt/core.h>

#include "lyra/backend/cpp/api.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/compiler/compile.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/render.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/diag/source_manager.hpp"
#include "lyra/driver/cpp_build.hpp"
#include "lyra/driver/runtime_export.hpp"
#include "lyra/frontend/load.hpp"
#include "lyra/hir/dump.hpp"
#include "lyra/mir/dump.hpp"
#include "lyra/support/subprocess.hpp"

namespace {

enum class CommandKind { kDumpHir, kDumpMir, kEmitCpp, kCompile, kRun };

struct ParsedArgs {
  CommandKind cmd = CommandKind::kEmitCpp;
  bool no_project = false;
  bool no_color = false;
  bool force_color = false;
  lyra::frontend::CompilationInput input;
  std::string out_dir;
};

void AddCompilationFlags(argparse::ArgumentParser& cmd) {
  cmd.add_argument("--no-project")
      .help("operate in direct file mode (no lyra.toml lookup)")
      .default_value(false)
      .implicit_value(true);
  cmd.add_argument("--no-color")
      .help("disable ANSI color in diagnostics")
      .default_value(false)
      .implicit_value(true);
  cmd.add_argument("--color")
      .help("force ANSI color in diagnostics (override TTY detection)")
      .default_value(false)
      .implicit_value(true);
  cmd.add_argument("--top")
      .help("top module name")
      .default_value(std::string{});
  cmd.add_argument("-I", "--include-directory")
      .help("add include search directory")
      .append();
  cmd.add_argument("-D", "--define-macro")
      .help("define preprocessor macro (NAME or NAME=VALUE)")
      .append();
  cmd.add_argument("-G")
      .help("override module parameter (NAME=VALUE)")
      .metavar("NAME=VALUE")
      .append();
  cmd.add_argument("--single-unit")
      .help("compile all files as a single compilation unit")
      .default_value(false)
      .implicit_value(true);
  cmd.add_argument("files").help("SystemVerilog source files").remaining();
}

void BindCompilationFlags(
    const argparse::ArgumentParser& cmd, ParsedArgs& out) {
  out.no_project = cmd.get<bool>("--no-project");
  out.no_color = cmd.get<bool>("--no-color");
  out.force_color = cmd.get<bool>("--color");
  out.input.top = cmd.get<std::string>("--top");
  if (auto incs =
          cmd.present<std::vector<std::string>>("--include-directory")) {
    out.input.incdirs = std::move(*incs);
  }
  if (auto defs = cmd.present<std::vector<std::string>>("--define-macro")) {
    out.input.defines = std::move(*defs);
  }
  if (auto ovr = cmd.present<std::vector<std::string>>("-G")) {
    out.input.param_overrides = std::move(*ovr);
  }
  out.input.single_unit = cmd.get<bool>("--single-unit");
  if (auto files = cmd.present<std::vector<std::string>>("files")) {
    out.input.files = std::move(*files);
  }
}

auto ParseArgs(int argc, char** argv)
    -> std::expected<ParsedArgs, std::string> {
  argparse::ArgumentParser program("lyra");

  argparse::ArgumentParser dump_cmd("dump");
  argparse::ArgumentParser dump_hir_cmd("hir");
  argparse::ArgumentParser dump_mir_cmd("mir");
  AddCompilationFlags(dump_hir_cmd);
  AddCompilationFlags(dump_mir_cmd);
  dump_cmd.add_subparser(dump_hir_cmd);
  dump_cmd.add_subparser(dump_mir_cmd);

  argparse::ArgumentParser emit_cmd("emit");
  argparse::ArgumentParser emit_cpp_cmd("cpp");
  AddCompilationFlags(emit_cpp_cmd);
  emit_cpp_cmd.add_argument("-o", "--out-dir")
      .help("write the self-contained C++ project to this directory")
      .default_value(std::string{});
  emit_cmd.add_subparser(emit_cpp_cmd);

  argparse::ArgumentParser compile_cmd("compile");
  AddCompilationFlags(compile_cmd);
  compile_cmd.add_argument("-o", "--out-dir")
      .help("write the self-contained project and built program here")
      .default_value(std::string{});

  argparse::ArgumentParser run_cmd("run");
  AddCompilationFlags(run_cmd);

  program.add_subparser(dump_cmd);
  program.add_subparser(emit_cmd);
  program.add_subparser(compile_cmd);
  program.add_subparser(run_cmd);

  try {
    program.parse_args(argc, argv);
  } catch (const std::exception& e) {
    return std::unexpected(
        std::format("{}\n{}", e.what(), program.help().str()));
  }

  ParsedArgs out;
  if (program.is_subcommand_used("dump")) {
    if (dump_cmd.is_subcommand_used("hir")) {
      out.cmd = CommandKind::kDumpHir;
      BindCompilationFlags(dump_hir_cmd, out);
    } else if (dump_cmd.is_subcommand_used("mir")) {
      out.cmd = CommandKind::kDumpMir;
      BindCompilationFlags(dump_mir_cmd, out);
    } else {
      return std::unexpected(
          std::format(
              "dump requires 'hir' or 'mir'\n{}", dump_cmd.help().str()));
    }
  } else if (program.is_subcommand_used("emit")) {
    if (emit_cmd.is_subcommand_used("cpp")) {
      out.cmd = CommandKind::kEmitCpp;
      BindCompilationFlags(emit_cpp_cmd, out);
      out.out_dir = emit_cpp_cmd.get<std::string>("--out-dir");
      if (out.out_dir.empty()) {
        return std::unexpected(
            std::format(
                "emit cpp requires --out-dir\n{}", emit_cpp_cmd.help().str()));
      }
    } else {
      return std::unexpected(
          std::format("emit requires 'cpp'\n{}", emit_cmd.help().str()));
    }
  } else if (program.is_subcommand_used("compile")) {
    out.cmd = CommandKind::kCompile;
    BindCompilationFlags(compile_cmd, out);
    out.out_dir = compile_cmd.get<std::string>("--out-dir");
    if (out.out_dir.empty()) {
      return std::unexpected(
          std::format(
              "compile requires --out-dir\n{}", compile_cmd.help().str()));
    }
  } else if (program.is_subcommand_used("run")) {
    out.cmd = CommandKind::kRun;
    BindCompilationFlags(run_cmd, out);
  } else {
    return std::unexpected(program.help().str());
  }
  return out;
}

auto ResolveColorPreference(bool no_color_flag, bool force_color_flag) -> bool {
  if (no_color_flag) {
    return false;
  }
  if (force_color_flag) {
    return true;
  }
  return ::isatty(::fileno(stderr)) != 0;
}

}  // namespace

auto main(int argc, char** argv) -> int {
  try {
    const std::span<char* const> raw_args(argv, static_cast<std::size_t>(argc));
    const std::string program_path =
        raw_args.empty() ? std::string{} : std::string(raw_args.front());
    auto parsed = ParseArgs(argc, argv);
    const bool use_color =
        parsed.has_value()
            ? ResolveColorPreference(parsed->no_color, parsed->force_color)
            : ResolveColorPreference(false, false);
    const lyra::diag::RenderOptions render_opts{
        .use_color = use_color, .show_source_snippet = true};

    auto report = [&](lyra::diag::Diagnostic diag,
                      const lyra::diag::SourceManager* mgr = nullptr) {
      fmt::print(
          stderr, "{}", lyra::diag::RenderDiagnostic(diag, mgr, render_opts));
    };

    if (!parsed) {
      report(
          lyra::diag::Diagnostic::HostError(
              lyra::diag::DiagCode::kHostInvalidCliArgs, parsed.error()));
      return 1;
    }
    auto& args = *parsed;

    if (!args.no_project) {
      report(
          lyra::diag::Diagnostic::HostError(
              lyra::diag::DiagCode::kHostProjectModeUnimplemented,
              "project mode is not implemented yet; pass --no-project to "
              "run in direct file mode"));
      return 1;
    }
    if (args.input.files.empty()) {
      report(
          lyra::diag::Diagnostic::HostError(
              lyra::diag::DiagCode::kHostNoInputFiles, "no input files"));
      return 1;
    }

    lyra::diag::DiagnosticSink sink;
    const auto stop_after = args.cmd == CommandKind::kDumpHir
                                ? lyra::compiler::StopAfter::kHir
                                : lyra::compiler::StopAfter::kMir;
    auto result = lyra::compiler::Compile(args.input, sink, stop_after);

    // `run` executes the simulation; its stdout/stderr are the simulation's
    // own, so compile-phase warnings must not bleed into them. Surface slang
    // diagnostics for run only when they carry errors (which abort below);
    // other commands always show them. Use `compile`/`dump` to see warnings.
    const bool suppress_compile_warnings =
        args.cmd == CommandKind::kRun && result.slang_ok && !sink.HasErrors();
    if (result.artifacts.parse && !suppress_compile_warnings) {
      std::string slang_text;
      lyra::frontend::RenderSlangDiagnostics(
          *result.artifacts.parse, use_color, slang_text);
      if (!slang_text.empty()) {
        fmt::print(stderr, "{}", slang_text);
      }
    }

    if (sink.HasErrors()) {
      const lyra::diag::SourceManager* mgr =
          result.artifacts.parse ? &result.artifacts.parse->diag_sources
                                 : nullptr;
      fmt::print(
          stderr, "{}", lyra::diag::RenderDiagnostics(sink, mgr, render_opts));
      return 1;
    }
    if (!result.slang_ok) {
      return 1;
    }

    if (args.cmd == CommandKind::kDumpHir) {
      fmt::print("{}", lyra::hir::DumpHir(*result.artifacts.hir_units));
      return 0;
    }

    const lyra::diag::SourceManager* mgr =
        result.artifacts.parse ? &result.artifacts.parse->diag_sources
                               : nullptr;

    auto resolve_runtime =
        [&]() -> std::optional<lyra::driver::RuntimeLocation> {
      auto loc_or = lyra::driver::ResolveRuntimeLocation(program_path);
      if (!loc_or) {
        report(
            lyra::diag::Diagnostic::HostError(
                lyra::diag::DiagCode::kHostIoError, std::move(loc_or.error())));
        return std::nullopt;
      }
      return *loc_or;
    };

    auto build_tops = [](const std::vector<lyra::mir::CompilationUnit>& units,
                         const std::vector<std::string>& top_names) {
      std::vector<lyra::backend::cpp::TopInstance> tops;
      for (const auto& unit : units) {
        if (std::ranges::find(top_names, unit.structural_scope.name) !=
            top_names.end()) {
          tops.push_back({.name = unit.structural_scope.name, .unit = &unit});
        }
      }
      return tops;
    };

    switch (args.cmd) {
      case CommandKind::kDumpMir:
        for (const auto& unit : *result.artifacts.mir_units) {
          fmt::print("{}", lyra::mir::DumpMir(unit));
        }
        return 0;
      case CommandKind::kEmitCpp: {
        auto runtime = resolve_runtime();
        if (!runtime) {
          return 1;
        }
        const std::filesystem::path dir = args.out_dir;
        const auto& units = *result.artifacts.mir_units;
        const auto tops = build_tops(units, result.artifacts.top_unit_names);
        auto assembled =
            lyra::driver::AssembleProject(*runtime, units, tops, dir);
        if (!assembled) {
          report(std::move(assembled.error()), mgr);
          return 1;
        }
        fmt::print("emitted: {}\n", dir.string());
        return 0;
      }
      case CommandKind::kCompile: {
        auto runtime = resolve_runtime();
        if (!runtime) {
          return 1;
        }
        const std::filesystem::path dir = args.out_dir;
        const auto& units = *result.artifacts.mir_units;
        const auto tops = build_tops(units, result.artifacts.top_unit_names);
        auto assembled =
            lyra::driver::AssembleProject(*runtime, units, tops, dir);
        if (!assembled) {
          report(std::move(assembled.error()), mgr);
          return 1;
        }
        auto built = lyra::driver::BuildProject(dir);
        if (!built) {
          report(std::move(built.error()), mgr);
          return 1;
        }
        fmt::print("compiled: {}\n", built->string());
        return 0;
      }
      case CommandKind::kRun: {
        auto runtime = resolve_runtime();
        if (!runtime) {
          return 1;
        }
        auto tmp_or = lyra::support::MakeTempDir();
        if (!tmp_or) {
          report(
              lyra::diag::Diagnostic::HostError(
                  lyra::diag::DiagCode::kHostIoError,
                  std::move(tmp_or.error())));
          return 1;
        }
        const auto& units = *result.artifacts.mir_units;
        const auto tops = build_tops(units, result.artifacts.top_unit_names);
        auto exit_code =
            lyra::driver::RunInPlace(*runtime, units, tops, *tmp_or);
        if (!exit_code) {
          report(std::move(exit_code.error()), mgr);
          return 1;
        }
        return *exit_code;
      }
      case CommandKind::kDumpHir:
        break;
    }
    return 0;
  } catch (const lyra::InternalError& e) {
    fmt::print(stderr, "{}", lyra::diag::RenderInternalError(e.what()));
    return 2;
  } catch (const std::exception& e) {
    fmt::print(stderr, "lyra: error: {}\n", e.what());
    return 2;
  }
}
