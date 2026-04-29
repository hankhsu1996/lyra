#include <argparse/argparse.hpp>
#include <cstdio>
#include <exception>
#include <expected>
#include <filesystem>
#include <format>
#include <fstream>
#include <string>
#include <system_error>
#include <unistd.h>
#include <utility>
#include <vector>

#include <fmt/core.h>

#include "lyra/backend/cpp/api.hpp"
#include "lyra/backend/cpp/artifact.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/compiler/compile.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/render.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/diag/source_manager.hpp"
#include "lyra/frontend/load.hpp"
#include "lyra/hir/dump.hpp"
#include "lyra/mir/class_decl.hpp"
#include "lyra/mir/dump.hpp"

namespace {

enum class CommandKind { kDumpHir, kDumpMir, kEmitCpp };

struct ParsedArgs {
  CommandKind cmd = CommandKind::kEmitCpp;
  bool no_project = false;
  bool no_color = false;
  bool force_color = false;
  lyra::frontend::CompilationInput input;
  std::string emit_out_dir;
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
      .help("write C++ artifacts to this directory")
      .default_value(std::string{});
  emit_cmd.add_subparser(emit_cpp_cmd);

  program.add_subparser(dump_cmd);
  program.add_subparser(emit_cmd);

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
      out.emit_out_dir = emit_cpp_cmd.get<std::string>("--out-dir");
    } else {
      return std::unexpected(
          std::format("emit requires 'cpp'\n{}", emit_cmd.help().str()));
    }
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

    if (result.artifacts.parse) {
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

    switch (args.cmd) {
      case CommandKind::kDumpMir:
        fmt::print("{}", lyra::mir::DumpMir(*result.artifacts.mir_unit));
        return 0;
      case CommandKind::kEmitCpp: {
        const lyra::mir::ClassDecl* entry = nullptr;
        for (const auto& cls : result.artifacts.mir_unit->classes) {
          if (cls.name == args.input.top) {
            entry = &cls;
            break;
          }
        }
        if (entry == nullptr) {
          throw lyra::InternalError(
              std::format(
                  "emit cpp: top class '{}' not found in compilation unit",
                  args.input.top));
        }
        auto set_or =
            lyra::backend::cpp::EmitCpp(*result.artifacts.mir_unit, *entry);
        if (!set_or) {
          const lyra::diag::SourceManager* mgr =
              result.artifacts.parse ? &result.artifacts.parse->diag_sources
                                     : nullptr;
          report(std::move(set_or.error()), mgr);
          return 1;
        }
        const auto& set = *set_or;
        if (args.emit_out_dir.empty()) {
          for (const auto& file : set.files) {
            fmt::print("=== {} ===\n{}", file.relpath, file.content);
            if (!file.content.empty() && file.content.back() != '\n') {
              fmt::print("\n");
            }
          }
        } else {
          std::error_code ec;
          std::filesystem::create_directories(args.emit_out_dir, ec);
          if (ec) {
            throw lyra::InternalError(
                std::format(
                    "emit cpp: failed to create out-dir '{}': {}",
                    args.emit_out_dir, ec.message()));
          }
          for (const auto& file : set.files) {
            const auto path =
                std::filesystem::path(args.emit_out_dir) / file.relpath;
            std::filesystem::create_directories(path.parent_path(), ec);
            if (ec) {
              throw lyra::InternalError(
                  std::format(
                      "emit cpp: failed to create '{}': {}",
                      path.parent_path().string(), ec.message()));
            }
            std::ofstream out_stream(path);
            if (!out_stream) {
              throw lyra::InternalError(
                  std::format(
                      "emit cpp: failed to open '{}' for write",
                      path.string()));
            }
            out_stream << file.content;
            out_stream.flush();
            if (!out_stream) {
              throw lyra::InternalError(
                  std::format("emit cpp: failed to write '{}'", path.string()));
            }
          }
        }
        return 0;
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
