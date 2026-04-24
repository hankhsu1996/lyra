#include <argparse/argparse.hpp>
#include <exception>
#include <expected>
#include <format>
#include <string>
#include <utility>
#include <vector>

#include <fmt/core.h>

#include "lyra/backend/cpp/api.hpp"
#include "lyra/frontend/load.hpp"
#include "lyra/hir/dump.hpp"
#include "lyra/lowering/ast_to_hir/lower.hpp"
#include "lyra/lowering/hir_to_mir/lower_module_unit.hpp"
#include "lyra/mir/dump.hpp"
#include "lyra/support/internal_error.hpp"

namespace {

enum class CommandKind { kDumpHir, kDumpMir, kEmitCpp };

struct ParsedArgs {
  CommandKind cmd = CommandKind::kEmitCpp;
  bool no_project = false;
  lyra::frontend::CompilationInput input;
};

void AddCompilationFlags(argparse::ArgumentParser& cmd) {
  cmd.add_argument("--no-project")
      .help("operate in direct file mode (no lyra.toml lookup)")
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
    } else {
      return std::unexpected(
          std::format("emit requires 'cpp'\n{}", emit_cmd.help().str()));
    }
  } else {
    return std::unexpected(program.help().str());
  }
  return out;
}

}  // namespace

auto main(int argc, char** argv) -> int {
  try {
    auto parsed = ParseArgs(argc, argv);
    if (!parsed) {
      fmt::print(stderr, "lyra: error: {}\n", parsed.error());
      return 1;
    }
    auto& args = *parsed;

    if (!args.no_project) {
      fmt::print(
          stderr,
          "lyra: error: project mode is not implemented yet; "
          "pass --no-project to run in direct file mode\n");
      return 1;
    }
    if (args.input.files.empty()) {
      fmt::print(stderr, "lyra: error: no input files\n");
      return 1;
    }

    auto parse = lyra::frontend::LoadFiles(args.input);
    if (!parse) {
      return 1;
    }

    auto hir_units =
        lyra::lowering::ast_to_hir::LowerCompilation(*parse->compilation);

    if (args.cmd == CommandKind::kDumpHir) {
      fmt::print("{}", lyra::hir::DumpHir(hir_units));
      return 0;
    }

    if (hir_units.size() != 1) {
      fmt::print(
          stderr, "lyra: error: expected exactly one top module, got {}\n",
          hir_units.size());
      return 1;
    }
    auto mir_unit =
        lyra::lowering::hir_to_mir::LowerModuleUnit(hir_units.front());

    switch (args.cmd) {
      case CommandKind::kDumpMir:
        fmt::print("{}", lyra::mir::DumpMir(mir_unit));
        return 0;
      case CommandKind::kEmitCpp:
        fmt::print("{}", lyra::backend::cpp::EmitCpp(mir_unit));
        return 0;
      case CommandKind::kDumpHir:
        break;
    }
    return 0;
  } catch (const lyra::support::InternalError& e) {
    fmt::print(stderr, "lyra: internal error: {}\n", e.what());
    return 2;
  } catch (const std::exception& e) {
    fmt::print(stderr, "lyra: error: {}\n", e.what());
    return 2;
  }
}
