#include <cstddef>
#include <cstdlib>
#include <exception>
#include <span>
#include <string>
#include <string_view>

#include <fmt/core.h>

#include "lyra/frontend/load.hpp"
#include "lyra/lowering/ast_to_hir/lower.hpp"
#include "lyra/lowering/hir_to_mir/lower_module_unit.hpp"
#include "lyra/projection/cpp/api.hpp"
#include "lyra/support/internal_error.hpp"

namespace {

void PrintUsage() {
  fmt::print(stderr, "usage: lyra_cli [--top NAME] file.sv [file.sv ...]\n");
}

auto ParseArgs(std::span<const char* const> args)
    -> lyra::frontend::CompilationInput {
  lyra::frontend::CompilationInput input;
  for (std::size_t i = 0; i < args.size(); ++i) {
    const std::string_view arg{args[i]};
    if (arg == "--top") {
      ++i;
      if (i >= args.size()) {
        PrintUsage();
        std::exit(1);
      }
      input.top = args[i];
    } else {
      input.files.emplace_back(arg);
    }
  }
  return input;
}

}  // namespace

auto main(int argc, char** argv) -> int {
  try {
    const std::span<const char* const> all_args{
        argv, static_cast<std::size_t>(argc)};
    auto input = ParseArgs(all_args.subspan(1));
    if (input.files.empty()) {
      PrintUsage();
      return 1;
    }

    auto parse = lyra::frontend::LoadFiles(input);
    if (!parse) {
      return 1;
    }

    auto hir_units =
        lyra::lowering::ast_to_hir::LowerCompilation(*parse->compilation);
    if (hir_units.size() != 1) {
      fmt::print(
          stderr,
          "lyra: error: projection mode requires exactly one top module (got "
          "{})\n",
          hir_units.size());
      return 1;
    }
    auto mir_unit =
        lyra::lowering::hir_to_mir::LowerModuleUnit(hir_units.front());
    auto projected = lyra::projection::cpp::ProjectModuleUnitToCpp(mir_unit);

    fmt::print("{}", projected);
    return 0;
  } catch (const lyra::support::InternalError& e) {
    fmt::print(stderr, "lyra: internal error: {}\n", e.what());
    return 2;
  } catch (const std::exception& e) {
    fmt::print(stderr, "lyra: error: {}\n", e.what());
    return 2;
  }
}
