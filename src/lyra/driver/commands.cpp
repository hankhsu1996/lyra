#include "commands.hpp"

#include <filesystem>
#include <format>
#include <fstream>
#include <iostream>
#include <optional>
#include <string>

#include "argparse/argparse.hpp"
#include "print.hpp"

namespace lyra::driver {
namespace {

namespace fs = std::filesystem;

}  // namespace

auto InitCommand(const argparse::ArgumentParser& cmd) -> int {
  std::optional<std::string> name;
  if (auto n = cmd.present<std::string>("name")) {
    name = *n;
  }
  bool force = cmd.get<bool>("--force");

  fs::path project_dir;
  std::string project_name;
  bool create_directory = false;

  if (name) {
    project_dir = fs::path(*name);
    if (project_dir.is_relative()) {
      project_dir = fs::current_path() / project_dir;
    }
    project_name = project_dir.filename().string();
    create_directory = true;

    if (fs::exists(project_dir)) {
      PrintError(
          std::format("directory '{}' already exists", project_dir.string()));
      return 1;
    }
  } else {
    project_dir = fs::current_path();
    project_name = project_dir.filename().string();

    if (fs::exists(project_dir / "lyra.toml") && !force) {
      PrintError("lyra.toml already exists (use --force to overwrite)");
      return 1;
    }
  }

  if (create_directory) {
    fs::create_directories(project_dir);

    std::ofstream sv_file(project_dir / (project_name + ".sv"));
    sv_file << std::format(
        "module {};\n"
        "  initial begin\n"
        "    $display(\"Hello from {}!\");\n"
        "  end\n"
        "endmodule\n",
        project_name, project_name);
  }

  std::ofstream toml_file(project_dir / "lyra.toml");
  toml_file << std::format(
      "[package]\n"
      "name = \"{}\"\n"
      "top = \"{}\"\n"
      "\n"
      "[sources]\n"
      "files = [\"{}.sv\"]\n",
      project_name, project_name, project_name);

  std::cout << std::format("Created project '{}'\n", project_name);
  return 0;
}

}  // namespace lyra::driver
