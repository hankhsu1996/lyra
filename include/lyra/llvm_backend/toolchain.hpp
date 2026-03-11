#pragma once

#include <expected>
#include <filesystem>
#include <string>

namespace lyra::lowering::mir_to_llvm {

// Toolchain configuration for linking AOT executables.
struct Toolchain {
  std::filesystem::path cc_path;  // C compiler (used as linker driver)
};

// Structured error from the link step.
struct LinkError {
  std::string stage;    // "link"
  std::string message;  // Human-readable description
  std::string stderr;   // Raw stderr from the linker
};

// Detect the host toolchain.
// Resolution order: LYRA_CC env > CC env > PATH search (clang, cc, gcc).
// Set allow_ambient_search=false to skip PATH search (LYRA_CC/CC only).
auto DetectToolchain(bool allow_ambient_search = true)
    -> std::expected<Toolchain, std::string>;

// Link an object file with a runtime archive into an executable at
// output_dir/<name>. Runtime symbols referenced by the design object are
// pulled from the archive by normal archive linking.
auto LinkExecutable(
    const Toolchain& toolchain, const std::filesystem::path& object_path,
    const std::filesystem::path& runtime_lib_path,
    const std::filesystem::path& output_dir, const std::string& name)
    -> std::expected<std::filesystem::path, LinkError>;

}  // namespace lyra::lowering::mir_to_llvm
