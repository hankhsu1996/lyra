#pragma once

#include <expected>
#include <filesystem>
#include <string>

#include "lyra/llvm_backend/link_request.hpp"

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

// Link object files with native inputs into an executable.
// The output path is request.output_path; its parent directory is created
// if it does not exist.
auto LinkExecutable(const Toolchain& toolchain, const LinkRequest& request)
    -> std::expected<std::filesystem::path, LinkError>;

}  // namespace lyra::lowering::mir_to_llvm
