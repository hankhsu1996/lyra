#pragma once

#include <filesystem>
#include <string>
#include <variant>
#include <vector>

namespace lyra::lowering::mir_to_llvm {

// Runtime linkage by full file path (e.g., static archive for production AOT).
struct RuntimePathLinkInput {
  std::filesystem::path path;
};

// Runtime linkage by directory search (e.g., shared library for test AOT).
// Produces -L<search_dir> -l:<library_name> on the link line.
struct RuntimeSearchLinkInput {
  std::filesystem::path search_dir;
  std::string library_name;
};

using RuntimeLinkInput =
    std::variant<RuntimePathLinkInput, RuntimeSearchLinkInput>;

// Structured description of a link invocation.
// Used by both production AOT and test executable linking.
struct LinkRequest {
  std::filesystem::path output_path;
  std::vector<std::filesystem::path> object_inputs;
  std::vector<RuntimeLinkInput> runtime_link_inputs;
  std::vector<std::filesystem::path> external_link_inputs;
  std::vector<std::string> system_libs;
};

}  // namespace lyra::lowering::mir_to_llvm
