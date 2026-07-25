#include "lyra/driver/file_output.hpp"

#include <filesystem>
#include <format>
#include <fstream>
#include <string_view>
#include <system_error>

#include "lyra/diag/diag_code.hpp"

namespace lyra::driver {

namespace {

auto IoError(std::string message) -> diag::Diagnostic {
  return diag::Make(diag::DiagCode::kHostIoError, std::move(message));
}

}  // namespace

auto WriteFile(const std::filesystem::path& path, std::string_view content)
    -> diag::Result<void> {
  std::error_code ec;
  std::filesystem::create_directories(path.parent_path(), ec);
  if (ec) {
    return std::unexpected(IoError(
        std::format(
            "failed to create '{}': {}", path.parent_path().string(),
            ec.message())));
  }
  std::ofstream out(path, std::ios::binary);
  out << content;
  out.flush();
  if (!out) {
    return std::unexpected(
        IoError(std::format("failed to write '{}'", path.string())));
  }
  return {};
}

auto CopyFileWritable(
    const std::filesystem::path& from, const std::filesystem::path& to)
    -> diag::Result<void> {
  std::error_code ec;
  std::filesystem::create_directories(to.parent_path(), ec);
  if (ec) {
    return std::unexpected(IoError(
        std::format(
            "failed to create '{}': {}", to.parent_path().string(),
            ec.message())));
  }
  std::filesystem::copy_file(
      from, to, std::filesystem::copy_options::overwrite_existing, ec);
  if (ec) {
    return std::unexpected(IoError(
        std::format(
            "failed to copy '{}' to '{}': {}", from.string(), to.string(),
            ec.message())));
  }
  std::filesystem::permissions(
      to, std::filesystem::perms::owner_write,
      std::filesystem::perm_options::add, ec);
  if (ec) {
    return std::unexpected(IoError(
        std::format(
            "failed to set permissions on '{}': {}", to.string(),
            ec.message())));
  }
  return {};
}

}  // namespace lyra::driver
