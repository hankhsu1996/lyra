#include "lyra/driver/file_output.hpp"

#include <cstddef>
#include <expected>
#include <filesystem>
#include <format>
#include <fstream>
#include <ios>
#include <string>
#include <string_view>
#include <system_error>
#include <utility>

#include "lyra/diag/diag_code.hpp"

namespace lyra::driver {

namespace {

auto IoError(std::string message) -> diag::Diagnostic {
  return diag::Make(diag::DiagCode::kHostIoError, std::move(message));
}

auto ReadContent(const std::filesystem::path& path)
    -> std::expected<std::string, std::string> {
  std::ifstream in(path, std::ios::binary | std::ios::ate);
  if (!in) {
    return std::unexpected(std::format("failed to read '{}'", path.string()));
  }
  const auto size = static_cast<std::streamsize>(in.tellg());
  if (size <= 0) {
    return std::string{};
  }
  std::string content(static_cast<std::size_t>(size), '\0');
  in.seekg(0);
  in.read(content.data(), size);
  if (!in) {
    return std::unexpected(std::format("failed to read '{}'", path.string()));
  }
  return content;
}

// Whether the file already holds exactly these bytes. A file that cannot be
// read answers no, which is the answer that writes it.
auto AlreadyHolds(const std::filesystem::path& path, std::string_view content)
    -> bool {
  auto existing = ReadContent(path);
  return existing.has_value() && *existing == content;
}

}  // namespace

auto WriteFile(const std::filesystem::path& path, std::string_view content)
    -> diag::Result<void> {
  if (AlreadyHolds(path, content)) {
    return {};
  }
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

auto CopyFile(
    const std::filesystem::path& from, const std::filesystem::path& to)
    -> diag::Result<void> {
  auto content = ReadContent(from);
  if (!content) {
    return std::unexpected(IoError(std::move(content.error())));
  }
  return WriteFile(to, *content);
}

}  // namespace lyra::driver
