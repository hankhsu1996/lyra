#include "lyra/ports/port.hpp"

#include <algorithm>
#include <array>
#include <charconv>
#include <cstdint>
#include <expected>
#include <filesystem>
#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <system_error>
#include <toml.hpp>
#include <utility>
#include <vector>

#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/StringExtras.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/SHA256.h>

namespace lyra::ports {

namespace {

namespace fs = std::filesystem;

constexpr std::string_view kDigestScheme = "sha256:";

// A length as eight bytes, least significant first, whatever the host's
// order, so the digest is the same everywhere.
auto AddLength(llvm::SHA256& hash, std::uint64_t length) -> void {
  std::array<std::uint8_t, sizeof(std::uint64_t)> bytes{};
  for (std::uint8_t& byte : bytes) {
    byte = static_cast<std::uint8_t>(length & 0xFFU);
    length >>= 8U;
  }
  hash.update(llvm::ArrayRef<std::uint8_t>(bytes));
}

auto Quoted(std::string_view text) -> std::string {
  std::string out = "\"";
  for (const char c : text) {
    if (c == '"' || c == '\\') {
      out += '\\';
    }
    out += c;
  }
  out += '"';
  return out;
}

auto ReadPortMetadata(const fs::path& file)
    -> std::expected<PortMetadata, std::string> {
  const toml::parse_result parsed = toml::parse_file(file.string());
  if (!parsed) {
    return std::unexpected(
        std::format(
            "'{}' cannot be read ({})", file.string(),
            parsed.error().description()));
  }
  const toml::table& table = parsed.table();
  const std::optional<std::string> name = table["name"].value<std::string>();
  const std::optional<std::string> version =
      table["version"].value<std::string>();
  const std::optional<std::int64_t> port_version =
      table["port_version"].value<std::int64_t>();
  const std::optional<std::string> digest =
      table["digest"].value<std::string>();
  if (!name || !version || !port_version || !digest) {
    return std::unexpected(
        std::format(
            "'{}' does not state a name, a version, a port version and a "
            "digest",
            file.string()));
  }
  return PortMetadata{
      .identity =
          {.name = *name, .version = *version, .port_version = *port_version},
      .digest = *digest};
}

}  // namespace

auto ParsePortVersion(std::string_view text) -> std::optional<std::int64_t> {
  const char* const last = std::to_address(text.end());
  std::int64_t port_version = 0;
  const auto [past, error] =
      std::from_chars(std::to_address(text.begin()), last, port_version);
  if (text.empty() || error != std::errc{} || past != last ||
      port_version < 1 || text.front() == '0') {
    return std::nullopt;
  }
  return port_version;
}

auto DigestOfPreparedTree(const fs::path& root)
    -> std::expected<std::string, std::string> {
  std::vector<std::pair<std::string, fs::path>> files;
  std::error_code walking;
  fs::recursive_directory_iterator entries(
      root, fs::directory_options::follow_directory_symlink, walking);
  for (; !walking && entries != fs::recursive_directory_iterator();
       entries.increment(walking)) {
    std::error_code kind;
    if (!fs::is_regular_file(entries->path(), kind)) {
      continue;
    }
    std::string relative =
        entries->path().lexically_relative(root).generic_string();
    if (relative == kPortMetadataFile) {
      continue;
    }
    files.emplace_back(std::move(relative), entries->path());
  }
  if (walking) {
    return std::unexpected(
        std::format(
            "cannot read the tree at '{}' ({})", root.string(),
            walking.message()));
  }
  std::ranges::sort(files, {}, &std::pair<std::string, fs::path>::first);
  llvm::SHA256 hash;
  for (const auto& [relative, path] : files) {
    const auto content = llvm::MemoryBuffer::getFile(path.string());
    if (!content) {
      return std::unexpected(
          std::format(
              "cannot read '{}' ({})", path.string(),
              content.getError().message()));
    }
    AddLength(hash, relative.size());
    hash.update(llvm::StringRef(relative));
    AddLength(hash, (*content)->getBufferSize());
    hash.update((*content)->getBuffer());
  }
  const std::array<std::uint8_t, 32> digest = hash.final();
  return std::format(
      "{}{}", kDigestScheme,
      llvm::toHex(llvm::ArrayRef<std::uint8_t>(digest), true));
}

auto RenderPortMetadata(const PortMetadata& metadata) -> std::string {
  return std::format(
      "name = {}\nversion = {}\nport_version = {}\ndigest = {}\n",
      Quoted(metadata.identity.name), Quoted(metadata.identity.version),
      metadata.identity.port_version, Quoted(metadata.digest));
}

auto PortVersionsIn(
    const fs::path& store, std::string_view name, std::string_view version)
    -> std::vector<std::int64_t> {
  std::vector<std::int64_t> found;
  std::error_code listing;
  for (fs::directory_iterator entries(store / name / version, listing);
       !listing && entries != fs::directory_iterator();
       entries.increment(listing)) {
    if (const std::optional<std::int64_t> port_version =
            ParsePortVersion(entries->path().filename().string())) {
      found.push_back(*port_version);
    }
  }
  std::ranges::sort(found);
  return found;
}

auto VerifiedPort(const fs::path& store, const PortIdentity& identity)
    -> std::expected<PreparedPort, std::string> {
  const fs::path root = store / identity.name / identity.version /
                        std::to_string(identity.port_version);
  auto metadata = ReadPortMetadata(root / kPortMetadataFile);
  if (!metadata) {
    return std::unexpected(std::move(metadata.error()));
  }
  if (metadata->identity != identity) {
    return std::unexpected(
        std::format(
            "'{}' states port {} of {} {}", root.string(),
            metadata->identity.port_version, metadata->identity.name,
            metadata->identity.version));
  }
  auto digest = DigestOfPreparedTree(root);
  if (!digest) {
    return std::unexpected(std::move(digest.error()));
  }
  if (*digest != metadata->digest) {
    return std::unexpected(
        std::format(
            "the content at '{}' has the digest {}, not the {} it was "
            "prepared with",
            root.string(), *digest, metadata->digest));
  }
  return PreparedPort{.root = root, .metadata = *std::move(metadata)};
}

}  // namespace lyra::ports
