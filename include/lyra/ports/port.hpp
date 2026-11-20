#pragma once

#include <cstdint>
#include <expected>
#include <filesystem>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace lyra::ports {

// A port: a release of a third-party library Lyra supports, as Lyra prepares
// it. The version is the upstream release's; the port version counts revisions
// of Lyra's preparation of that release, as vcpkg's `port-version` does, and a
// published one is never changed.
struct PortIdentity {
  std::string name;
  std::string version;
  std::int64_t port_version = 0;

  auto operator==(const PortIdentity&) const -> bool = default;
};

// What a prepared tree states of itself, in the file of this name at its
// root: its identity, and the digest of the rest of its content, written by
// the build that prepared it.
inline constexpr std::string_view kPortMetadataFile = "port.toml";

struct PortMetadata {
  PortIdentity identity;
  std::string digest;

  auto operator==(const PortMetadata&) const -> bool = default;
};

// A port version as written: a count from 1 in decimal with no leading zero,
// which is also how a store names the directory holding it; none where the
// text is not one.
auto ParsePortVersion(std::string_view text) -> std::optional<std::int64_t>;

// The digest of a prepared tree: SHA-256 over its files but the metadata file,
// in the order of their paths relative to `root`, written with `/`, each file
// adding its path and then its bytes, each preceded by its length. A link is
// followed to the file it names, so the same content laid out anywhere has one
// digest. Spelled `sha256:<hex>`.
auto DigestOfPreparedTree(const std::filesystem::path& root)
    -> std::expected<std::string, std::string>;

// The metadata as a prepared tree states it, a TOML table of its four keys.
auto RenderPortMetadata(const PortMetadata& metadata) -> std::string;

// A store of prepared ports holds each at `<name>/<version>/<port_version>/`
// under its root. The port versions it holds of one release, ascending.
auto PortVersionsIn(
    const std::filesystem::path& store, std::string_view name,
    std::string_view version) -> std::vector<std::int64_t>;

// A port's prepared tree as a store holds it.
struct PreparedPort {
  std::filesystem::path root;
  PortMetadata metadata;
};

// The prepared tree of `identity` in `store`, after checking that it states
// that identity and that its content has the digest it states, so a tree
// changed after it was prepared is never taken.
auto VerifiedPort(
    const std::filesystem::path& store, const PortIdentity& identity)
    -> std::expected<PreparedPort, std::string>;

}  // namespace lyra::ports
