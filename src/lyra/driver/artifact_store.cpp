#include "lyra/driver/artifact_store.hpp"

#include <algorithm>
#include <array>
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <filesystem>
#include <format>
#include <fstream>
#include <functional>
#include <iterator>
#include <optional>
#include <random>
#include <string>
#include <string_view>
#include <system_error>
#include <vector>

#include "lyra/base/hash.hpp"
#include "lyra/diag/diag_code.hpp"

namespace lyra::driver {

namespace {

constexpr std::uint64_t kStreamPrime = 0x100000001b3ULL;

// How long an entry may go unused before a trim removes it, and how often a
// trim runs at all. Go's build cache uses the same two figures.
constexpr auto kUnusedLifetime = std::chrono::days{5};
constexpr auto kTrimInterval = std::chrono::days{1};

// The file whose age says when the store was last trimmed.
constexpr std::string_view kTrimMarker = "trimmed";

// The kinds of entry a trim or a clear walks.
constexpr std::array<std::string_view, 2> kEntryKinds = {
    kStoredHeaderDir, kStoredProgramDir};

auto ReadWholeFile(const std::filesystem::path& path) -> std::string {
  std::ifstream in(path, std::ios::binary);
  return std::string{
      std::istreambuf_iterator<char>(in), std::istreambuf_iterator<char>()};
}

void MarkUsed(const std::filesystem::path& entry) {
  std::error_code ignored;
  std::filesystem::last_write_time(
      entry, std::filesystem::file_time_type::clock::now(), ignored);
}

}  // namespace

void ContentNamer::Mix(std::string_view bytes) {
  for (const char byte : bytes) {
    stream_digest_ ^= static_cast<unsigned char>(byte);
    stream_digest_ *= kStreamPrime;
  }
  base::HashCombine(piece_digest_, std::hash<std::string_view>{}(bytes));
}

void ContentNamer::Add(std::string_view label, std::string_view bytes) {
  Mix(label);
  Mix(std::format("{}", bytes.size()));
  Mix(bytes);
}

void ContentNamer::AddFile(
    std::string_view label, const std::filesystem::path& file) {
  Add(label, ReadWholeFile(file));
}

void ContentNamer::AddTree(
    std::string_view label, const std::filesystem::path& root) {
  std::vector<std::filesystem::path> files;
  std::error_code ec;
  for (const auto& entry : std::filesystem::recursive_directory_iterator(
           root, std::filesystem::directory_options::follow_directory_symlink,
           ec)) {
    if (entry.is_regular_file()) {
      files.push_back(entry.path());
    }
  }
  std::ranges::sort(files);
  Add(label, std::format("{}", files.size()));
  for (const auto& file : files) {
    Add("path", file.lexically_relative(root).generic_string());
    AddFile("content", file);
  }
}

void ContentNamer::AddExecutable(
    std::string_view label, const std::filesystem::path& exe) {
  std::error_code resolve_ec;
  const std::filesystem::path resolved =
      std::filesystem::canonical(exe, resolve_ec);
  // A file that cannot be read answers the same sentinel every time, so it
  // names consistently rather than failing a build that may not need it.
  std::error_code read_ec;
  const std::filesystem::directory_entry file(
      resolve_ec ? exe : resolved, read_ec);
  Add(label, file.path().string());
  Add("changed",
      std::format(
          "{}", file.last_write_time(read_ec).time_since_epoch().count()));
  Add("size", std::format("{}", file.file_size(read_ec)));
}

auto ContentNamer::Finish() const -> ContentName {
  return ContentName{
      .hex = std::format("{:016x}{:016x}", stream_digest_, piece_digest_)};
}

auto LocateStore(const std::optional<std::filesystem::path>& override)
    -> std::optional<std::filesystem::path> {
  if (override) {
    return override;
  }
  // A relative base would name a different directory from every working
  // directory, which the base directory specification rules out.
  const auto absolute_from =
      [](const char* var) -> std::optional<std::filesystem::path> {
    const char* value = std::getenv(var);
    if (value == nullptr || !std::filesystem::path(value).is_absolute()) {
      return std::nullopt;
    }
    return std::filesystem::path(value);
  };
  if (auto base = absolute_from("XDG_CACHE_HOME")) {
    return *base / "lyra";
  }
  if (auto home = absolute_from("HOME")) {
    return *home / ".cache" / "lyra";
  }
  return std::nullopt;
}

auto StoredPath(
    const std::filesystem::path& store, std::string_view kind,
    const ContentName& name) -> std::filesystem::path {
  return store / kind / name.hex;
}

auto TemporaryBeside(const std::filesystem::path& target)
    -> std::filesystem::path {
  std::random_device random;
  return target.parent_path() / std::format(
                                    ".{}.tmp.{:08x}{:08x}",
                                    target.filename().string(), random(),
                                    random());
}

auto FindStored(
    const std::filesystem::path& store, std::string_view kind,
    const ContentName& name) -> std::optional<std::filesystem::path> {
  const auto entry = StoredPath(store, kind, name);
  std::error_code ec;
  if (!std::filesystem::is_regular_file(entry, ec)) {
    return std::nullopt;
  }
  MarkUsed(entry);
  return entry;
}

auto CopyStored(
    const std::filesystem::path& store, std::string_view kind,
    const ContentName& name, const std::filesystem::path& to)
    -> diag::Result<bool> {
  const auto entry = StoredPath(store, kind, name);
  auto copied = CopyOut(entry, to);
  if (copied && *copied) {
    MarkUsed(entry);
  }
  return copied;
}

void KeepStored(
    const std::filesystem::path& store, std::string_view kind,
    const ContentName& name, const std::filesystem::path& built) {
  const auto entry = StoredPath(store, kind, name);
  std::error_code ec;
  std::filesystem::create_directories(entry.parent_path(), ec);
  if (ec) {
    return;
  }
  if (auto copied = CopyOut(built, entry); copied && *copied) {
    TrimStore(store);
  }
}

auto CopyOut(const std::filesystem::path& from, const std::filesystem::path& to)
    -> diag::Result<bool> {
  const auto temporary = TemporaryBeside(to);
  std::error_code ec;
  std::filesystem::copy_file(from, temporary, ec);
  if (ec) {
    std::error_code ignored;
    std::filesystem::remove(temporary, ignored);
    if (!std::filesystem::exists(from, ignored)) {
      return false;
    }
    return diag::Fail(
        diag::DiagCode::kHostIoError,
        std::format(
            "failed to copy '{}' to '{}': {}", from.string(), to.string(),
            ec.message()));
  }
  std::filesystem::permissions(
      temporary, std::filesystem::status(from, ec).permissions(),
      std::filesystem::perm_options::replace, ec);
  std::filesystem::rename(temporary, to, ec);
  if (ec) {
    std::error_code ignored;
    std::filesystem::remove(temporary, ignored);
    return diag::Fail(
        diag::DiagCode::kHostIoError,
        std::format("failed to write '{}': {}", to.string(), ec.message()));
  }
  return true;
}

void TrimStore(const std::filesystem::path& store) {
  const auto now = std::filesystem::file_time_type::clock::now();
  const auto marker = store / kTrimMarker;
  std::error_code ec;
  const auto last_trim = std::filesystem::last_write_time(marker, ec);
  if (!ec && now - last_trim < kTrimInterval) {
    return;
  }
  // The marker moves first, so two builds finishing together do not both walk
  // the store.
  std::ofstream{marker}.flush();
  std::filesystem::last_write_time(marker, now, ec);
  for (const std::string_view kind : kEntryKinds) {
    for (const auto& entry :
         std::filesystem::directory_iterator(store / kind, ec)) {
      std::error_code entry_ec;
      const auto used = entry.last_write_time(entry_ec);
      if (!entry_ec && now - used > kUnusedLifetime) {
        std::filesystem::remove(entry.path(), entry_ec);
      }
    }
  }
}

auto ClearStore(const std::filesystem::path& store) -> std::size_t {
  std::size_t removed = 0;
  for (const std::string_view kind : kEntryKinds) {
    std::error_code ec;
    for (const auto& entry :
         std::filesystem::directory_iterator(store / kind, ec)) {
      std::error_code entry_ec;
      if (entry.is_regular_file(entry_ec) &&
          std::filesystem::remove(entry.path(), entry_ec)) {
        ++removed;
      }
    }
  }
  return removed;
}

}  // namespace lyra::driver
