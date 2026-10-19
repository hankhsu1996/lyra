#include "lyra/driver/action_cache.hpp"

#include <algorithm>
#include <chrono>
#include <cstddef>
#include <cstdlib>
#include <filesystem>
#include <format>
#include <fstream>
#include <iterator>
#include <mutex>
#include <optional>
#include <random>
#include <string>
#include <string_view>
#include <system_error>
#include <utility>
#include <vector>

#include "lyra/diag/diag_code.hpp"
#include "lyra/stable_hash/fingerprint.hpp"
#include "lyra/stable_hash/stable_hasher.hpp"

namespace lyra::driver {

namespace {

// How long an entry may go unused before a trim removes it, and how often a
// trim runs at all. Go's build cache uses the same two figures.
constexpr auto kUnusedLifetime = std::chrono::days{5};
constexpr auto kTrimInterval = std::chrono::days{1};

// The file whose age says when the store was last trimmed.
constexpr std::string_view kTrimMarker = "trimmed";

// The two stores, each a directory of entries named by a digest: the action
// cache's entries say which output an action produced, and the
// content-addressable storage's hold those outputs' bytes.
constexpr std::string_view kActionCacheDir = "ac";
constexpr std::string_view kContentDir = "cas";

// What a link optimizing a whole program compiled per module, one directory
// for each layout a link may keep it in.
constexpr std::string_view kLlvmLinkTimeDir = "thinlto";
constexpr std::string_view kGccLinkTimeDir = "gcc-lto";

// Every entry the store trims: each file in each directory whose files are
// named by key and laid out flat.
auto EntriesOf(const std::filesystem::path& store)
    -> std::vector<std::filesystem::path> {
  std::vector<std::filesystem::path> entries;
  for (const std::string_view dir :
       {kActionCacheDir, kContentDir, kLlvmLinkTimeDir}) {
    std::error_code ec;
    for (const auto& entry :
         std::filesystem::directory_iterator(store / dir, ec)) {
      std::error_code entry_ec;
      if (entry.is_regular_file(entry_ec)) {
        entries.push_back(entry.path());
      }
    }
  }
  return entries;
}

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

auto EntryPath(
    const std::filesystem::path& store, std::string_view dir,
    const stable_hash::Fingerprint& digest) -> std::filesystem::path {
  return store / dir / stable_hash::HexOf(digest);
}

// The digest of a file's bytes, which is what the content-addressable storage
// keeps it under.
auto DigestOfFile(const std::filesystem::path& file)
    -> diag::Result<stable_hash::Fingerprint> {
  std::error_code ec;
  if (!std::filesystem::is_regular_file(file, ec)) {
    return diag::Fail(
        diag::DiagCode::kHostIoError, "cannot read '{}'", file.string());
  }
  return stable_hash::FingerprintOfText(ReadWholeFile(file));
}

// The output the action cache records for `digest`, if it is still in the
// content-addressable storage. One whose output has been trimmed or discarded
// is a miss.
auto Kept(
    const std::filesystem::path& store, const stable_hash::Fingerprint& digest)
    -> std::optional<ActionResult> {
  const auto entry = EntryPath(store, kActionCacheDir, digest);
  std::error_code ec;
  if (!std::filesystem::is_regular_file(entry, ec)) {
    return std::nullopt;
  }
  const auto output = stable_hash::FingerprintOfHex(ReadWholeFile(entry));
  if (!output) {
    return std::nullopt;
  }
  const auto path = EntryPath(store, kContentDir, *output);
  if (!std::filesystem::is_regular_file(path, ec)) {
    return std::nullopt;
  }
  MarkUsed(entry);
  MarkUsed(path);
  return ActionResult{.digest = *output, .path = path, .performed = false};
}

// Keeps `bytes` in the store under `path`, written whole or not at all, and
// answers whether it is there now.
auto KeepAt(const std::filesystem::path& path, std::string_view bytes) -> bool {
  std::error_code ec;
  std::filesystem::create_directories(path.parent_path(), ec);
  if (ec) {
    return false;
  }
  const auto temporary = TemporaryBeside(path);
  {
    std::ofstream out(temporary, std::ios::binary);
    out.write(bytes.data(), static_cast<std::streamsize>(bytes.size()));
    if (!out) {
      std::filesystem::remove(temporary, ec);
      return false;
    }
  }
  std::filesystem::rename(temporary, path, ec);
  if (ec) {
    std::filesystem::remove(temporary, ec);
    return false;
  }
  return true;
}

// Keeps a copy of `made` at `kept`, unless the same bytes are there already,
// and answers whether they are there now.
auto KeepCopy(
    const std::filesystem::path& made, const std::filesystem::path& kept)
    -> bool {
  std::error_code ec;
  if (std::filesystem::is_regular_file(kept, ec)) {
    return true;
  }
  std::filesystem::create_directories(kept.parent_path(), ec);
  if (ec) {
    return false;
  }
  const auto copied = CopyOut(made, kept);
  return copied && *copied;
}

// Removes every entry unused for five days, at most once a day. A concurrent
// build reading an entry either finds it whole, or finds it gone and performs
// the action. Removing one costs that build time and nothing more.
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
  for (const std::filesystem::path& entry : EntriesOf(store)) {
    std::error_code entry_ec;
    const auto used = std::filesystem::last_write_time(entry, entry_ec);
    if (!entry_ec && now - used > kUnusedLifetime) {
      std::filesystem::remove(entry, entry_ec);
    }
  }
}

// Performs the step and keeps what it produced. An output that cannot be kept
// makes the next build slower and fails nothing, so this build then reads it
// where the step wrote it, and the action is recorded only once its output is
// in the store.
auto Perform(
    const std::optional<std::filesystem::path>& store,
    const std::filesystem::path& scratch,
    const stable_hash::Fingerprint& digest, ActionStep step)
    -> diag::Result<ActionResult> {
  const auto dir = scratch / "actions" / stable_hash::HexOf(digest);
  std::error_code ec;
  std::filesystem::create_directories(dir, ec);
  if (ec) {
    return diag::Fail(
        diag::DiagCode::kHostIoError, "cannot create '{}' ({})", dir.string(),
        ec.message());
  }
  const auto made = dir / "output";
  if (auto performed = step(made); !performed) {
    return std::unexpected(std::move(performed.error()));
  }
  auto output = DigestOfFile(made);
  if (!output) {
    return std::unexpected(std::move(output.error()));
  }
  ActionResult result{.digest = *output, .path = made, .performed = true};
  if (!store) {
    return result;
  }
  const auto kept = EntryPath(*store, kContentDir, *output);
  if (!KeepCopy(made, kept)) {
    return result;
  }
  MarkUsed(kept);
  result.path = kept;
  if (KeepAt(
          EntryPath(*store, kActionCacheDir, digest),
          stable_hash::HexOf(*output))) {
    TrimStore(*store);
  }
  return result;
}

}  // namespace

Action::Action(std::string_view step) {
  Input("step", step);
}

void Action::Input(std::string_view label, std::string_view bytes) {
  hasher_.AddText(label);
  hasher_.AddText(bytes);
}

void Action::InputDigest(
    std::string_view label, const stable_hash::Fingerprint& digest) {
  hasher_.AddText(label);
  hasher_.AddFingerprint(digest);
}

void Action::InputFile(
    std::string_view label, const std::filesystem::path& file) {
  Input(label, ReadWholeFile(file));
}

void Action::InputExecutable(
    std::string_view label, const std::filesystem::path& exe) {
  std::error_code resolve_ec;
  const std::filesystem::path resolved =
      std::filesystem::canonical(exe, resolve_ec);
  // A file that cannot be read answers the same sentinel every time, so it
  // names consistently rather than failing a build that may not need it.
  std::error_code read_ec;
  const std::filesystem::directory_entry file(
      resolve_ec ? exe : resolved, read_ec);
  Input(label, file.path().string());
  Input(
      "changed",
      std::format(
          "{}", file.last_write_time(read_ec).time_since_epoch().count()));
  Input("size", std::format("{}", file.file_size(read_ec)));
}

auto Action::Digest() const -> stable_hash::Fingerprint {
  return hasher_.Finish();
}

auto DigestOfTree(const std::filesystem::path& root)
    -> stable_hash::Fingerprint {
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
  stable_hash::StableHasher hasher;
  hasher.AddNumber(files.size());
  for (const auto& file : files) {
    hasher.AddText(file.lexically_relative(root).generic_string());
    hasher.AddText(ReadWholeFile(file));
  }
  return hasher.Finish();
}

// A thread's claim on performing one action. Settling it hands what the
// action came to to every thread waiting on it; a claim never settled, because
// the step threw, is withdrawn instead, so a waiting thread claims the action
// itself rather than waiting on a result nobody is going to produce.
class ActionCache::Claim {
 public:
  Claim(ActionCache& cache, const stable_hash::Fingerprint& digest)
      : cache_(&cache), digest_(digest) {
  }
  Claim(const Claim&) = delete;
  Claim(Claim&&) = delete;
  auto operator=(const Claim&) -> Claim& = delete;
  auto operator=(Claim&&) -> Claim& = delete;

  ~Claim() {
    if (published_) {
      return;
    }
    {
      const std::scoped_lock lock(cache_->mutex_);
      cache_->in_flight_.erase(digest_);
    }
    cache_->settled_.notify_all();
  }

  void Settle(const diag::Result<ActionResult>& result) {
    {
      const std::scoped_lock lock(cache_->mutex_);
      cache_->in_flight_.at(digest_) = result;
    }
    published_ = true;
    cache_->settled_.notify_all();
  }

 private:
  ActionCache* cache_;
  stable_hash::Fingerprint digest_;
  bool published_ = false;
};

ActionCache::ActionCache(
    std::optional<std::filesystem::path> store, std::filesystem::path scratch,
    bool reuse_kept)
    : store_(std::move(store)),
      scratch_(std::move(scratch)),
      reuse_kept_(reuse_kept) {
}

auto ActionCache::Execute(const Action& action, ActionStep step)
    -> diag::Result<ActionResult> {
  const stable_hash::Fingerprint digest = action.Digest();
  if (store_ && reuse_kept_) {
    if (auto kept = Kept(*store_, digest)) {
      return *std::move(kept);
    }
  }
  if (auto shared = AwaitOrClaim(digest)) {
    return *std::move(shared);
  }
  Claim claim(*this, digest);
  diag::Result<ActionResult> result =
      Perform(store_, scratch_, digest, std::move(step));
  claim.Settle(result);
  return result;
}

// The entry for `digest` is looked for afresh on every wake, because a claim
// withdrawn while this thread waited has erased it, and then this thread is
// the one to claim it.
auto ActionCache::AwaitOrClaim(const stable_hash::Fingerprint& digest)
    -> std::optional<diag::Result<ActionResult>> {
  std::unique_lock lock(mutex_);
  while (true) {
    const auto [found, first] = in_flight_.try_emplace(digest);
    if (first) {
      return std::nullopt;
    }
    if (found->second.has_value()) {
      diag::Result<ActionResult> shared = *found->second;
      if (shared) {
        shared->performed = false;
      }
      return shared;
    }
    settled_.wait(lock);
  }
}

void ActionCache::Evict(const ActionResult& result) const {
  if (!store_ ||
      result.path != EntryPath(*store_, kContentDir, result.digest)) {
    return;
  }
  std::error_code ec;
  std::filesystem::remove(result.path, ec);
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

auto LinkTimeCacheIn(const std::filesystem::path& store, LinkTimeCache layout)
    -> std::filesystem::path {
  switch (layout) {
    case LinkTimeCache::kLlvm:
      return store / kLlvmLinkTimeDir;
    case LinkTimeCache::kGcc:
      return store / kGccLinkTimeDir;
  }
  std::unreachable();
}

auto TemporaryBeside(const std::filesystem::path& target)
    -> std::filesystem::path {
  std::random_device random;
  return target.parent_path() / std::format(
                                    ".{}.tmp.{:08x}{:08x}",
                                    target.filename().string(), random(),
                                    random());
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
        diag::DiagCode::kHostIoError, "cannot copy '{}' to '{}' ({})",
        from.string(), to.string(), ec.message());
  }
  std::filesystem::permissions(
      temporary, std::filesystem::status(from, ec).permissions(),
      std::filesystem::perm_options::replace, ec);
  std::filesystem::rename(temporary, to, ec);
  if (ec) {
    std::error_code ignored;
    std::filesystem::remove(temporary, ignored);
    return diag::Fail(
        diag::DiagCode::kHostIoError, "cannot write '{}' ({})", to.string(),
        ec.message());
  }
  return true;
}

auto ClearStore(const std::filesystem::path& store) -> std::size_t {
  std::vector<std::filesystem::path> entries = EntriesOf(store);
  // GCC's cache is laid out as GCC lays it out, so every file under it goes.
  std::error_code walk_ec;
  for (const auto& entry : std::filesystem::recursive_directory_iterator(
           store / kGccLinkTimeDir, walk_ec)) {
    std::error_code entry_ec;
    if (entry.is_regular_file(entry_ec)) {
      entries.push_back(entry.path());
    }
  }
  std::size_t removed = 0;
  for (const std::filesystem::path& entry : entries) {
    std::error_code ec;
    if (std::filesystem::remove(entry, ec)) {
      ++removed;
    }
  }
  return removed;
}

}  // namespace lyra::driver
