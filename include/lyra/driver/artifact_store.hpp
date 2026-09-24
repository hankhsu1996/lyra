#pragma once

#include <cstddef>
#include <cstdint>
#include <filesystem>
#include <optional>
#include <string>
#include <string_view>

#include "lyra/diag/diagnostic.hpp"

namespace lyra::driver {

// The name an entry of the store is kept under, computed from everything the
// step that produced it read. Two entries with one name are equivalent by
// construction, so a name that matches is a content that matches.
struct ContentName {
  std::string hex;
};

// Accumulates what an entry's name is computed from. Every piece is framed by
// its label and its length, so two different sequences of pieces never
// produce the same stream, and the stream is digested twice, by independent
// functions, into a name wide enough that two different programs never share
// one in practice.
class ContentNamer {
 public:
  void Add(std::string_view label, std::string_view bytes);
  void AddFile(std::string_view label, const std::filesystem::path& file);
  // Every regular file under `root`, each under its path relative to it, in
  // a fixed order -- so adding, removing, renaming, or editing any file there
  // changes the name.
  void AddTree(std::string_view label, const std::filesystem::path& root);
  // A program named by path, identified by the file it resolves to, when that
  // file last changed, and how large it is: another installation, and an
  // upgrade in place of the same one, both change the name.
  void AddExecutable(std::string_view label, const std::filesystem::path& exe);

  [[nodiscard]] auto Finish() const -> ContentName;

 private:
  void Mix(std::string_view bytes);

  std::uint64_t stream_digest_ = 0xcbf29ce484222325ULL;
  std::size_t piece_digest_ = 0;
};

// Where the store is: `override` when given, and otherwise `lyra` under the
// platform's cache directory. Absent when the platform names none, and then
// nothing is kept -- the store only ever makes a build faster, so having none
// makes it slower and nothing else.
//
// Nothing is created here. Whoever keeps an entry makes the directories it
// writes into, so a command that keeps nothing leaves no trace of having asked.
auto LocateStore(const std::optional<std::filesystem::path>& override)
    -> std::optional<std::filesystem::path>;

// The two kinds of entry the store keeps, each under a directory of its own.
inline constexpr std::string_view kStoredHeaderDir = "pch";
inline constexpr std::string_view kStoredProgramDir = "programs";

// The entry kept under `name`, marked as used just now, or nothing.
auto FindStored(
    const std::filesystem::path& store, std::string_view kind,
    const ContentName& name) -> std::optional<std::filesystem::path>;

// Copies the entry kept under `name` to `to`, marking it used, and answers
// whether one was kept. An entry cleared or trimmed by another process while
// this runs was not kept, so the answer is the same whenever it went.
auto CopyStored(
    const std::filesystem::path& store, std::string_view kind,
    const ContentName& name, const std::filesystem::path& to)
    -> diag::Result<bool>;

// Keeps a copy of `built` under `name`. The copy is written under a name of
// its own and renamed into place, so a concurrent reader sees a whole entry or
// none, and two writers of one name each leave a whole entry behind. Also
// trims the store, at most once a day.
//
// Keeping is what makes the next build faster, so a copy that cannot be kept
// makes that build slower and fails nothing, which is why nothing is answered.
void KeepStored(
    const std::filesystem::path& store, std::string_view kind,
    const ContentName& name, const std::filesystem::path& built);

// The path an entry of `kind` is kept at under `name`, whether or not it is
// there. For a writer that produces the entry in place, through a temporary it
// renames onto this path.
auto StoredPath(
    const std::filesystem::path& store, std::string_view kind,
    const ContentName& name) -> std::filesystem::path;

// A name to write `target` under before renaming it into place: unique to this
// writer, and beside `target`, since a rename is atomic only within one
// filesystem.
auto TemporaryBeside(const std::filesystem::path& target)
    -> std::filesystem::path;

// Copies `from` to `to` through a temporary renamed into place, so `to` is
// never seen half written. Answers false when there is no `from` to copy.
auto CopyOut(const std::filesystem::path& from, const std::filesystem::path& to)
    -> diag::Result<bool>;

// Removes every entry unused for five days, at most once a day. A concurrent
// build reading an entry either copies it out, and then finds none and builds,
// or hands it to the compiler as a prepared header, which is an attempt a
// compile survives losing. Removing one costs that build time and nothing more.
void TrimStore(const std::filesystem::path& store);

// Removes every entry, answering how many were removed.
auto ClearStore(const std::filesystem::path& store) -> std::size_t;

}  // namespace lyra::driver
