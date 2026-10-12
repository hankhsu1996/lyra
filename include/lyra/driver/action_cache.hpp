#pragma once

#include <condition_variable>
#include <cstddef>
#include <filesystem>
#include <functional>
#include <map>
#include <mutex>
#include <optional>
#include <string_view>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/stable_hash/fingerprint.hpp"
#include "lyra/stable_hash/stable_hasher.hpp"

namespace lyra::driver {

// One step of a build applied to what it reads: the step's name, then every
// input under a label of its own. Every piece is framed by its label and its
// length, so two different sequences of inputs never produce the same stream,
// and the action is known by that stream's digest. Two actions with one digest
// are the same step on the same inputs, so whatever one produced the other
// would have.
class Action {
 public:
  explicit Action(std::string_view step);

  void Input(std::string_view label, std::string_view bytes);
  void InputDigest(
      std::string_view label, const stable_hash::Fingerprint& digest);
  void InputFile(std::string_view label, const std::filesystem::path& file);
  // Every regular file under `root`, each under its path relative to it, in
  // a fixed order -- so adding, removing, renaming, or editing any file there
  // changes the action.
  void InputTree(std::string_view label, const std::filesystem::path& root);
  // A program named by path, identified by the file it resolves to, when that
  // file last changed, and how large it is: another installation, and an
  // upgrade in place of the same one, both change the action.
  void InputExecutable(
      std::string_view label, const std::filesystem::path& exe);

  [[nodiscard]] auto Digest() const -> stable_hash::Fingerprint;

 private:
  stable_hash::StableHasher hasher_;
};

// What performing an action produced: the digest of its output's bytes, where
// those bytes are now, and whether this build performed it -- the difference
// between an action that cost the build its work and one that cost it nothing.
struct ActionResult {
  stable_hash::Fingerprint digest;
  std::filesystem::path path;
  bool performed = false;
};

// Performs an action, writing its output at the path it is handed.
using ActionStep =
    std::move_only_function<diag::Result<void>(const std::filesystem::path&)>;

// The results of every action a build asks for, kept across builds in two
// stores under one directory, as Bazel's remote execution API and LLVM's CAS
// keep them: the action cache, from an action's digest to the digest of its
// output, and the content-addressable storage, from a digest to the bytes it
// is the digest of. Both are written once and never changed, so nothing one
// build does can change what another reads, and identical outputs of different
// actions are kept once.
//
// With no store every action is performed, into this build's scratch
// directory, and nothing is kept: the store only ever makes a build faster.
class ActionCache {
 public:
  ActionCache(
      std::optional<std::filesystem::path> store, std::filesystem::path scratch,
      bool reuse_kept);

  // The result of `action`: the kept one when one is kept with its output
  // still there and taking one is allowed, the one another thread of this
  // build is performing when there is one, and otherwise `step` performed now
  // and kept. An action is performed at most once in a build, however many ask
  // for it.
  auto Execute(const Action& action, ActionStep step)
      -> diag::Result<ActionResult>;

  // Whether what is performed is kept for a later build.
  [[nodiscard]] auto Keeps() const -> bool {
    return store_.has_value();
  }

 private:
  std::optional<std::filesystem::path> store_;
  std::filesystem::path scratch_;
  bool reuse_kept_;
  std::mutex mutex_;
  std::condition_variable settled_;
  // What each action this build has asked for came to, once it has come to
  // something.
  std::map<stable_hash::Fingerprint, std::optional<diag::Result<ActionResult>>>
      in_flight_;
};

// Where the store is: `override` when given, and otherwise `lyra` under the
// platform's cache directory. Absent when the platform names none, and then
// nothing is kept.
//
// Nothing is created here. Whoever keeps an entry makes the directories it
// writes into, so a command that keeps nothing leaves no trace of having asked.
auto LocateStore(const std::optional<std::filesystem::path>& override)
    -> std::optional<std::filesystem::path>;

// Drops one kept output, so that the next action asking for it performs the
// step again. For the caller that has just watched a consumer refuse those
// bytes, which is the only evidence there is that they had gone stale.
void Discard(const std::filesystem::path& output);

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
// build reading an entry either finds it whole, or finds it gone and performs
// the action. Removing one costs that build time and nothing more.
void TrimStore(const std::filesystem::path& store);

// Removes every entry, answering how many were removed.
auto ClearStore(const std::filesystem::path& store) -> std::size_t;

}  // namespace lyra::driver
