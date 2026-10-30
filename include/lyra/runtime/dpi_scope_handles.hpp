#pragma once

#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>

namespace lyra::runtime {

class Scope;

// The scopes a running simulation has handed to foreign code as `svScope`
// handles (LRM 35.5.3, Annex H.9.3), with what foreign code asked of each.
// Foreign code obtains a handle only from the C layer -- the current scope, the
// one a replacement displaced, the one a name finds -- so a handle is valid iff
// it was handed out here, which is checked without reading through a pointer
// the foreign side supplied.
//
// Nothing is walked or named ahead of a question, so a run that asks nothing
// pays nothing for being askable: a handle's name is composed the first time it
// is asked for and kept for the rest of the run, since the foreign side holds
// the characters it was given, and a name is found by descending the tree from
// the root along the parts the name is made of.
class DpiScopeHandles {
 public:
  explicit DpiScopeHandles(Scope& root);

  // Records `scope` as handed to foreign code and answers it; null, which is
  // no scope, is answered as it is.
  auto Issue(Scope* scope) -> Scope*;

  [[nodiscard]] auto IsIssued(const Scope* scope) const -> bool;

  // The fully qualified name of a handed-out scope (the `%m` form, LRM
  // 21.2.1.5), stable for the run; null for a handle never handed out.
  [[nodiscard]] auto NameOf(const Scope* scope) -> const char*;

  // The scope a fully qualified name names, or null where none carries it
  // (LRM Annex H.9.3). Finding one does not hand it out.
  [[nodiscard]] auto ScopeOfName(std::string_view name) const -> Scope*;

  // `svPutUserData` (Annex H): store `data` under `key` for `scope`. Returns 0
  // on success, -1 for a handle never handed out or a null key.
  auto PutUserData(const Scope* scope, void* key, void* data) -> int;

  // `svGetUserData` (Annex H): the pointer previously stored under `key` for
  // `scope`, or null where nothing was. Only a handed-out scope and a non-null
  // key are ever stored under, so every invalid pair finds nothing.
  [[nodiscard]] auto GetUserData(const Scope* scope, void* key) const -> void*;

 private:
  Scope* root_;
  // Each scope handed out, with its name once it has been asked for.
  std::unordered_map<const Scope*, std::optional<std::string>> issued_;
  std::unordered_map<const Scope*, std::unordered_map<void*, void*>> user_data_;
};

}  // namespace lyra::runtime
