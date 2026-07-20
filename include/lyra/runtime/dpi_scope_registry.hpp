#pragma once

#include <string>
#include <string_view>
#include <unordered_map>

namespace lyra::runtime {

class Scope;

// The DPI scope directory for a running simulation (LRM 35.5.3, Annex H). Every
// instance / generate scope addressable by a fully qualified name is a valid
// `svScope` handle; the directory keys that relation both ways so
// `svGetScopeFromName` resolves a name and `svGetNameFromScope` recovers a
// stable name. It also holds the per-scope user-data store
// (`svPutUserData` / `svGetUserData`). The hierarchy is built once from the
// design root and read-only after; the user-data store is mutable across the
// run. A handle is valid iff it is a registered scope.
class DpiScopeRegistry {
 public:
  explicit DpiScopeRegistry(Scope* root);

  [[nodiscard]] auto IsValidHandle(const Scope* scope) const -> bool;

  // The fully qualified name of a valid handle (the `%m` form, LRM 21.2.1.1),
  // owned by the directory and stable for the run; null for an unregistered
  // handle.
  [[nodiscard]] auto NameOf(const Scope* scope) const -> const char*;

  // The scope registered under a fully qualified name; null if no scope carries
  // that name. Non-const because the result is an opaque `svScope` handle the
  // foreign side holds, not a read-only view.
  [[nodiscard]] auto ScopeOfName(std::string_view name) const -> Scope*;

  // `svPutUserData` (Annex H): store `data` under `key` for `scope`. Returns 0
  // on success, -1 for an invalid scope or a null key.
  auto PutUserData(const Scope* scope, void* key, void* data) -> int;

  // `svGetUserData` (Annex H): the pointer previously stored under `key` for
  // `scope`, or null for an invalid scope, a null key, or an absent entry.
  [[nodiscard]] auto GetUserData(const Scope* scope, void* key) const -> void*;

 private:
  void RegisterSubtree(Scope& scope);

  std::unordered_map<const Scope*, std::string> scope_to_path_;
  std::unordered_map<std::string_view, Scope*> path_to_scope_;
  std::unordered_map<const Scope*, std::unordered_map<void*, void*>> user_data_;
};

}  // namespace lyra::runtime
