#pragma once

#include <cstdint>
#include <string>
#include <type_traits>
#include <vector>

namespace lyra::runtime {

struct RuntimeScope;

enum class RuntimeScopeKind : uint8_t {
  kInstance,
  kGenerate,
};

struct RuntimeChildEdge {
  uint32_t ordinal_in_parent = 0;
  RuntimeScope* child = nullptr;
};

// Common hierarchy base for all runtime scope nodes (module instances and
// generate scopes). Not a base class -- embedded as a member in
// RuntimeInstance and RuntimeGenerateScope. No virtual dispatch.
//
// Owns hierarchical path string, parent/child edges, and scope identity.
// Instance-specific payload (storage, body, processes, observability) lives
// on RuntimeInstance, not here.
struct RuntimeScope {
  RuntimeScopeKind kind = RuntimeScopeKind::kInstance;
  RuntimeScope* parent = nullptr;
  std::vector<RuntimeChildEdge> children;
  std::string path_storage;
  const char* path_c_str = nullptr;
  uint32_t ordinal_in_parent = 0;
};

// Generate scope node: pure hierarchy intermediate with no storage, body,
// processes, or observability. Exists only to give generate scopes a real
// runtime identity and correct hierarchical path.
struct RuntimeGenerateScope {
  RuntimeScope scope;
};

// Lock down embedded-scope representation across both node kinds.
// RuntimeInstance asserts the same property in runtime_instance.hpp once
// its layout is visible. Together these guarantee that scope is always
// the first member of its enclosing struct and container_of recovery is
// well-defined.
static_assert(std::is_standard_layout_v<RuntimeScope>);
static_assert(std::is_standard_layout_v<RuntimeGenerateScope>);

// Recover the containing RuntimeInstance from an instance-kind scope
// pointer. Uses the standard container_of pattern. Checks the precondition
// scope != nullptr && scope->kind == kInstance on entry. Defined inline
// in runtime_instance.hpp once RuntimeInstance layout is visible.
struct RuntimeInstance;
auto ScopeAsInstanceChecked(RuntimeScope* scope) -> RuntimeInstance*;
auto ScopeAsInstanceChecked(const RuntimeScope* scope)
    -> const RuntimeInstance*;

}  // namespace lyra::runtime
