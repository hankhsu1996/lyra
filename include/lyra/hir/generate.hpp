#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/common/scope_types.hpp"
#include "lyra/hir/fwd.hpp"

namespace lyra::hir {

// First-class HIR generate-region tree. Each ModuleBody owns a root
// GenerateRegion. Nested generate-if/for constructs appear as
// GenerateConstruct items inside their enclosing region; each construct
// owns its own child region(s).

enum class RegionItemKind : uint8_t {
  kVariable,
  kNet,
  kParameter,
  kFunction,
  kTask,
  kProcess,
  kInstanceMember,
  kGenerateConstruct,
};

struct RegionItem {
  RegionItemKind kind = RegionItemKind::kVariable;
  std::variant<
      VariableId, NetId, ParameterId, FunctionId, TaskId, ProcessId,
      InstanceMemberId, GenerateConstructId>
      payload;

  auto operator==(const RegionItem&) const -> bool = default;
};

struct GenerateRegion {
  std::vector<RegionItem> items;
  ScopeId scope;
  std::string label;
  std::optional<GenerateRegionId> parent_region;
  bool is_root = false;
};

enum class GenerateConstructKind : uint8_t {
  kConditional,
  kLoop,
};

// N-way alternatives, source order. An if-only has one entry; if/else
// has two; case-generate has one per arm. Pure topology: no condition
// expression, no default marker. Per-arm semantics is a follow-up
// cut's concern.
struct ConditionalGenerateConstruct {
  std::vector<GenerateRegionId> alternatives;

  auto operator==(const ConditionalGenerateConstruct&) const -> bool = default;
};

struct LoopGenerateIteration {
  GenerateRegionId region;
  int64_t elaborated_index = 0;
  std::string label;

  auto operator==(const LoopGenerateIteration&) const -> bool = default;
};

struct LoopGenerateConstruct {
  std::vector<LoopGenerateIteration> iterations;

  auto operator==(const LoopGenerateConstruct&) const -> bool = default;
};

struct GenerateConstruct {
  GenerateConstructKind kind = GenerateConstructKind::kConditional;
  std::variant<ConditionalGenerateConstruct, LoopGenerateConstruct> payload;
};

}  // namespace lyra::hir
