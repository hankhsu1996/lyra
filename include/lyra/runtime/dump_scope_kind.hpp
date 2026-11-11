#pragma once

#include <cstdint>
#include <string_view>

#include "lyra/support/scope_kind.hpp"

namespace lyra::runtime {

// What a scope is recorded as in a value dump (LRM 21.7.2.1 `scope_type`).
enum class DumpScopeKind : std::uint8_t {
  kModule,
  kTask,
  kFunction,
  kBegin,
  kFork,
};

auto DumpScopeKindKeyword(DumpScopeKind kind) -> std::string_view;

// What a dump records a scope declared as `kind` as: an instance as a
// module, a generate block or a loop declaring its own variables as a
// `begin`, and a block statement or a subroutine as itself.
auto DumpScopeKindOf(support::ElaboratedScopeKind kind) -> DumpScopeKind;
auto DumpScopeKindOf(support::LexicalScopeKind kind) -> DumpScopeKind;

}  // namespace lyra::runtime
