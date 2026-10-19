#pragma once

#include <cstdint>
#include <string_view>

namespace lyra::support {

// What a declaration is recorded as in a value dump (LRM 21.7.2.1
// `var_type`): its net type for a net, and for a variable the kind of data it
// holds. The front end, which reads the declaration, states it, and the
// runtime writes it into the file, so it lives beside both. A parameter is no
// storage, so nothing is recorded as one.
enum class DumpVarKind : std::uint8_t {
  kEvent,
  kInteger,
  kReal,
  kRealtime,
  kReg,
  kSupply0,
  kSupply1,
  kTime,
  kTri,
  kTriand,
  kTrior,
  kTrireg,
  kTri0,
  kTri1,
  kWand,
  kWire,
  kWor,
};

// The keyword LRM 21.7.2.1 spells the kind with.
auto DumpVarKindKeyword(DumpVarKind kind) -> std::string_view;

// What a dump records of a thing of a kind (LRM 21.7.2.1 `value`): a vector of
// 0, 1, x and z, a real number, or that an event was triggered.
enum class DumpValueForm : std::uint8_t {
  kBits,
  kReal,
  kTriggered,
};

auto DumpValueFormOf(DumpVarKind kind) -> DumpValueForm;

// What a scope is recorded as in a value dump (LRM 21.7.2.1 `scope_type`).
enum class DumpScopeKind : std::uint8_t {
  kModule,
  kTask,
  kFunction,
  kBegin,
  kFork,
};

auto DumpScopeKindKeyword(DumpScopeKind kind) -> std::string_view;

// The direction a port is declared with, which is what an extended value
// change dump reads a value it records by (LRM 21.7.4.3.1): one the test
// fixture drives into an input, one the device drives out of an output, and
// one an inout carries either way.
enum class DumpPortDirection : std::uint8_t {
  kInput,
  kOutput,
  kInOut,
};

}  // namespace lyra::support
