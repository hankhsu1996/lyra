#pragma once

#include <cstdint>
#include <string_view>

#include "lyra/support/scope_execution.hpp"

namespace lyra::support {

// What a definition a scope of the run comes from is, as its record states it
// (LRM 3.3 to 3.6, 3.12, 26.2): a design element, a package, the compilation
// unit's own scope, or the implicit root of the design hierarchy (LRM
// 23.3.1), which no source declares. The runtime reads it wherever a scope is
// asked what it is.
enum class DefinitionKind : std::uint8_t {
  kModule,
  kInterface,
  kProgram,
  kPackage,
  kCompilationUnit,
  kDesignRoot,
};

// The name of the implicit root of the design hierarchy (LRM 23.3.1), which
// the run's own root scope answers and the unit that builds it is called. The
// leading `$` keeps it distinct from every source unit (an SV identifier
// cannot begin with `$`); a backend maps it to a target-language identifier
// when emitting.
inline constexpr std::string_view kDesignRootName = "$root";

// How what an instance of a definition of `kind` starts is run: a program's in
// the reactive region set and every other's in the active one (LRM 24.3.1).
[[nodiscard]] auto ExecutionOf(DefinitionKind kind) -> ScopeExecution;

// What a declaration of a scope inside another is where the run builds the
// scopes it declares (LRM 23.3.2, 27): an instance of a module, an interface or
// a program, an array of them, a block a generate builds and the blocks a loop
// generate builds; and the implicit root of the design hierarchy (LRM
// 23.3.1), which the runtime library declares since no source does.
enum class ElaboratedScopeKind : std::uint8_t {
  kModule,
  kInterface,
  kProgram,
  kModuleArray,
  kInterfaceArray,
  kProgramArray,
  kGenerateBlock,
  kGenerateLoop,
  kDesignRoot,
};

// What a declaration of a scope inside another is where the run builds
// nothing for it, the declaration being the whole of the scope (LRM 23.9,
// 37.12): a block statement by how it joins its branches (LRM 9.3), a loop
// declaring its own variables (LRM 12.7.1, 12.7.3), a subroutine, a class and
// a clocking block.
enum class LexicalScopeKind : std::uint8_t {
  kBegin,
  kForkJoin,
  kForkJoinAny,
  kForkJoinNone,
  kFor,
  kForeach,
  kTask,
  kFunction,
  kClass,
  kClockingBlock,
};

// The net type `default_nettype` gives an implicit net of a definition, or
// none (LRM 22.8).
enum class DefaultNetType : std::uint8_t {
  kWire,
  kTri,
  kTri0,
  kTri1,
  kWand,
  kTriand,
  kWor,
  kTrior,
  kTrireg,
  kUwire,
  kNone,
};

// The pull `unconnected_drive` gives an unconnected input port of a
// definition, or none (LRM 22.9).
enum class UnconnectedDrive : std::uint8_t {
  kNone,
  kPull0,
  kPull1,
};

}  // namespace lyra::support
