#pragma once

#include <cstdint>
#include <string>
#include <variant>

#include "lyra/support/scope_kind.hpp"

namespace lyra::hir {

// What kind of design element a definition is (LRM 3.3 to 3.5).
enum class DesignElementKind : std::uint8_t {
  kModule,
  kInterface,
  kProgram,
};

// A design element's definition, with the directives in force where it was
// written: whether `celldefine marks it (LRM 22.10), the net type of an
// implicit net (LRM 22.8), the pull of an unconnected input (LRM 22.9), and
// whether its subroutines are automatic by default (LRM 23.2.1).
struct DesignElementDefinition {
  DesignElementKind kind = DesignElementKind::kModule;
  bool celldefine = false;
  support::DefaultNetType default_net_type = support::DefaultNetType::kWire;
  support::UnconnectedDrive unconnected_drive =
      support::UnconnectedDrive::kNone;
  bool automatic = false;

  auto operator==(const DesignElementDefinition&) const -> bool = default;
};

// A package's definition (LRM 26), with the directives that reach a package:
// the net type of an implicit net and whether its subroutines are automatic
// by default.
struct PackageDefinition {
  support::DefaultNetType default_net_type = support::DefaultNetType::kWire;
  bool automatic = false;

  auto operator==(const PackageDefinition&) const -> bool = default;
};

// The compilation unit's own scope (LRM 3.12.1).
struct CompilationUnitScope {
  auto operator==(const CompilationUnitScope&) const -> bool = default;
};

// A specialization of a parameterized class lowered on its own (LRM 8.25).
struct ClassSpecialization {
  auto operator==(const ClassSpecialization&) const -> bool = default;
};

// A text of Lyra's own that it reads in beside the design: its definitions of
// the built-in package `std`'s classes (LRM 26.7), and what a covergroup is
// counted by.
struct ToolText {
  auto operator==(const ToolText&) const -> bool = default;
};

// The implicit root of the design hierarchy (LRM 23.3.1), which no source
// declares.
struct DesignRoot {
  auto operator==(const DesignRoot&) const -> bool = default;
};

using DefinitionForm = std::variant<
    DesignElementDefinition, PackageDefinition, CompilationUnitScope,
    ClassSpecialization, ToolText, DesignRoot>;

// What the source states of the definition a unit is lowered from, which
// every scope of the run that comes from it answers (LRM 37.10): the library
// it was read from (LRM 33.3) and the cell its text lies in (LRM 33.2.1),
// which together are what a `%l` written anywhere in it prints (LRM 33.7), and
// what kind of definition it is. Its name is the unit's source name.
struct UnitDefinition {
  std::string library;
  std::string cell;
  DefinitionForm form;

  auto operator==(const UnitDefinition&) const -> bool = default;
};

// Whether a unit of `definition` roots an object, as a design element and the
// design's root do; a package, the compilation unit's scope, a class lowered
// alone and a text of the tool's hold declarations reached with no instance
// (LRM 26, 3.12.1, 8.25).
[[nodiscard]] auto RootsAnObject(const UnitDefinition& definition) -> bool;

// Whether the procedures a unit of `definition` declares are a program's,
// which run in the reactive region set and end the run with them (LRM 24.3,
// 24.7).
[[nodiscard]] auto IsProgram(const UnitDefinition& definition) -> bool;

// Whether a unit of `definition` is one of the run's namespace objects (LRM
// 37.10): a package, or the compilation unit's scope, which VPI shows as a
// package marked as a unit (LRM 37.10 detail 5).
[[nodiscard]] auto IsNamespaceObject(const UnitDefinition& definition) -> bool;

}  // namespace lyra::hir
