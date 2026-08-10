#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <format>
#include <span>
#include <string>
#include <variant>
#include <vector>

#include "lyra/backend/cpp/api.hpp"
#include "lyra/backend/cpp/artifact.hpp"
#include "lyra/backend/cpp/formatting.hpp"
#include "lyra/backend/cpp/render_decl.hpp"
#include "lyra/backend/cpp/render_type.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/runtime_prelude.hpp"

namespace lyra::backend::cpp {

namespace {

auto CollectExternalUnitNames(const mir::CompilationUnit& unit)
    -> std::vector<std::string> {
  std::vector<std::string> names;
  const auto add = [&](const std::string& name) {
    if (std::ranges::find(names, name) == names.end()) {
      names.push_back(name);
    }
  };
  // A unit an instance is built from names its unit through the child object's
  // `ExternalUnitObjectType`; a unit whose namespace symbol is reached by name
  // (a receiver-less callable or a package variable) names its unit in the
  // reference-dependency list, since such a reference interns no such type;
  // a unit a class is referenced from -- as a handle, a `new`, a field /
  // method / static access, or a base extension -- names its unit in the
  // class-dependency list. All three are external units this unit's artifact
  // includes.
  for (const auto& t : unit.types) {
    if (const auto* ext = std::get_if<mir::ExternalUnitObjectType>(&t.data)) {
      add(ext->unit_name);
    }
  }
  for (const std::string& name : unit.external_referenced_units) {
    add(name);
  }
  for (const std::string& name : unit.external_class_units) {
    add(name);
  }
  return names;
}

// Whether the unit defines a symbol in the program-global DPI-C name space --
// an export's entry point (LRM 35.7). A unit whose only consumer is foreign C
// has no SV referrer to pull its header into the include graph, so the program
// entry must include it directly for that definition to land and link. An
// import's prototype is a declaration, not a definition, and pulls nothing.
auto DefinesForeignSymbol(const mir::CompilationUnit& unit) -> bool {
  return std::ranges::any_of(unit.callables, [](const auto& callable) {
    return callable.foreign.has_value() && callable.code.body.has_value();
  });
}

// The include preamble every emitted unit header shares: the runtime umbrella
// naming everything a rendered body may call into, and one include per
// external unit this unit references (instantiated or called), so a cross-unit
// name resolves against the other unit's emitted header. Naming the umbrella
// rather than the individual headers is what keeps the emit's include set and
// the precompiled header's coverage the same set.
auto RenderUnitIncludes(const mir::CompilationUnit& unit) -> std::string {
  std::string out;
  out += std::format("#include \"{}\"\n", support::kRuntimePreludeHeader);
  for (const auto& name : CollectExternalUnitNames(unit)) {
    out += std::format("#include \"{}.hpp\"\n", ToCppName(name));
  }
  return out;
}

// A package variable is one program-global observable cell (LRM 26.2). C++17
// `inline` gives it a single definition across every translation unit that
// includes the header, matching the header-only, link-by-name model the
// namespace callables use. A unit rooted in a design element declares none: its
// storage is per-instance.
auto RenderUnitStaticVariables(const mir::CompilationUnit& unit)
    -> std::string {
  std::string out;
  for (std::size_t i = 0; i < unit.static_variables.size(); ++i) {
    const auto& var = unit.static_variables.Get(
        mir::StaticVariableId{static_cast<std::uint32_t>(i)});
    out += std::format(
        "inline {} {}{{}};\n", RenderTypeAsCpp(unit, var.type), var.name);
  }
  return out;
}

// A unit's C++ peer is a namespace holding everything the unit declares. That
// is the unit boundary made literal: inside it every class the unit owns is
// reached by the one name it carries, and outside it every reference qualifies
// by the unit -- the same two forms whether the unit is rooted in a design
// element or is a rootless package.
auto RenderUnitHeaderFile(const mir::CompilationUnit& unit) -> std::string {
  const UnitCallableText callables = RenderUnitCallables(unit);
  const ClassText classes = RenderUnitClasses(unit);
  std::string out;
  out += "#pragma once\n";
  out += RenderUnitIncludes(unit);
  out += "\n";
  out += std::format("namespace {} {{\n", ToCppName(unit.name));
  AppendSection(out, callables.declarations);
  AppendSection(out, RenderUnitStaticVariables(unit));
  AppendSection(out, classes.declaration);
  AppendSection(out, classes.definitions);
  AppendSection(out, callables.definitions);
  out += std::format("\n}}  // namespace {}\n", ToCppName(unit.name));
  return out;
}

// The program entry. A design's whole contribution to it is the class its
// `$root` is an instance of and the label that root carries, so that is all
// this writes; every invariant host-boundary concern is behind the runtime
// entry it hands off to, and a new one is added there rather than here. The
// includes are the design's own: each unit whose header the entry must pull in
// for its definitions to land, which is the root's and any unit defining a
// symbol only foreign C refers to.
auto RenderHostMain(
    std::span<const mir::CompilationUnit> units,
    const mir::CompilationUnit& root) -> std::string {
  const auto& root_class = root.GetClass(*root.root);
  std::string out;
  out += std::format("#include \"{}\"\n", support::kHostEntryHeader);
  for (const auto& unit : units) {
    if (DefinesForeignSymbol(unit)) {
      out += std::format("#include \"{}.hpp\"\n", ToCppName(unit.name));
    }
  }
  out += std::format("#include \"{}.hpp\"\n", ToCppName(root.name));
  out += "\n";
  out += "auto main(int argc, char** argv) -> int {\n";
  out += std::format(
      "  return lyra::runtime::RunDesign<{}::{}>(argc, argv, \"{}\");\n",
      ToCppName(root.name), ToCppName(root_class.name), root_class.name);
  out += "}\n";
  return out;
}

auto EmitCppDeclarations(const mir::CompilationUnit& unit) -> CppArtifact {
  return {
      .relpath = std::format("{}.hpp", ToCppName(unit.name)),
      .content = RenderUnitHeaderFile(unit)};
}

auto EmitCppHostMain(
    std::span<const mir::CompilationUnit> units,
    const mir::CompilationUnit& root) -> CppArtifact {
  return {.relpath = "main.cpp", .content = RenderHostMain(units, root)};
}

}  // namespace

auto EmitCpp(
    std::span<const mir::CompilationUnit> units,
    const mir::CompilationUnit& root) -> CppArtifactSet {
  CppArtifactSet set;
  for (const auto& unit : units) {
    set.files.push_back(EmitCppDeclarations(unit));
  }
  set.files.push_back(EmitCppDeclarations(root));
  set.files.push_back(EmitCppHostMain(units, root));
  return set;
}

}  // namespace lyra::backend::cpp
