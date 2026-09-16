#include <algorithm>
#include <format>
#include <string>
#include <vector>

#include "lyra/backend/cpp/api.hpp"
#include "lyra/backend/cpp/artifact.hpp"
#include "lyra/backend/cpp/formatting.hpp"
#include "lyra/backend/cpp/naming.hpp"
#include "lyra/backend/cpp/render_decl.hpp"
#include "lyra/backend/cpp/render_expr.hpp"
#include "lyra/backend/cpp/render_type.hpp"
#include "lyra/backend/cpp/scope_view.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/packed_type_descriptor.hpp"
#include "lyra/mir/type_id.hpp"
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
  // A unit whose object this one reaches -- an instance it builds, a port it
  // connects, a published member it names -- has a record of that object here;
  // a unit whose namespace symbol is reached by name (a receiver-less callable
  // or a package variable) names its unit in the reference-dependency list,
  // since such a reference reaches no object; a unit this one reaches into a
  // class of -- a `new`, a field / method / static access, or a base extension
  // -- names its unit in the class-dependency list. All three are external
  // units whose definitions this unit's artifact needs and so includes.
  for (const mir::ExternalUnitObject& object : unit.external_unit_objects) {
    add(object.unit_name);
  }
  for (const std::string& name : unit.external_referenced_units) {
    add(name);
  }
  for (const std::string& name : unit.external_class_units) {
    add(name);
  }
  return names;
}

// The include preamble every emitted unit header shares: the runtime umbrella
// naming everything a rendered body may call into, and one include per external
// unit whose definitions this one needs, so a name reaching into another unit
// resolves against that unit's emitted header. Naming the umbrella
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

// What each of the unit's types is described by, one definition per described
// type, ahead of any code that names one.
auto RenderPackedTypeDescriptions(const mir::CompilationUnit& unit)
    -> std::string {
  std::string out;
  for (const mir::TypeId id : mir::DescribedPackedTypes(unit)) {
    const mir::PackedTypeDescription described =
        mir::DescribePackedType(unit, id);
    const ScopeView view = ScopeView::ForUnitConstant(unit, described.body);
    out += NamespaceConstantOf(
        RenderTypeAsCpp(unit, unit.builtins.packed_type), CppPackedTypeName(id),
        RenderExpr(view, view.Expr(described.value)));
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
  for (const mir::StaticVariableId id : unit.static_variables.Ids()) {
    out += std::format(
        "inline {} {}{{}};\n",
        RenderTypeAsCpp(unit, unit.static_variables.Get(id).type),
        CppStaticVariableName(unit.named_static_variables, id));
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
  std::string body;
  AppendSection(body, callables.declarations);
  AppendSection(body, RenderPackedTypeDescriptions(unit));
  AppendSection(body, RenderUnitStaticVariables(unit));
  AppendSection(body, classes.declaration);
  AppendSection(body, classes.definitions);
  AppendSection(body, callables.definitions);
  AppendSection(body, RenderForeignScopeSymbols(unit));
  body += "\n";

  std::string out;
  out += "#pragma once\n";
  out += RenderUnitIncludes(unit);
  out += "\n";
  out += NamespaceBlockOf(UnitNamespaceOf(unit.name), body);
  return out;
}

// The program entry. A design's whole contribution to it is the class its
// `$root` is an instance of and the label that root carries, so that is all
// this writes; every invariant host-boundary concern is behind the runtime
// entry it hands off to, and a new one is added there rather than here. It
// includes the design root's header and nothing else: a symbol only foreign C
// calls is defined by the unit that declares it, and the root reaches every
// such unit -- the namespaces it brings up, and the design elements it builds.
auto RenderHostMain(const mir::CompilationUnit& root) -> std::string {
  const mir::RootedTree* tree = mir::RootedTreeOf(root);
  if (tree == nullptr) {
    throw InternalError("backend::cpp: the design root roots no tree");
  }
  const mir::Class& root_class = root.GetClass(tree->root);

  std::string out;
  out += std::format("#include \"{}\"\n", support::kHostEntryHeader);
  out += std::format("#include \"{}.hpp\"\n", ToCppName(root.name));
  out += "\n";
  out += "auto main(int argc, char** argv) -> int {\n";
  out += std::format(
      "  return lyra::runtime::RunDesign<{}::{}>(argc, argv, \"{}\");\n",
      UnitNamespaceOf(root.name), CppClassName(root_class, tree->root),
      root.name);
  out += "}\n";
  return out;
}

}  // namespace

auto EmitCppUnit(const mir::CompilationUnit& unit) -> CppArtifact {
  return {
      .relpath = std::format("{}.hpp", ToCppName(unit.name)),
      .content = RenderUnitHeaderFile(unit)};
}

auto EmitCppHostMain(const mir::CompilationUnit& root) -> CppArtifact {
  return {.relpath = "main.cpp", .content = RenderHostMain(root)};
}

}  // namespace lyra::backend::cpp
