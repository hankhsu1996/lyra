#include <algorithm>
#include <format>
#include <string>
#include <string_view>
#include <variant>
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
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/integral_constant_id.hpp"
#include "lyra/mir/type_descriptor.hpp"
#include "lyra/mir/type_descriptor_id.hpp"
#include "lyra/mir/value_build.hpp"
#include "lyra/support/runtime_prelude.hpp"

namespace lyra::backend::cpp {

namespace {

// Each unit the emission will write an include for, held once however many
// references reach it. The name is a key here and is spelled where the include
// line that names it is written.
void CollectUnitName(
    std::vector<std::string>& names, const std::string& unit_name) {
  if (std::ranges::find(names, unit_name) == names.end()) {
    names.push_back(unit_name);
  }
}

// The units this one extends a class of. A base must be complete where the
// derived class is declared (LRM 8.13, and 8.26 for an interface class), so
// this is the one cross-unit name a declaration cannot reach through a pointer:
// what the other unit declared has to be in scope beside this unit's own
// declarations rather than named without its contents.
auto CollectBaseUnitNames(const mir::CompilationUnit& unit)
    -> std::vector<std::string> {
  std::vector<std::string> names;
  const auto collect_if_cross_unit = [&](const mir::ClassRef& ref) {
    if (const auto* cross = std::get_if<mir::CrossUnitClassRef>(&ref)) {
      CollectUnitName(names, cross->unit_name);
    }
  };
  for (const mir::ClassId id : unit.classes.Ids()) {
    const mir::Class& cls = unit.GetClass(id);
    if (cls.base.has_value()) {
      collect_if_cross_unit(*cls.base);
    }
    for (const mir::ClassRef& contract : cls.implements) {
      collect_if_cross_unit(contract);
    }
  }
  return names;
}

auto CollectExternalUnitNames(const mir::CompilationUnit& unit)
    -> std::vector<std::string> {
  std::vector<std::string> names;
  // A unit whose object this one reaches -- an instance it builds, a port it
  // connects, a published member it names -- has a record of that object here;
  // a unit whose namespace symbol is reached by name (a receiver-less callable
  // or a package variable) names its unit in the reference-dependency list,
  // since such a reference reaches no object; a unit this one reaches into a
  // class of -- a `new`, a field / method / static access, or a base extension
  // -- names its unit in the class-dependency list. All three are external
  // units whose definitions a body of this one needs and so includes.
  for (const mir::ExternalUnitObject& object : unit.external_unit_objects) {
    CollectUnitName(names, object.unit_name);
  }
  for (const std::string& name : unit.external_referenced_units) {
    CollectUnitName(names, name);
  }
  for (const std::string& name : unit.external_class_units) {
    CollectUnitName(names, name);
  }
  return names;
}

// One value the unit settles before the program runs, written as a constant of
// the namespace: the type it has, the name it is reached by, and the expression
// that builds it. The build is an expression tree with no statements, so what
// renders it is the ordinary expression render over a scope holding nothing but
// that tree.
auto NamespaceValueOf(
    const mir::CompilationUnit& unit, mir::TypeId type, std::string_view name,
    const mir::ValueBuild& build) -> std::string {
  const ScopeView view = ScopeView::ForUnitConstant(unit, build.body);
  return NamespaceConstantOf(
      RenderTypeAsCpp(unit, type), name,
      RenderExpr(view, view.Expr(build.value)));
}

// The descriptions the unit holds, one definition each, ahead of any code that
// names one. These sit beside the declarations they serve: a class's own
// constant may name one in its initializer, and two constants of one file are
// initialized in the order the file writes them, which is a guarantee that ends
// at the file boundary.
auto RenderTypeDescriptions(const mir::CompilationUnit& unit) -> std::string {
  std::string out;
  for (const mir::TypeDescriptorId id : unit.type_descriptors.Ids()) {
    out += NamespaceValueOf(
        unit, mir::TypeDescriptorTypeOf(unit, id), CppTypeDescriptorName(id),
        unit.builds.descriptors.Get(id));
  }
  return out;
}

// The values the unit was written with, one definition per distinct value,
// after the descriptions because every one of them names the description of its
// own type. A use is the name written here, so each is built once for the whole
// artifact.
auto RenderIntegralConstants(const mir::CompilationUnit& unit) -> std::string {
  std::string out;
  for (const mir::IntegralConstantId id : unit.integral_constants.Ids()) {
    out += NamespaceValueOf(
        unit, unit.integral_constants.Get(id).type, CppIntegralConstantName(id),
        unit.builds.constants.Get(id));
  }
  return out;
}

// A unit's C++ peer is a namespace holding everything the unit declares. That
// is the unit boundary made literal: a class the unit owns is reached by the
// one name it carries, and everything the namespace itself holds is reached
// through the namespace -- the same forms whether the unit is rooted in a
// design element or is a rootless package.
//
// The signature names another unit's file only where it extends a class of it.
// Every other external name its declarations carry is reached through a
// pointer, so declaring the class without its contents is enough: what a
// referrer compiling against this unit takes on is that base's unit and nothing
// else this unit itself referenced.
auto RenderUnitFiles(const mir::CompilationUnit& unit) -> CppUnitArtifacts {
  const UnitText callables = RenderUnitCallables(unit);
  const UnitText variables = RenderUnitStaticVariables(unit);
  const UnitText classes = RenderUnitClasses(unit);

  std::string declared;
  AppendSection(declared, callables.signature);
  AppendSection(declared, RenderTypeDescriptions(unit));
  AppendSection(declared, RenderIntegralConstants(unit));
  AppendSection(declared, variables.signature);
  AppendSection(declared, classes.signature);
  declared += "\n";

  std::string signature;
  signature += "#pragma once\n";
  signature += std::format("#include \"{}\"\n", support::kRuntimePreludeHeader);
  for (const std::string& name : CollectBaseUnitNames(unit)) {
    signature += std::format("#include \"{}\"\n", UnitSignatureFileOf(name));
  }
  signature += "\n";
  AppendSection(signature, RenderExternalObjectDeclarations(unit));
  signature += NamespaceBlockOf(UnitNamespaceOf(unit.name), declared);

  std::string realized;
  AppendSection(realized, variables.code);
  AppendSection(realized, classes.code);
  AppendSection(realized, callables.code);
  AppendSection(realized, RenderForeignScopeSymbols(unit));
  realized += "\n";

  // The runtime umbrella names everything a rendered body may call into, and
  // naming it rather than the individual headers is what keeps the emit's
  // include set and the precompiled header's coverage the same set. Each
  // external unit follows, since a body reaching into one needs what that
  // unit's signature promised.
  std::string code;
  code += std::format("#include \"{}\"\n", support::kRuntimePreludeHeader);
  code += std::format("#include \"{}\"\n", UnitSignatureFileOf(unit.name));
  for (const std::string& name : CollectExternalUnitNames(unit)) {
    code += std::format("#include \"{}\"\n", UnitSignatureFileOf(name));
  }
  code += "\n";
  code += NamespaceBlockOf(UnitNamespaceOf(unit.name), realized);

  return {
      .signature =
          {.relpath = UnitSignatureFileOf(unit.name),
           .content = std::move(signature)},
      .code = {
          .relpath = UnitCodeFileOf(unit.name), .content = std::move(code)}};
}

// The program entry. A design's whole contribution to it is the class its
// `$root` is an instance of and the label that root carries, so that is all
// this writes; every invariant host-boundary concern is behind the runtime
// entry it hands off to, and a new one is added there rather than here. It
// names the design root's signature and nothing else: a symbol only foreign C
// calls is defined by the unit that declares it, and every such unit is one the
// program already links.
auto RenderHostMain(const mir::CompilationUnit& root) -> std::string {
  const mir::RootedTree* tree = mir::RootedTreeOf(root);
  if (tree == nullptr) {
    throw InternalError("backend::cpp: the design root roots no tree");
  }
  const mir::Class& root_class = root.GetClass(tree->root);

  std::string out;
  out += std::format("#include \"{}\"\n", support::kHostEntryHeader);
  out += std::format("#include \"{}\"\n", UnitSignatureFileOf(root.name));
  out += "\n";
  out += "auto main(int argc, char** argv) -> int {\n";
  out += std::format(
      "  return lyra::runtime::RunDesign<{}::{}>(argc, argv, \"{}\");\n",
      CppUnitScope(root.name), CppClassName(root_class, tree->root), root.name);
  out += "}\n";
  return out;
}

}  // namespace

auto EmitCppUnit(const mir::CompilationUnit& unit) -> CppUnitArtifacts {
  return RenderUnitFiles(unit);
}

auto EmitCppHostMain(const mir::CompilationUnit& root) -> CppArtifact {
  return {.relpath = "main.cpp", .content = RenderHostMain(root)};
}

}  // namespace lyra::backend::cpp
