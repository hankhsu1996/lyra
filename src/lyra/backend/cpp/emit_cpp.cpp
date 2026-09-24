#include <string>
#include <string_view>
#include <utility>
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
#include "lyra/backend/cpp/target_text.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/integral_constant_id.hpp"
#include "lyra/mir/type_descriptor.hpp"
#include "lyra/mir/type_descriptor_id.hpp"
#include "lyra/mir/value_build.hpp"
#include "lyra/support/runtime_prelude.hpp"

namespace lyra::backend::cpp {

namespace {

void WriteInclude(TargetText& out, std::string_view path) {
  Write(out, "#include \"", path, "\"\n");
}

// A constant of the unit's namespace, `const T name = value;`. The value is a
// single expression, written by the ordinary expression render.
void RenderNamespaceValue(
    const mir::CompilationUnit& unit, mir::TypeId type, const CppName& name,
    const mir::ValueBuild& build, TargetText& out) {
  const ScopeView view = ScopeView::ForUnitConstant(unit, build.body);
  WriteDeclaration(
      out,
      DeclaredCell{
          .owner = CellOwner::kNamespace,
          .text = CellText::kDefined,
          .immutable = true,
          .type = CppType(unit, type),
          .name = name,
          .qualifier = {}},
      [&](TargetText& value) { Write(view, value, build.value); });
}

// The run-time type descriptions, defined in the code file ahead of every
// class. A class's constant may use one, and C++ initializes the constants of
// one file in the order they are written, but gives no order across files.
void RenderTypeDescriptions(const mir::CompilationUnit& unit, TargetText& out) {
  for (const mir::TypeDescriptorId id : unit.type_descriptors.Ids()) {
    RenderNamespaceValue(
        unit, mir::TypeDescriptorTypeOf(unit, id), CppTypeDescriptorName(id),
        unit.builds.descriptors.Get(id), out);
  }
}

// The constant values the unit uses, one definition per distinct value, so each
// is built once. They come after the descriptions because each uses the
// description of its own type.
void RenderIntegralConstants(
    const mir::CompilationUnit& unit, TargetText& out) {
  for (const mir::IntegralConstantId id : unit.integral_constants.Ids()) {
    RenderNamespaceValue(
        unit, unit.integral_constants.Get(id).type, CppIntegralConstantName(id),
        unit.builds.constants.Get(id), out);
  }
}

// The header of a class of this unit. The file is written and included under
// the name computed here, so the two agree. Only a class other units may name
// has one, and only such a class is asked about: its bases have to be
// nameable too, since a unit compiling against it sees them.
auto FileDeclaring(const mir::CompilationUnit& unit, mir::ClassId id)
    -> std::string {
  return UnitClassFileOf(unit.name, CppClassName(unit.GetClass(id), id));
}

// The header of a base class, which may belong to another unit; that unit's
// name and the class name are all it takes to compute.
auto FileDeclaring(
    const mir::CompilationUnit& unit, const mir::ClassRef& rests_on)
    -> std::string {
  if (const auto* intra = std::get_if<mir::IntraUnitClassRef>(&rests_on)) {
    return FileDeclaring(unit, intra->class_id);
  }
  const auto& cross = std::get<mir::CrossUnitClassRef>(rests_on);
  return UnitClassFileOf(cross.unit_name, ToCppName(cross.class_name));
}

// The header for one thing this unit uses from another: the opening header for
// its namespace, or one class's header. Including only those means a change
// elsewhere in that unit does not recompile this one.
auto FileConsumed(const mir::ConsumedSignature& consumed) -> std::string {
  return std::visit(
      Overloaded{
          [](const mir::ConsumedNamespace& consumed_namespace) {
            return UnitOpeningFileOf(consumed_namespace.unit_name);
          },
          [](const mir::ConsumedClass& consumed_class) {
            return UnitClassFileOf(
                consumed_class.unit_name, ToCppName(consumed_class.class_name));
          }},
      consumed);
}

// A unit becomes one C++ namespace, named after it, written across these files:
//
//   Top.opening.hpp   forward declarations, namespace functions and variables
//   Top.<Class>.hpp   one per class other units may name
//   Top.hpp           includes all of the above; what other units include
//   Top.cpp           every other class, and every definition
//
// The opening header includes nothing of the program, and a class header
// includes the headers of its bases, so two units can include each other's
// headers without a cycle. Anything other units never name goes into the
// `.cpp`, so changing it recompiles no other unit.
auto RenderUnitFiles(const mir::CompilationUnit& unit) -> CppUnitArtifacts {
  const UnitText callables = RenderUnitCallables(unit);
  const UnitText variables = RenderUnitStaticVariables(unit);
  const UnitText forwards = RenderUnitForwardDeclarations(unit);
  UnitClasses classes = RenderUnitClasses(unit);
  const SourceName unit_namespace = UnitNamespaceOf(unit.name);

  TargetText opened;
  AppendSection(opened, forwards.signature);
  AppendSection(opened, callables.signature);
  AppendSection(opened, variables.signature);
  opened += "\n";

  TargetText opening;
  opening += "#pragma once\n";
  WriteInclude(opening, support::kRuntimePreludeHeader);
  opening += "\n";
  {
    const TargetText::Section external(opening);
    RenderExternalObjectDeclarations(unit, opening);
  }
  OpenNamespace(opening, unit_namespace);
  opening += opened.View();
  CloseNamespace(opening, unit_namespace);

  std::vector<CppArtifact> declarations;
  declarations.push_back(
      {.relpath = UnitOpeningFileOf(unit.name),
       .content = std::move(opening).Take()});

  TargetText umbrella;
  umbrella += "#pragma once\n";
  WriteInclude(umbrella, UnitOpeningFileOf(unit.name));
  for (const PromisedClass& promised : classes.promised) {
    const mir::Class& cls = unit.GetClass(promised.id);
    TargetText file;
    file += "#pragma once\n";
    WriteInclude(file, UnitOpeningFileOf(unit.name));
    for (const mir::ClassRef& rests_on : mir::RestsOnDeclaredClasses(cls)) {
      WriteInclude(file, FileDeclaring(unit, rests_on));
    }
    file += "\n";
    OpenNamespace(file, unit_namespace);
    file += promised.text.View();
    CloseNamespace(file, unit_namespace);
    const std::string relpath = FileDeclaring(unit, promised.id);
    WriteInclude(umbrella, relpath);
    declarations.push_back(
        {.relpath = relpath, .content = std::move(file).Take()});
  }
  declarations.push_back(
      {.relpath = UnitSignatureFileOf(unit.name),
       .content = std::move(umbrella).Take()});

  TargetText realized;
  AppendSection(realized, forwards.code);
  {
    const TargetText::Section descriptions(realized);
    RenderTypeDescriptions(unit, realized);
  }
  {
    const TargetText::Section constants(realized);
    RenderIntegralConstants(unit, realized);
  }
  AppendSection(realized, classes.internal);
  AppendSection(realized, variables.code);
  AppendSection(realized, classes.definitions);
  AppendSection(realized, callables.code);
  {
    const TargetText::Section foreign(realized);
    RenderForeignScopeSymbols(unit, realized);
  }
  realized += "\n";

  // The whole runtime through its one umbrella header, which is also what the
  // precompiled header covers; then, of every other unit, only the headers
  // this unit uses.
  TargetText code;
  WriteInclude(code, support::kRuntimePreludeHeader);
  WriteInclude(code, UnitSignatureFileOf(unit.name));
  for (const mir::ConsumedSignature& consumed : unit.consumed_signatures) {
    WriteInclude(code, FileConsumed(consumed));
  }
  code += "\n";
  OpenNamespace(code, unit_namespace);
  code += realized.View();
  CloseNamespace(code, unit_namespace);

  return {
      .declarations = std::move(declarations),
      .code = {
          .relpath = UnitCodeFileOf(unit.name),
          .content = std::move(code).Take()}};
}

// `main.cpp`: hands the root unit's label and its `sv_create` entry to the
// runtime, which does everything else about running the program -- command
// line, errors, the simulation itself. `sv_create` is the entry any other unit
// would use to make an object of the root unit.
auto RenderHostMain(const mir::CompilationUnit& root) -> std::string {
  if (mir::RootedTreeOf(root) == nullptr) {
    throw InternalError("backend::cpp: the design root roots no tree");
  }

  TargetText out;
  WriteInclude(out, support::kHostEntryHeader);
  WriteInclude(out, UnitSignatureFileOf(root.name));
  out += "\n";
  out += "auto main(int argc, char** argv) -> int {\n";
  Write(
      out, "  return lyra::runtime::RunDesignRoot(argc, argv, \"", root.name,
      "\", ", CppUnitScope(root.name),
      "::", CppMintedEntryName(mir::MintedEntry::kMakeObject), ");\n");
  out += "}\n";
  return std::move(out).Take();
}

}  // namespace

auto EmitCppUnit(const mir::CompilationUnit& unit) -> CppUnitArtifacts {
  return RenderUnitFiles(unit);
}

auto EmitCppHostMain(const mir::CompilationUnit& root) -> CppArtifact {
  return {.relpath = "main.cpp", .content = RenderHostMain(root)};
}

}  // namespace lyra::backend::cpp
