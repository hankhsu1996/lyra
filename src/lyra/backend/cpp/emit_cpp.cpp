#include <set>
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
    const mir::CompilationUnit& unit, diag::DiagnosticSink& refusals,
    mir::TypeId type, const CppName& name, const mir::ValueBuild& build,
    TargetText& out) {
  const ScopeView view = ScopeView::ForConstant(unit, build.body, refusals);
  WriteDeclaration(
      out,
      VariableDeclaration{
          .form = VariableForm::kNamespaceScopeDefinition,
          .is_const = true,
          .type = CppType(unit, type),
          .name = name},
      [&](TargetText& value) { Write(view, value, build.value); });
}

// The run-time type descriptions, defined in the code file. C++ initializes the
// variables of one file in the order they are written, but gives no order
// across files, so whatever reads one is written after it in the same file.
void RenderTypeDescriptions(
    const mir::CompilationUnit& unit, diag::DiagnosticSink& refusals,
    TargetText& out) {
  for (const mir::TypeDescriptorId id : unit.type_descriptors.Ids()) {
    RenderNamespaceValue(
        unit, refusals, mir::TypeDescriptorTypeOf(unit, id),
        CppTypeDescriptorName(id), unit.builds.descriptors.Get(id), out);
  }
}

// The constant values the unit uses, one definition per distinct value, so each
// is built once. They come after the descriptions because each uses the
// description of its own type.
void RenderIntegralConstants(
    const mir::CompilationUnit& unit, diag::DiagnosticSink& refusals,
    TargetText& out) {
  for (const mir::IntegralConstantId id : unit.integral_constants.Ids()) {
    RenderNamespaceValue(
        unit, refusals, unit.integral_constants.Get(id).type,
        CppIntegralConstantName(id), unit.builds.constants.Get(id), out);
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
    const mir::CompilationUnit& unit, const mir::DeclaredClassRef& rests_on)
    -> std::string {
  return std::visit(
      Overloaded{
          [&](const mir::IntraUnitClassRef& intra) {
            return FileDeclaring(unit, intra.class_id);
          },
          [](const mir::CrossUnitClassRef& cross) {
            return UnitClassFileOf(
                cross.unit_name, ToCppName(cross.class_name));
          }},
      rests_on);
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
//   Top.types.hpp     the structs the unit declares under a name
//   Top.forward.hpp   the classes other units may name, declared
//   Top.opening.hpp   namespace functions and variables
//   Top.<Class>.hpp   one per class other units may name, and per further name
//                     one goes by
//   Top.hpp           includes all of the above; what other units include
//   Top.cpp           every other class, and every definition
//
// The types header includes only other units' types headers, the forward
// header nothing, the opening header only this unit's types header and forward
// headers, and a class header the headers of its bases, so two units can
// include each other's headers without a cycle.
// Anything other units never name goes into the `.cpp`, so changing it
// recompiles no other unit.
auto RenderUnitFiles(
    const mir::CompilationUnit& unit, diag::DiagnosticSink& refusals)
    -> CppUnitArtifacts {
  const UnitText structs = RenderUnitStructs(unit, refusals);
  const UnitText callables = RenderUnitCallables(unit, refusals);
  const UnitText variables = RenderUnitStaticVariables(unit);
  const UnitText forwards = RenderUnitForwardDeclarations(unit);
  UnitClasses classes = RenderUnitClasses(unit, refusals);
  const UnitClosures closures = RenderUnitClosures(unit, refusals);
  const SourceName unit_namespace = UnitNamespaceOf(unit.name);

  // A struct is built from structs of the units whose namespaces this one
  // consumes, so their types headers come first.
  TargetText types;
  types += "#pragma once\n";
  WriteInclude(types, support::kRuntimePreludeHeader);
  for (const mir::ConsumedSignature& consumed : unit.consumed_signatures) {
    if (const auto* consumed_namespace =
            std::get_if<mir::ConsumedNamespace>(&consumed)) {
      WriteInclude(types, UnitTypesFileOf(consumed_namespace->unit_name));
    }
  }
  types += "\n";
  OpenNamespace(types, unit_namespace);
  types += structs.signature.View();
  CloseNamespace(types, unit_namespace);

  TargetText forward;
  forward += "#pragma once\n\n";
  OpenNamespace(forward, unit_namespace);
  forward += forwards.signature.View();
  CloseNamespace(forward, unit_namespace);

  TargetText opened;
  AppendSection(opened, callables.signature);
  AppendSection(opened, variables.signature);
  opened += "\n";

  // A pointer to an object of another unit's class needs only its declaration,
  // so the unit's declarations can point at objects of a unit whose own
  // declarations point back. Every class this unit names is declared, whether
  // the unit reads what that class published or only holds a pointer to one.
  TargetText opening;
  opening += "#pragma once\n";
  WriteInclude(opening, support::kRuntimePreludeHeader);
  WriteInclude(opening, UnitTypesFileOf(unit.name));
  std::set<std::string> forward_files{UnitForwardFileOf(unit.name)};
  for (const mir::ExternalClass& object : unit.external_classes) {
    forward_files.insert(UnitForwardFileOf(object.unit_name));
  }
  for (const mir::Type& type : unit.types) {
    const auto* object = type.As<mir::ObjectType>();
    if (object == nullptr) continue;
    if (const auto* named = std::get_if<mir::CrossUnitClassRef>(&object->of)) {
      forward_files.insert(UnitForwardFileOf(named->unit_name));
    }
  }
  for (const std::string& forward_file : forward_files) {
    WriteInclude(opening, forward_file);
  }
  opening += "\n";
  OpenNamespace(opening, unit_namespace);
  opening += opened.View();
  CloseNamespace(opening, unit_namespace);

  std::vector<CppArtifact> declarations;
  declarations.push_back(
      {.relpath = UnitTypesFileOf(unit.name),
       .content = std::move(types).Take()});
  declarations.push_back(
      {.relpath = UnitForwardFileOf(unit.name),
       .content = std::move(forward).Take()});
  declarations.push_back(
      {.relpath = UnitOpeningFileOf(unit.name),
       .content = std::move(opening).Take()});

  TargetText umbrella;
  umbrella += "#pragma once\n";
  WriteInclude(umbrella, UnitOpeningFileOf(unit.name));
  for (const PublishedClass& published : classes.published) {
    const mir::Class& cls = unit.GetClass(published.id);
    TargetText file;
    file += "#pragma once\n";
    WriteInclude(file, UnitOpeningFileOf(unit.name));
    for (const mir::DeclaredClassRef& rests_on :
         mir::RestsOnDeclaredClasses(cls)) {
      WriteInclude(file, FileDeclaring(unit, rests_on));
    }
    file += "\n";
    OpenNamespace(file, unit_namespace);
    file += published.text.View();
    CloseNamespace(file, unit_namespace);
    const std::string relpath = FileDeclaring(unit, published.id);
    WriteInclude(umbrella, relpath);
    declarations.push_back(
        {.relpath = relpath, .content = std::move(file).Take()});
    // A unit naming the class by a further name includes the file that name
    // computes, which defines it by including the class's own.
    for (const std::string& alias : cls.aliases) {
      TargetText aliased;
      aliased += "#pragma once\n";
      WriteInclude(aliased, relpath);
      declarations.push_back(
          {.relpath = UnitClassFileOf(unit.name, ToCppName(alias)),
           .content = std::move(aliased).Take()});
    }
  }
  declarations.push_back(
      {.relpath = UnitSignatureFileOf(unit.name),
       .content = std::move(umbrella).Take()});

  TargetText realized;
  AppendSection(realized, forwards.code);
  {
    const TargetText::Section descriptions(realized);
    RenderTypeDescriptions(unit, refusals, realized);
  }
  {
    const TargetText::Section constants(realized);
    RenderIntegralConstants(unit, refusals, realized);
  }
  AppendSection(realized, structs.code);
  AppendSection(realized, classes.internal);
  AppendSection(realized, closures.declarations);
  AppendSection(realized, variables.code);
  AppendSection(realized, closures.definitions);
  AppendSection(realized, classes.definitions);
  AppendSection(realized, callables.code);
  {
    const TargetText::Section foreign(realized);
    RenderForeignScopeSymbols(unit, refusals, realized);
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
      out, "  return lyra::runtime::RunDesignRoot(argc, argv, ",
      CppNameLiteral(root.name), ", ", CppUnitScope(root.name),
      "::", CppMintedEntryName(mir::MintedEntry::kMakeObject), ");\n");
  out += "}\n";
  return std::move(out).Take();
}

}  // namespace

auto EmitCppUnit(
    const mir::CompilationUnit& unit, diag::DiagnosticSink& refusals)
    -> CppUnitArtifacts {
  return RenderUnitFiles(unit, refusals);
}

auto EmitCppHostMain(const mir::CompilationUnit& root) -> CppArtifact {
  return {.relpath = "main.cpp", .content = RenderHostMain(root)};
}

}  // namespace lyra::backend::cpp
