#include <algorithm>
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
#include "lyra/mir/enum_table_id.hpp"
#include "lyra/mir/integral_constant_id.hpp"
#include "lyra/support/runtime_prelude.hpp"

namespace lyra::backend::cpp {

namespace {

void WriteInclude(TargetText& out, std::string_view path) {
  Write(out, "#include \"", path, "\"\n");
}

// The enumeration member tables the unit's text names, one `constexpr`
// definition per table: its members are constant data, which the compiler
// builds while the program is compiled.
void RenderEnumTables(
    const mir::CompilationUnit& unit, const UnitRenderReport& report,
    TargetText& out) {
  for (const mir::EnumTableId id : report.enum_tables_named) {
    WriteDeclaration(
        out,
        VariableDeclaration{
            .form = VariableForm::kNamespaceScopeDefinition,
            .constness = Constness::kConstexpr,
            .type = CppType(unit, unit.builtins.enumeration),
            .name = CppEnumTableName(id)},
        [&](TargetText& value) {
          Write(value, CppEnumTableLiteral(unit, id));
        });
  }
}

// The constant values the unit's text names, one `constexpr` definition per
// distinct value: each is the literal of its type, which the compiler builds
// while the program is compiled.
void RenderIntegralConstants(
    const mir::CompilationUnit& unit, const UnitRenderReport& report,
    TargetText& out) {
  for (const mir::IntegralConstantId id : report.constants_named) {
    WriteDeclaration(
        out,
        VariableDeclaration{
            .form = VariableForm::kNamespaceScopeDefinition,
            .constness = Constness::kConstexpr,
            .type = CppType(unit, unit.integral_constants.Get(id).type),
            .name = CppIntegralConstantName(id)},
        [&](TargetText& value) { Write(value, CppIntegralLiteral(unit, id)); });
  }
}

// Includes, into the file `self`, each file `text` said it needs ahead of it.
// A declaration may name what it declares, and a file does not include itself.
void WriteRequiredIncludes(
    TargetText& file, const TargetText& text, std::string_view self) {
  for (const std::string& required : text.RequiredFiles()) {
    if (required != self) {
      WriteInclude(file, required);
    }
  }
}

// The header for one thing this unit's bodies read of another: the opening
// header for its namespace, or the header defining one class. A body reaches a
// member of another unit's object without writing the class's name, so its
// text cannot ask for the class's definition; what the unit read is what says
// so. Including only those means a change elsewhere in that unit does not
// recompile this one.
auto FileConsumed(const mir::ConsumedSignature& consumed) -> std::string {
  return std::visit(
      Overloaded{
          [](const mir::ConsumedNamespace& consumed_namespace) {
            return UnitOpeningFileOf(consumed_namespace.unit_name);
          },
          [](const mir::ConsumedClass& consumed_class) {
            return ClassDefinitionFileOf(
                consumed_class.unit_name, consumed_class.class_path);
          }},
      consumed);
}

// One header defining classes other units may name, and what it defines.
struct ClassFile {
  std::string relpath;
  TargetText defined;
};

// A unit becomes one C++ namespace, named after it, written across these files:
//
//   Top.<Struct>.types.hpp  one per struct the unit declares under a name
//   Top.forward.hpp         the classes other units may name, declared
//   Top.opening.hpp         namespace functions and variables
//   Top.<Class>.hpp         one per class the source declared
//   Top.hpp                 what the unit published of its scopes: the class
//                           of its instance, and the class of each generate
//                           block declared inside the class of the scope
//                           holding it
//   Top.cpp                 every other class, and every definition
//
// A declaration has a file of its own only where a declaration of another unit
// needs it complete ahead of its own: a class another class may extend, a
// struct another holds by value. Nothing outside a unit extends the class of
// one of its scopes, so those are written together in the file that carries
// the unit's name, and no file is named after a path through the hierarchy.
//
// Every file includes what its own text asked for as it named things: the
// forward header of a class reached through a pointer, the header of a base,
// the header of a struct held by value. A class cannot be its own ancestor and
// a struct cannot hold itself, a forward header includes nothing, and a scope's
// class holds what it reaches in another unit through the class every scope
// extends, so two units can include each other's headers without a cycle.
// Anything other units never name goes into the `.cpp`, so changing it
// recompiles no other unit.
auto RenderUnitFiles(
    const mir::CompilationUnit& unit, diag::DiagnosticSink& refusals)
    -> CppUnitArtifacts {
  // Every body is written before the constants and the member tables are,
  // since the ones the code file defines are the ones those bodies turn out to
  // name.
  UnitRenderReport report{
      .refusals = &refusals, .constants_named = {}, .enum_tables_named = {}};
  UnitStructs structs = RenderUnitStructs(unit, report);
  const UnitText callables = RenderUnitCallables(unit, report);
  const UnitText variables = RenderUnitStaticVariables(unit);
  const UnitText forwards = RenderUnitForwardDeclarations(unit);
  UnitClasses classes = RenderUnitClasses(unit, report);
  const UnitClosures closures = RenderUnitClosures(unit, report);
  TargetText foreign_symbols;
  RenderForeignScopeSymbols(unit, report, foreign_symbols);
  TargetText enum_tables;
  RenderEnumTables(unit, report, enum_tables);
  const SourceName unit_namespace = UnitNamespaceOf(unit.name);

  std::vector<CppArtifact> declarations;
  for (const UnitStruct& declared : structs.declared) {
    std::string relpath =
        UnitStructFileOf(unit.name, CppStructName(unit.GetStruct(declared.id)));
    TargetText file;
    file += "#pragma once\n";
    WriteInclude(file, support::kRuntimePreludeHeader);
    WriteRequiredIncludes(file, declared.declaration, relpath);
    file += "\n";
    OpenNamespace(file, unit_namespace);
    file += declared.declaration.View();
    CloseNamespace(file, unit_namespace);
    declarations.push_back(
        {.relpath = std::move(relpath), .content = std::move(file).Take()});
  }

  TargetText forward;
  forward += "#pragma once\n\n";
  OpenNamespace(forward, unit_namespace);
  forward += forwards.signature.View();
  CloseNamespace(forward, unit_namespace);

  TargetText opened;
  AppendSection(opened, callables.signature);
  AppendSection(opened, variables.signature);
  opened += "\n";

  const std::string opening_file = UnitOpeningFileOf(unit.name);
  TargetText opening;
  opening += "#pragma once\n";
  WriteInclude(opening, support::kRuntimePreludeHeader);
  WriteRequiredIncludes(opening, opened, opening_file);
  opening += "\n";
  OpenNamespace(opening, unit_namespace);
  opening += opened.View();
  CloseNamespace(opening, unit_namespace);

  declarations.push_back(
      {.relpath = UnitForwardFileOf(unit.name),
       .content = std::move(forward).Take()});
  declarations.push_back(
      {.relpath = opening_file, .content = std::move(opening).Take()});

  // Each class other units may name is written into the file that defines it,
  // after whatever that file already holds. The unit's own header comes first
  // and is written for every unit, since it is the file the unit is known by.
  std::vector<ClassFile> class_files;
  class_files.push_back(
      ClassFile{.relpath = UnitSignatureFileOf(unit.name), .defined = {}});
  for (const PublishedClass& published : classes.published) {
    std::string relpath =
        ClassDefinitionFileOf(unit.name, *unit.GetClass(published.id).path);
    auto file = std::ranges::find(class_files, relpath, &ClassFile::relpath);
    if (file == class_files.end()) {
      file = class_files.insert(
          file, ClassFile{.relpath = std::move(relpath), .defined = {}});
    }
    AppendSection(file->defined, published.text);
  }
  for (const ClassFile& class_file : class_files) {
    TargetText file;
    file += "#pragma once\n";
    WriteInclude(file, opening_file);
    WriteRequiredIncludes(file, class_file.defined, class_file.relpath);
    file += "\n";
    OpenNamespace(file, unit_namespace);
    file += class_file.defined.View();
    CloseNamespace(file, unit_namespace);
    declarations.push_back(
        {.relpath = class_file.relpath, .content = std::move(file).Take()});
  }

  TargetText realized;
  AppendSection(realized, forwards.code);
  AppendSection(realized, enum_tables);
  {
    const TargetText::Section constants(realized);
    RenderIntegralConstants(unit, report, realized);
  }
  AppendSection(realized, structs.code);
  AppendSection(realized, classes.internal);
  AppendSection(realized, closures.declarations);
  AppendSection(realized, variables.code);
  AppendSection(realized, closures.definitions);
  AppendSection(realized, classes.definitions);
  AppendSection(realized, callables.code);
  AppendSection(realized, foreign_symbols);
  realized += "\n";

  // The whole runtime through its one umbrella header, which is also what the
  // precompiled header covers; then every header of this unit, and of every
  // other unit only the headers this unit uses, each once.
  const std::string code_file = UnitCodeFileOf(unit.name);
  std::set<std::string> included{code_file};
  TargetText code;
  WriteInclude(code, support::kRuntimePreludeHeader);
  const auto include_once = [&](const std::string& file) {
    if (included.insert(file).second) WriteInclude(code, file);
  };
  for (const ClassFile& class_file : class_files) {
    include_once(class_file.relpath);
  }
  for (const mir::ConsumedSignature& consumed : unit.consumed_signatures) {
    include_once(FileConsumed(consumed));
  }
  for (const std::string& required : realized.RequiredFiles()) {
    include_once(required);
  }
  code += "\n";
  OpenNamespace(code, unit_namespace);
  code += realized.View();
  CloseNamespace(code, unit_namespace);

  return {
      .declarations = std::move(declarations),
      .code = {.relpath = code_file, .content = std::move(code).Take()}};
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
