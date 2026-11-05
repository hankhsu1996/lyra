#include <algorithm>
#include <map>
#include <optional>
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
#include "lyra/backend/cpp/render_constant.hpp"
#include "lyra/backend/cpp/render_decl.hpp"
#include "lyra/backend/cpp/render_type.hpp"
#include "lyra/backend/cpp/scope_view.hpp"
#include "lyra/backend/cpp/target_text.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/constant_id.hpp"
#include "lyra/mir/referenced_declarations.hpp"
#include "lyra/mir/verify.hpp"
#include "lyra/support/linkage.hpp"
#include "lyra/support/runtime_prelude.hpp"

namespace lyra::backend::cpp {

namespace {

void WriteInclude(TargetText& out, std::string_view path) {
  Write(out, "#include \"", path, "\"\n");
}

// A file of the project being laid out: its text, and each file of the project
// it includes, recorded as the include is written. The runtime's headers are
// included with `WriteInclude` and are not among them, since a compile reads
// those as one input of their own.
struct ProjectFile {
  TargetText text;
  std::vector<std::string> includes;

  void Include(std::string path) {
    WriteInclude(text, path);
    includes.push_back(std::move(path));
  }

  [[nodiscard]] auto Into(std::string relpath) && -> CppArtifact {
    return {
        .relpath = std::move(relpath),
        .content = std::move(text).Take(),
        .includes = std::move(includes)};
  }
};

// The constants the unit's code file names, each declared ahead of every text
// naming it and defined once every class is, since a constant may hold the
// address of a class's member: every constant a text of the file names, and
// every constant one of those names, each `const`, defined in the order the
// unit states them, so each is defined after what it names.
struct NamedConstants {
  TargetText declarations;
  TargetText definitions;
};

auto RenderNamedConstants(
    const mir::CompilationUnit& unit, UnitRenderReport& report)
    -> NamedConstants {
  NamedConstants text;
  std::map<mir::ConstantId, TargetText> defined;
  while (defined.size() < report.constants_named.size()) {
    for (const mir::ConstantId named : std::set(report.constants_named)) {
      if (defined.contains(named)) continue;
      const mir::ConstantDecl& constant = unit.constants.Get(named);
      TargetText value;
      WriteConstantValue(
          unit, support::Linkage::kInternal, report, constant.type,
          constant.value, value);
      defined.emplace(named, std::move(value));
    }
  }
  for (const auto& [id, value] : defined) {
    const mir::ConstantDecl& constant = unit.constants.Get(id);
    WriteDeclaration(
        text.declarations, VariableDeclaration{
                               .form = VariableForm::kExternDeclaration,
                               .constness = Constness::kConst,
                               .type = CppType(unit, constant.type),
                               .name = CppConstantName(id)});
    WriteDeclaration(
        text.definitions,
        VariableDeclaration{
            .form = VariableForm::kNamespaceScopeDefinition,
            .constness = Constness::kConst,
            .type = CppType(unit, constant.type),
            .name = CppConstantName(id)},
        [&](TargetText& out) { out += value; });
  }
  return text;
}

// Includes, into the file `self`, each file `text` said it needs ahead of it.
// A declaration may name what it declares, and a file does not include itself.
void WriteRequiredIncludes(
    ProjectFile& file, const TargetText& text, std::string_view self) {
  for (const std::string& required : text.RequiredFiles()) {
    if (required != self) {
      file.Include(required);
    }
  }
}

// Declares, into `file`, each class `text` named without needing it whole,
// with the definition its objects carry, in the namespace of the unit declaring
// it: `namespace P { class D; extern const ObjectDefinition sv_definition_D;
// }`. Each is spelled as that unit spells it, so the file reads nothing of that
// unit to name it.
void WriteForwardDeclarations(
    ProjectFile& file, const TargetText& text,
    const mir::CompilationUnit& unit) {
  const CppType definition_type(
      unit,
      mir::LibraryTypeOf(unit.types, support::LibraryDecl::kObjectDefinition));
  std::optional<std::string_view> open;
  for (const ForwardDeclaration& declared : text.DeclaredForward()) {
    const VerbatimName cpp_namespace{.text = declared.cpp_namespace};
    if (open != declared.cpp_namespace) {
      if (open.has_value()) {
        CloseNamespace(file.text, VerbatimName{.text = *open});
      }
      open = declared.cpp_namespace;
      OpenNamespace(file.text, cpp_namespace);
    }
    Write(file.text, "class ", VerbatimName{.text = declared.cpp_class}, ";\n");
    WriteDeclaration(
        file.text, VariableDeclaration{
                       .form = VariableForm::kExternDeclaration,
                       .constness = Constness::kConst,
                       .type = definition_type,
                       .name = DefinitionName{.of_class = declared.cpp_class}});
  }
  if (open.has_value()) CloseNamespace(file.text, VerbatimName{.text = *open});
}

// The header for one declaration of another unit this unit's code names: the
// opening header for its namespace, the header defining one class, or the
// header of the body an object entry makes. A body reaches a member of another
// unit's object without writing the class's name, so its text cannot ask for
// the class's definition; what the unit's MIR names is what says so. Including
// only those means a change elsewhere in that unit does not recompile this one.
auto FileReferenced(const mir::ReferencedDeclaration& referenced)
    -> std::string {
  return std::visit(
      Overloaded{
          [](const mir::ReferencedNamespace& referenced_namespace) {
            return UnitOpeningFileOf(referenced_namespace.unit_name);
          },
          [](const mir::ReferencedClass& referenced_class) {
            return ClassDefinitionFileOf(
                referenced_class.unit_name, referenced_class.class_path);
          },
          [](const mir::ReferencedObjectEntry& referenced_entry) {
            return BodyHeaderFileOf(referenced_entry.body_name);
          }},
      referenced);
}

// One header and the text it carries, gathered from whatever is written into
// it before it is laid out as a file.
struct HeaderFile {
  std::string relpath;
  TargetText text;
};

// Appends `text` to the header `relpath` among `files`, starting that header
// where none has that name yet.
void AppendToHeader(
    std::vector<HeaderFile>& files, std::string relpath,
    const TargetText& text) {
  auto file = std::ranges::find(files, relpath, &HeaderFile::relpath);
  if (file == files.end()) {
    file = files.insert(
        file, HeaderFile{.relpath = std::move(relpath), .text = {}});
  }
  AppendSection(file->text, text);
}

// Lays out `text` as the header `relpath`, in the unit's namespace: the
// runtime, then what the text said it needs included and declared ahead of it,
// then the text.
auto HeaderOf(
    const mir::CompilationUnit& unit, std::string relpath,
    SourceName unit_namespace, const TargetText& text) -> CppArtifact {
  ProjectFile file;
  file.text += "#pragma once\n";
  WriteInclude(file.text, support::kRuntimePreludeHeader);
  WriteRequiredIncludes(file, text, relpath);
  file.text += "\n";
  WriteForwardDeclarations(file, text, unit);
  OpenNamespace(file.text, unit_namespace);
  file.text += text.View();
  CloseNamespace(file.text, unit_namespace);
  return std::move(file).Into(std::move(relpath));
}

// A unit becomes one C++ namespace, named after it, and each body of it is
// written across these files, `Body` being the body's name, which is the
// unit's for the unit's first body:
//
//   Top.<Record>.types.hpp  one per struct or union the unit declares
//   Top.opening.hpp         namespace functions and variables
//   Body.body.hpp           the entry that makes an object of this body
//   Top.<Class>.class.hpp   one per class the source declared
//   Top.hpp                 what the unit published of its scopes: the class
//                           of its instance, and the class of each generate
//                           block declared inside the class of the scope
//                           holding it, each defined there
//   Body.cpp                every other class, and every other definition
//
// Every body states the unit's declarations alike, so every file but the two
// named after the body is the same text from each.
//
// A declaration has a file of its own only where a declaration of another unit
// needs it complete ahead of its own: a class another class may extend, a
// struct or union another holds by value. Nothing outside a unit extends the
// class of one of its scopes, so those are written together in the file that
// carries the unit's name, and no file is named after a path through the
// hierarchy.
//
// Every file includes what its own text asked for as it named things: the
// header of a base, the header of a record held by value, the header of a
// namespace whose function or variable it names. A class it only points at it
// declares itself, so naming one reads nothing of the unit declaring it. A
// class cannot be its own ancestor and a record cannot hold itself, and a
// scope's class holds what it reaches in another unit through the class every
// scope extends, so two units can include each other's headers without a
// cycle. Anything other units never read goes into the `.cpp` -- the value of
// the definition a class's objects carry, and, for a class one code file
// defines, what the class keeps for its own bodies -- so changing it
// recompiles no other unit.
auto RenderUnitFiles(
    const mir::CompilationUnit& unit, diag::DiagnosticSink& refusals)
    -> CppUnitArtifacts {
  // Every body is written before the constants are, since the ones the code
  // file defines are the ones those bodies turn out to name.
  UnitRenderReport report{.refusals = &refusals, .constants_named = {}};
  UnitRecords records = RenderUnitRecords(unit, report);
  const UnitCallables callables = RenderUnitCallables(unit, report);
  const UnitText variables = RenderUnitStaticVariables(unit);
  const TargetText internal_forwards =
      RenderUnitInternalForwardDeclarations(unit);
  UnitClasses classes = RenderUnitClasses(unit, report);
  const UnitClosures closures = RenderUnitClosures(unit, report);
  TargetText foreign_symbols;
  RenderForeignScopeSymbols(unit, report, foreign_symbols);
  const NamedConstants constants = RenderNamedConstants(unit, report);
  const SourceName unit_namespace = UnitNamespaceOf(unit.name);
  // `text` placed where what only this body states is: in the unit's
  // namespace, inside the body's own where it has one.
  const auto in_body = [&](const TargetText& text) {
    const std::optional<SourceName> body =
        BodyNamespaceOf(unit.name, unit.body_name);
    if (!body.has_value()) return text;
    TargetText placed;
    AppendSectionInNamespace(placed, *body, text);
    return placed;
  };

  std::vector<CppArtifact> declarations;
  for (const UnitRecord& declared : records.declared) {
    declarations.push_back(HeaderOf(
        unit,
        UnitRecordFileOf(unit.name, CppRecordName(unit.GetRecord(declared.id))),
        unit_namespace, declared.declaration));
  }

  // What the unit's namespace offers another unit opens its own header, which
  // every body writes alike; the entries that make an object of this body are
  // in the body's own, in the body's namespace, after what the unit's header
  // declares.
  const std::string opening_file = UnitOpeningFileOf(unit.name);
  const std::string body_file = BodyHeaderFileOf(unit.body_name);
  TargetText opened;
  AppendSection(opened, callables.of_unit.signature);
  AppendSection(opened, variables.signature);
  declarations.push_back(HeaderOf(unit, opening_file, unit_namespace, opened));
  TargetText opened_by_body = in_body(callables.of_body.signature);
  opened_by_body.Require(opening_file);
  declarations.push_back(
      HeaderOf(unit, body_file, unit_namespace, opened_by_body));

  // Each class other units may name is written into the file that defines it,
  // after whatever that file already holds. The unit's own header comes first
  // and is written for every unit, since it is the file the unit is known by.
  std::vector<HeaderFile> class_files{
      HeaderFile{.relpath = UnitSignatureFileOf(unit.name), .text = {}}};
  for (const PublishedClass& published : classes.published) {
    AppendToHeader(
        class_files,
        ClassDefinitionFileOf(
            unit.name, *support::PathOf(unit.GetClass(published.id).identity)),
        published.text);
  }
  for (const HeaderFile& class_file : class_files) {
    declarations.push_back(
        HeaderOf(unit, class_file.relpath, unit_namespace, class_file.text));
  }

  // What nothing outside this file names -- the classes realizing the unit's
  // scopes, its closures, the functions only its own bodies call, the
  // constants its texts name -- is in the namespace with no name, so another
  // body of the unit stating its own under the same names is no conflict.
  // Their members are still defined in the unit's namespace, which encloses
  // it.
  TargetText own_declarations;
  AppendSection(own_declarations, internal_forwards);
  AppendSection(own_declarations, callables.of_artifact.signature);
  AppendSection(own_declarations, constants.declarations);
  TargetText own_classes;
  AppendSection(own_classes, classes.internal);
  AppendSection(own_classes, closures.declarations);

  TargetText realized;
  AppendSectionInUnnamedNamespace(realized, own_declarations);
  AppendSection(realized, records.code);
  AppendSectionInUnnamedNamespace(realized, own_classes);
  AppendSection(realized, classes.details);
  AppendSection(realized, variables.code);
  AppendSection(realized, closures.definitions);
  AppendSection(realized, classes.definitions);
  AppendSection(realized, classes.object_definitions);
  AppendSectionInUnnamedNamespace(
      realized, classes.internal_object_definitions);
  AppendSection(realized, callables.of_unit.code);
  AppendSectionInUnnamedNamespace(realized, callables.of_artifact.code);
  AppendSection(realized, foreign_symbols);
  AppendSectionInUnnamedNamespace(realized, constants.definitions);
  realized += "\n";

  // The whole runtime through its one umbrella header, which is also what the
  // precompiled header covers; then every header of this unit, and of every
  // other unit only the headers this unit uses, each once.
  std::string code_file = BodyCodeFileOf(unit.body_name);
  std::set<std::string> included{code_file};
  ProjectFile code;
  WriteInclude(code.text, support::kRuntimePreludeHeader);
  const auto include_once = [&](const std::string& file) {
    if (included.insert(file).second) code.Include(file);
  };
  include_once(body_file);
  for (const HeaderFile& class_file : class_files) {
    include_once(class_file.relpath);
  }
  for (const mir::ReferencedDeclaration& referenced :
       mir::ReferencedDeclarationsOf(unit)) {
    include_once(FileReferenced(referenced));
  }
  for (const std::string& required : realized.RequiredFiles()) {
    include_once(required);
  }
  for (const std::string& required : callables.of_body.code.RequiredFiles()) {
    include_once(required);
  }
  code.text += "\n";
  TargetText named;
  named += realized;
  named += callables.of_body.code;
  WriteForwardDeclarations(code, named, unit);
  OpenNamespace(code.text, unit_namespace);
  code.text += realized.View();
  CloseNamespace(code.text, unit_namespace);
  AppendSectionInNamespace(
      code.text, unit_namespace, in_body(callables.of_body.code));

  return {
      .declarations = std::move(declarations),
      .code = std::move(code).Into(std::move(code_file))};
}

// `main.cpp`: hands the root unit's label and its `sv_create` entry to the
// runtime, which does everything else about running the program -- command
// line, errors, the simulation itself. `sv_create` is the entry any other unit
// would use to make an object of the root's body, and the runtime holds what
// it makes as a scope, which needs the root's class complete.
auto RenderHostMain(const mir::CompilationUnit& root) -> CppArtifact {
  if (mir::RootedTreeOf(root) == nullptr) {
    throw InternalError("backend::cpp: the design root roots no tree");
  }

  ProjectFile out;
  WriteInclude(out.text, support::kHostEntryHeader);
  out.Include(UnitSignatureFileOf(root.name));
  out.Include(BodyHeaderFileOf(root.body_name));
  out.text += "\n";
  out.text += "auto main(int argc, char** argv) -> int {\n";
  Write(
      out.text, "  return lyra::runtime::RunDesignRoot(argc, argv, ",
      CppNameLiteral(root.name), ", ", CppBodyScope(root.name, root.body_name),
      "::", CppObjectEntryName(std::nullopt), ");\n");
  out.text += "}\n";
  return std::move(out).Into("main.cpp");
}

}  // namespace

auto EmitCppUnit(
    const mir::CompilationUnit& unit, diag::DiagnosticSink& refusals)
    -> CppUnitArtifacts {
  mir::RequireLegalized(unit, "backend::cpp");
  return RenderUnitFiles(unit, refusals);
}

auto EmitCppHostMain(const mir::CompilationUnit& root) -> CppArtifact {
  mir::RequireLegalized(root, "backend::cpp");
  return RenderHostMain(root);
}

}  // namespace lyra::backend::cpp
