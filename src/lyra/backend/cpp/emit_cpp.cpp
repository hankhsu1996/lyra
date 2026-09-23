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

// One value the unit settles before the program runs, written as a constant of
// the namespace: the type it has, the name it is reached by, and the expression
// that builds it. The build is an expression tree with no statements, so what
// writes it is the ordinary expression render over a scope holding nothing but
// that tree.
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
      [&](TargetText& value) {
        RenderExpr(view, view.Expr(build.value), value);
      });
}

// The descriptions the unit holds, one definition each, ahead of any code that
// names one. These sit beside the declarations they serve: a class's own
// constant may name one in its initializer, and two constants of one file are
// initialized in the order the file writes them, which is a guarantee that ends
// at the file boundary.
void RenderTypeDescriptions(const mir::CompilationUnit& unit, TargetText& out) {
  for (const mir::TypeDescriptorId id : unit.type_descriptors.Ids()) {
    RenderNamespaceValue(
        unit, mir::TypeDescriptorTypeOf(unit, id), CppTypeDescriptorName(id),
        unit.builds.descriptors.Get(id), out);
  }
}

// The values the unit was written with, one definition per distinct value,
// after the descriptions because every one of them names the description of its
// own type. A use is the name written here, so each is built once for the whole
// artifact.
void RenderIntegralConstants(
    const mir::CompilationUnit& unit, TargetText& out) {
  for (const mir::IntegralConstantId id : unit.integral_constants.Ids()) {
    RenderNamespaceValue(
        unit, unit.integral_constants.Get(id).type, CppIntegralConstantName(id),
        unit.builds.constants.Get(id), out);
  }
}

// The file a class of this unit is written in. Every class a unit promised
// takes one of its own, so the class is the whole of what says where it is --
// and this is the one place that says it, so the file a class is written into
// and the file an include names cannot come apart.
//
// Only a class the unit promised has such a file, and only such a class is ever
// asked for: a class a referrer may name cannot rest on one it may not, because
// then what it rests on would have to be readable where the referrer compiles
// and would be promised after all.
auto FileDeclaring(const mir::CompilationUnit& unit, mir::ClassId id)
    -> std::string {
  return UnitClassFileOf(unit.name, CppClassName(unit.GetClass(id), id));
}

// The same, for a class named as something to rest on. A class of another unit
// is spelled from the pair naming it, which is what the referrer holds and what
// the declaring unit wrote -- so neither has to know anything else about the
// other's emission.
auto FileDeclaring(
    const mir::CompilationUnit& unit, const mir::ClassRef& rests_on)
    -> std::string {
  if (const auto* intra = std::get_if<mir::IntraUnitClassRef>(&rests_on)) {
    return FileDeclaring(unit, intra->class_id);
  }
  const auto& cross = std::get<mir::CrossUnitClassRef>(rests_on);
  return UnitClassFileOf(cross.unit_name, ToCppName(cross.class_name));
}

// The file one consumption reads. A unit writes its namespace in one file and
// each class it promised in one of its own, so a referrer names the part it
// read: the rest of the unit is text it never sees, and a change confined to
// that text moves nothing it compiles.
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

// A unit's C++ peer is a namespace holding everything the unit declares. That
// is the unit boundary made literal: a class the unit owns is reached by the
// one name it carries, and everything the namespace itself holds is reached
// through the namespace -- the same forms whether the unit is rooted in a
// design element or is a rootless package.
//
// The declarations carry what the unit promised and nothing else: a declaration
// the unit kept to itself is written with the code, so a change confined to one
// moves no text a referrer compiles.
//
// They are several files, which is what lets any two units reference each
// other. The opening one stands on nothing of the program and every other file
// of the unit reads it first; each class the unit promised takes one of its own
// and reads the file declaring whatever it rests on, whichever unit that
// belongs to. So the files are the size of the thing an order is over -- one
// class against the class it rests on -- and a program always has such an
// order, since a class may not rest on itself. The umbrella reads them all and
// is the name a referrer writes.
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

  // The runtime umbrella names everything a rendered body may call into, and
  // naming it rather than the individual headers is what keeps the emit's
  // include set and the precompiled header's coverage the same set. What this
  // unit read of every other follows, each named as the part it read, so a
  // change to a class this file never named moves nothing it compiles.
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

// The program entry. A design's whole contribution to it is how one makes the
// object its `$root` is and the label that root carries, so that is all this
// writes; every invariant host-boundary concern is behind the runtime entry it
// hands off to, and a new one is added there rather than here. It names the
// design root's signature and nothing else: a symbol only foreign C calls is
// defined by the unit that declares it, and every such unit is one the program
// already links.
//
// The entry it names is the one every referrer asks an object of that unit
// through, so the object with no owner above it is that same case with nothing
// above it.
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
