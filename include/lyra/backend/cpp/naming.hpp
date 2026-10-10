#pragma once

#include <array>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <variant>

#include "lyra/backend/cpp/target_text.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_constant_id.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/closure_id.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/enum_table_id.hpp"
#include "lyra/mir/external_class.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/integral_constant_id.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/minted_entry.hpp"
#include "lyra/mir/struct_decl.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/def_path.hpp"
#include "lyra/support/runtime_class.hpp"
#include "lyra/support/value_operation.hpp"

namespace lyra::backend::cpp {

// Every name the compiler makes up starts with `sv_`, and a source name that
// starts with it is escaped rather than written as is. SystemVerilog reserves
// no identifier for the compiler, so without the prefix a made-up name could
// collide with one the design declares.
inline constexpr std::string_view kMintedPrefix = "sv_";

// Words C++ does not accept as a name: keywords, the alternative operator
// spellings, and identifiers with a special meaning ([lex.key], [lex.name]
// table 4). SystemVerilog reserves none of them, so a design may declare any.
// The last group would compile as a name and is listed anyway: escaping one
// only makes it read worse, while missing one breaks the build.
inline constexpr auto kCppReservedWords = std::to_array<std::string_view>(
    {"alignas",
     "alignof",
     "and",
     "and_eq",
     "asm",
     "auto",
     "bitand",
     "bitor",
     "bool",
     "break",
     "case",
     "catch",
     "char",
     "char16_t",
     "char32_t",
     "char8_t",
     "class",
     "co_await",
     "co_return",
     "co_yield",
     "compl",
     "concept",
     "const",
     "const_cast",
     "consteval",
     "constexpr",
     "constinit",
     "continue",
     "contract_assert",
     "decltype",
     "default",
     "delete",
     "do",
     "double",
     "dynamic_cast",
     "else",
     "enum",
     "explicit",
     "export",
     "extern",
     "false",
     "final",
     "float",
     "for",
     "friend",
     "goto",
     "if",
     "import",
     "inline",
     "int",
     "long",
     "module",
     "mutable",
     "namespace",
     "new",
     "noexcept",
     "not",
     "not_eq",
     "nullptr",
     "operator",
     "or",
     "or_eq",
     "override",
     "post",
     "pre",
     "private",
     "protected",
     "public",
     "register",
     "reinterpret_cast",
     "requires",
     "return",
     "short",
     "signed",
     "sizeof",
     "static",
     "static_assert",
     "static_cast",
     "struct",
     "switch",
     "template",
     "this",
     "thread_local",
     "throw",
     "true",
     "try",
     "typedef",
     "typeid",
     "typename",
     "union",
     "unsigned",
     "using",
     "virtual",
     "void",
     "volatile",
     "wchar_t",
     "while",
     "xor",
     "xor_eq"});

// How each kind of name is spelled in the emitted C++. Every site that writes a
// name takes it from here, so a declaration and its uses cannot spell it
// differently. These types are written straight into the output; a site that
// needs the characters as a string, such as a file path, converts them first.
// Each holds views into the MIR or into constant words, which outlive the
// writing.

// A name from the source, as a C++ identifier. SystemVerilog allows any
// printable non-space character in an escaped identifier (LRM 5.6.1), while C++
// allows letters, digits and underscores and reserves some words. A name C++
// accepts, and that does not start with `sv_`, is written as is: `count` stays
// `count`. Anything else becomes `sv_esc_` and its bytes in hex, so `\a.b `
// becomes `sv_esc_612e62`. Replacing the bad characters instead (`a.b` ->
// `a_b`) could turn two different source names into one C++ name, and the
// escaped form can collide with neither a plain name nor a made-up one.
//
// Names C++ reserves to the implementation but still compiles -- a double
// underscore, an underscore before a capital -- are left plain.
struct SourceName {
  std::string_view name;
};

// A name made from a kind and a position: `sv_local_3`. Where the source gave
// the declaration an identifier it is appended, `sv_local_3_count`, so the
// output reads like the design while the position alone keeps two
// declarations apart.
struct MintedName {
  std::string_view what;
  std::uint32_t ordinal;
  std::optional<std::string_view> source;
};

// A made-up name with no position, `sv_create`, for a declaration there is only
// ever one of where it stands.
struct MintedWord {
  std::string_view word;
};

// A name written exactly as given, because something outside this compiler
// already spells it that way.
struct VerbatimName {
  std::string_view text;
};

// A name made from the scopes leading to a declaration, for one whose own name
// does not tell it from every other name of the namespace it is declared in:
// `sv_class_b1_g_c1_C` for class `C` declared in block `g`, and
// `sv_struct_b1_g_t7_entry_t` for a structure `entry_t` declared there. `what`
// says which kind of declaration it is. Each step is the letter of its kind
// and then the length of its name as a C++ identifier and that identifier, or
// for a step with no name the position it stands at, and after a name whatever
// else tells the step from its siblings, each behind a letter of its own. So
// where one step ends is stated rather than read off a separator and two paths
// never spell alike.
struct MintedPathName {
  std::string_view what;
  std::span<const support::DefPathData> steps;
};

// The name of a declaration whose source name alone does not tell it from the
// others of its scope: a generate block that is not the first of its scope
// under its label (LRM 27.5), `sv_gen_1_g`; an application of one the design
// fixed something for (LRM 27.3), `sv_gen_<digest>_g`; a specialization of a
// generic class (LRM 8.25), `sv_spec_<digest>_Box`. What tells it apart comes
// first, and the source name follows so the output reads like the design.
struct MintedAppliedName {
  std::string_view what;
  std::uint32_t disambiguator;
  std::optional<std::uint64_t> arguments;
  std::string_view source;
};

// Any of the above, for a declaration whose kind decides which.
using CppName = std::variant<
    SourceName, MintedName, MintedWord, VerbatimName, MintedPathName,
    MintedAppliedName>;

void WriteOne(TargetText& out, SourceName name);
void WriteOne(TargetText& out, const MintedName& name);
void WriteOne(TargetText& out, MintedWord name);
void WriteOne(TargetText& out, VerbatimName name);
void WriteOne(TargetText& out, MintedPathName name);
void WriteOne(TargetText& out, const MintedAppliedName& name);
void WriteOne(TargetText& out, const CppName& name);

[[nodiscard]] inline auto ToCppName(std::string_view name) -> SourceName {
  return SourceName{.name = name};
}

// A source name as a string the library reads, `"a.b"`: the name a design
// reports itself by. It keeps the source's own spelling, as a C string literal,
// rather than becoming an identifier.
struct NameLiteral {
  std::string_view name;
};

void WriteOne(TargetText& out, NameLiteral literal);

[[nodiscard]] inline auto CppNameLiteral(std::string_view name) -> NameLiteral {
  return NameLiteral{.name = name};
}

// The namespace a unit's declarations live in, named after the unit: unit `Top`
// is `namespace Top`. Sites call this rather than spelling the unit name, so
// the reader can tell a namespace from a class.
[[nodiscard]] inline auto UnitNamespaceOf(std::string_view unit_name)
    -> SourceName {
  return ToCppName(unit_name);
}

// The qualifier for a reference into a unit's namespace, `::Top`, used even
// from inside the unit itself. It starts at the global scope because the class
// at the root of the unit's hierarchy is also called `Top`, and inside that
// class a plain `Top` names the class, not the namespace.
struct UnitScope {
  std::string_view unit_name;
};

void WriteOne(TargetText& out, UnitScope scope);

[[nodiscard]] inline auto CppUnitScope(std::string_view unit_name)
    -> UnitScope {
  return UnitScope{.unit_name = unit_name};
}

// The files a unit is emitted as. Both the unit writing a file and every unit
// including it compute the name from here, so a unit can include a file of
// another unit without seeing how that unit was emitted. A file name is a
// string rather than output text, because what uses it first is the code that
// writes the file to disk.

// `Top.hpp`: the classes of the unit's scopes, the class of a generate block
// declared inside the class of the scope holding it. It carries the unit's
// name alone, since it is the unit as the source declared it.
[[nodiscard]] inline auto UnitSignatureFileOf(std::string_view unit_name)
    -> std::string {
  return TextOf(ToCppName(unit_name), ".hpp");
}

// `Top.opening.hpp`: the cells and functions the unit's namespace declares. Of
// the program it includes only forward files and the files of the structs its
// declarations name, so any class file, of this unit or another, can include
// it first.
[[nodiscard]] inline auto UnitOpeningFileOf(std::string_view unit_name)
    -> std::string {
  return TextOf(ToCppName(unit_name), ".opening.hpp");
}

// `Top.forward.hpp`: every class another unit may name that is declared in no
// other class, declared and not defined. It includes nothing, so any file can
// include it first, and a unit holding another's class by pointer takes its
// name from here rather than declaring it again.
[[nodiscard]] inline auto UnitForwardFileOf(std::string_view unit_name)
    -> std::string {
  return TextOf(ToCppName(unit_name), ".forward.hpp");
}

// `Top.Pair.types.hpp`: one struct a unit declares, alone in its file, which a
// value of it needs complete wherever it is held. It includes the file of each
// struct it holds by value, so whichever file is included first, every struct
// is defined after what it is built from. A struct cannot hold itself by value,
// so those files form no cycle -- while two units' structs may each hold the
// other's, so a file holding all of one unit's would.
[[nodiscard]] inline auto UnitStructFileOf(
    std::string_view unit_name, const CppName& struct_name) -> std::string {
  return TextOf(ToCppName(unit_name), ".", struct_name, ".types.hpp");
}

// `Top.Base.hpp`: one class the source declared, alone in its file, which a
// class of another unit extending it needs complete. It includes the file of
// each class it derives from, so whichever file is included first, every class
// is defined after its bases and only once.
[[nodiscard]] inline auto UnitClassFileOf(
    std::string_view unit_name, const CppName& declared_as) -> std::string {
  return TextOf(ToCppName(unit_name), ".", declared_as, ".hpp");
}

// `Top.cpp`: the translation unit defining everything the headers declare.
[[nodiscard]] inline auto UnitCodeFileOf(std::string_view unit_name)
    -> std::string {
  return TextOf(ToCppName(unit_name), ".cpp");
}

[[nodiscard]] inline auto MintedCppName(
    std::string_view what, std::uint32_t ordinal) -> MintedName {
  return MintedName{.what = what, .ordinal = ordinal, .source = std::nullopt};
}

[[nodiscard]] inline auto MintedCppNameWith(
    std::string_view what, std::uint32_t ordinal,
    std::optional<std::string_view> name) -> MintedName {
  return MintedName{.what = what, .ordinal = ordinal, .source = name};
}

// The name of a virtual method another unit's class declared, read from what
// this unit was told about that class. A virtual call to it and an override of
// it must spell it the same way, so both take it from here.
[[nodiscard]] inline auto CppExternalBehaviorName(
    const mir::CompilationUnit& unit, std::string_view unit_name,
    const support::DefPath& class_path, mir::BehaviorOrdinal ordinal)
    -> SourceName {
  const mir::ExternalClass* introducer =
      mir::FindExternalClass(unit.external_classes, unit_name, class_path);
  if (introducer == nullptr || ordinal.value >= introducer->behaviors.size()) {
    throw InternalError(
        "backend::cpp: a behavior is named that no consumed signature "
        "describes");
  }
  return ToCppName(introducer->behaviors[ordinal.value].name);
}

// The name of a function a class owns -- a method, a process, or a lifecycle
// body. A named one keeps its name; an unnamed one is `sv_body_<n>`. The
// declaration, the definition and every call take it from here.
//
// An override takes the name of the method it overrides, from the class that
// declared it: C++ matches an override to its virtual by name, so under any
// other name it would declare a second virtual instead.
//
// It takes a class rather than a unit's namespace, because a function of a
// namespace can also be reached by a foreign linkage name or as a fixed entry,
// which this does not spell.
[[nodiscard]] inline auto CppClassCallableName(
    const mir::CompilationUnit& unit, const mir::Class& cls,
    mir::CallableId body) -> CppName {
  const auto declared_here = [&]() -> CppName {
    const std::optional<std::string_view> name =
        mir::NameOf(cls.named_callables, body);
    if (name.has_value()) {
      return ToCppName(*name);
    }
    return MintedCppName("body", body.value);
  };
  const std::optional<mir::VirtualDispatchRole>& role =
      cls.callables.Get(body).virtual_dispatch;
  if (!role.has_value()) {
    return declared_here();
  }
  return std::visit(
      Overloaded{
          [&](const mir::IntroducesVirtualSlot&) -> CppName {
            return declared_here();
          },
          [&](const mir::OverridesIntraUnitSlot& taken) -> CppName {
            return CppClassCallableName(
                unit, unit.GetClass(taken.slot_owner), taken.slot_id);
          },
          [&](const mir::OverridesExternalSlot& taken) -> CppName {
            return CppExternalBehaviorName(
                unit, taken.unit_name, taken.class_path, taken.ordinal);
          },
          [](const mir::OverridesLibraryVirtual& taken) -> CppName {
            return VerbatimName{
                .text = support::LibraryVirtualName(taken.function)};
          }},
      *role);
}

// The name of a class field: `sv_field_<slot>`, plus the source name where
// there is one, `sv_field_1_v`. The source name alone would not do: a C++ class
// has one name space for fields and methods, and a scope's class holds the
// cells of every block nested in it beside the subroutines it publishes, so a
// cell `x` of a named block can stand beside a method `x`.
//
// The class declaring the field and another unit reading it both call this,
// each with the slot and the names the class answers, so the two agree.
[[nodiscard]] inline auto CppFieldName(
    std::span<const mir::NamedField> named, mir::FieldId slot) -> MintedName {
  return MintedCppNameWith("field", slot.value, mir::NameOf(named, slot));
}

// The name of a local: `sv_local_<slot>`, plus the source name where there is
// one. The source name alone would not do: two sibling scopes may each declare
// an `n` -- two `matches` patterns in one block each bind their own (LRM
// 12.6.1) -- and a function's locals are all declared in one C++ scope.
[[nodiscard]] inline auto CppLocalName(
    std::span<const mir::NamedLocal> named, mir::LocalId local) -> MintedName {
  return MintedCppNameWith("local", local.value, mir::NameOf(named, local));
}

// The names of a class's static properties and a unit's static variables. The
// ones the source declared keep their names; the rest were added by lowering,
// for state that outlives one call, and are `sv_cell_<n>` or
// `sv_variable_<n>`.
[[nodiscard]] inline auto CppStaticPropertyName(
    std::span<const mir::NamedStaticProperty> named, mir::StaticPropertyId slot)
    -> CppName {
  const std::optional<std::string_view> name = mir::NameOf(named, slot);
  if (name.has_value()) {
    return ToCppName(*name);
  }
  return MintedCppName("cell", slot.value);
}

[[nodiscard]] inline auto CppStaticVariableName(
    std::span<const mir::NamedStaticVariable> named,
    mir::StaticVariableId variable) -> CppName {
  const std::optional<std::string_view> name = mir::NameOf(named, variable);
  if (name.has_value()) {
    return ToCppName(*name);
  }
  return MintedCppName("variable", variable.value);
}

// The name the class of the generate block `block` has inside the class of the
// scope holding it, which `enclosing` names, as the step `depth` steps down
// its path. A block its label alone does not tell from the others of that
// scope -- one that is not the first under the label, or an application the
// design fixed something for -- takes a name carrying what does. C++ gives a
// class's own name to the class inside itself, so a nested class cannot take
// it, while a generate block may carry the label of the scope holding it; such
// a one is `sv_block_<depth>_<name>`. The depth is what keeps a run of blocks
// under one label from each taking the name of the one before it.
[[nodiscard]] inline auto CppNestedClassName(
    std::string_view enclosing, const support::GenerateBlockStep& block,
    std::size_t depth) -> CppName {
  if (block.disambiguator != 0 || block.arguments.has_value()) {
    return MintedAppliedName{
        .what = "gen",
        .disambiguator = block.disambiguator,
        .arguments = block.arguments,
        .source = block.label};
  }
  if (block.label == enclosing) {
    return MintedCppNameWith(
        "block", static_cast<std::uint32_t>(depth), block.label);
  }
  return ToCppName(block.label);
}

// A path naming a class ends in a generate block, whose class realizes a scope
// of the design hierarchy (LRM 23.6), or in a class the source declared (LRM
// 8.3). One ending in anything else reached a class's name by a defect.
[[noreturn]] inline void ThrowNamesNoClass() {
  throw InternalError(
      "backend::cpp: a class is named by a path ending in neither a generate "
      "block nor a class -- please report this as a bug");
}

// The generate block `step` is, for a step above the class of a scope of the
// design hierarchy: a generate block stands in an instance's body or in
// another generate block (LRM 27.3), so every step down to one is a block.
[[nodiscard]] inline auto BlockStepAt(const support::DefPathData& step)
    -> const support::GenerateBlockStep& {
  const auto* block = std::get_if<support::GenerateBlockStep>(&step);
  if (block == nullptr) {
    throw InternalError(
        "backend::cpp: the class of a generate block is named as standing in "
        "something other than a generate block -- please report this as a bug");
  }
  return *block;
}

// The name the class another unit reaches as `class_path` of `unit_name` is
// declared under. The declaring unit and every unit naming the class ask here
// with the same two facts, so they spell it alike with no record between them.
//
// The class an instance of the unit is takes the unit's name, and the class of
// a generate block the name the block has inside the class of the scope
// holding it. A class the source declared (LRM 8.3) is declared in the unit's
// namespace whichever scope of the unit wrote it, since a class of another
// unit may extend it; it keeps its own name where the unit itself declares it
// and that name is not the unit's, which the instance's class has taken; a
// specialization of a generic class (LRM 8.25) declared so takes a name
// carrying the digest of its bindings; and every other takes one made from its
// whole path.
[[nodiscard]] inline auto CppPublishedClassName(
    std::string_view unit_name, const support::DefPath& class_path) -> CppName {
  const std::span<const support::DefPathData> steps{class_path.data};
  if (steps.empty()) {
    return ToCppName(unit_name);
  }
  return std::visit(
      Overloaded{
          [&](const support::GenerateBlockStep& block) -> CppName {
            const std::string_view enclosing =
                steps.size() == 1
                    ? unit_name
                    : std::string_view{
                          BlockStepAt(steps[steps.size() - 2]).label};
            return CppNestedClassName(enclosing, block, steps.size() - 1);
          },
          [&](const support::ClassStep& cls) -> CppName {
            if (steps.size() != 1 || cls.name == unit_name) {
              return MintedPathName{.what = "class", .steps = steps};
            }
            if (cls.arguments.has_value()) {
              return MintedAppliedName{
                  .what = "spec",
                  .disambiguator = 0,
                  .arguments = cls.arguments,
                  .source = cls.name};
            }
            return ToCppName(cls.name);
          },
          [](const support::SubroutineStep&) -> CppName {
            ThrowNamesNoClass();
          },
          [](const support::NamedBlockStep&) -> CppName {
            ThrowNamesNoClass();
          },
          [](const support::UnnamedBlockStep&) -> CppName {
            ThrowNamesNoClass();
          },
          [](const support::TypeStep&) -> CppName { ThrowNamesNoClass(); },
          [](const support::UnnamedTypeStep&) -> CppName {
            ThrowNamesNoClass();
          }},
      steps.back());
}

// Whether a class is declared inside another, which is what keeps it from
// being declared ahead of its definition: C++ declares a nested class only
// inside the class holding it. The class of a generate block is, in the class
// of the scope holding the block.
[[nodiscard]] inline auto IsNestedClass(const support::DefPath& class_path)
    -> bool {
  return !class_path.data.empty() && support::NamesScopeClass(class_path);
}

// A class another unit may name as text anywhere in its unit's namespace names
// it: its own name, after that of each class it is nested in, `Top::g::inner`.
struct ClassPathInUnit {
  std::string_view unit_name;
  const support::DefPath* class_path;
};

inline void WriteOne(TargetText& out, ClassPathInUnit path) {
  const support::DefPath& class_path = *path.class_path;
  if (!support::NamesScopeClass(class_path)) {
    Write(out, CppPublishedClassName(path.unit_name, class_path));
    return;
  }
  Write(out, ToCppName(path.unit_name));
  std::string_view enclosing = path.unit_name;
  for (std::size_t depth = 0; depth < class_path.data.size(); ++depth) {
    const support::GenerateBlockStep& block =
        BlockStepAt(class_path.data[depth]);
    Write(out, "::", CppNestedClassName(enclosing, block, depth));
    enclosing = block.label;
  }
}

// The name of a class of this unit where it is declared: the name other units
// reach it by, or `sv_scope_<n>` for a class no other unit names, which is what
// realizes a scope of the design hierarchy.
[[nodiscard]] inline auto CppClassName(
    const mir::CompilationUnit& unit, mir::ClassId id) -> CppName {
  const mir::Class& cls = unit.GetClass(id);
  if (cls.path.has_value()) {
    return CppPublishedClassName(unit.name, *cls.path);
  }
  return MintedCppName("scope", id.value);
}

// A class of this unit as text anywhere in the unit's namespace names it.
class OwnClassPath {
 public:
  OwnClassPath(const mir::CompilationUnit& unit, mir::ClassId id)
      : unit_(&unit), id_(id) {
  }

  [[nodiscard]] auto Unit() const -> const mir::CompilationUnit& {
    return *unit_;
  }
  [[nodiscard]] auto Id() const -> mir::ClassId {
    return id_;
  }

 private:
  const mir::CompilationUnit* unit_;
  mir::ClassId id_;
};

[[nodiscard]] inline auto CppClassPath(
    const mir::CompilationUnit& unit, mir::ClassId id) -> OwnClassPath {
  return {unit, id};
}

inline void WriteOne(TargetText& out, const OwnClassPath& path) {
  const mir::Class& cls = path.Unit().GetClass(path.Id());
  if (cls.path.has_value()) {
    Write(
        out, ClassPathInUnit{
                 .unit_name = path.Unit().name, .class_path = &*cls.path});
    return;
  }
  Write(out, CppClassName(path.Unit(), path.Id()));
}

// A class of another unit from anywhere: `::Top::Top::g` for the class of a
// generate block, `::Pkg::C` for a class the source declared.
struct ExternalClassPath {
  std::string_view unit_name;
  const support::DefPath* class_path;
};

[[nodiscard]] inline auto CppExternalClassPath(
    std::string_view unit_name, const support::DefPath& class_path)
    -> ExternalClassPath {
  return ExternalClassPath{.unit_name = unit_name, .class_path = &class_path};
}

inline void WriteOne(TargetText& out, ExternalClassPath path) {
  Write(
      out, CppUnitScope(path.unit_name), "::",
      ClassPathInUnit{
          .unit_name = path.unit_name, .class_path = path.class_path});
}

// The file that declares a class another unit may name, for text that only
// points at it, and the file that defines it, for text that needs it complete.
// Every party asks here with the unit and the class's path in it, so the unit
// writing a class and a unit including it reach the same file.
//
// A class nested in another is declared nowhere but where that one is
// defined, in the unit's header; every other is declared in the forward
// header. What a unit published of a scope of the design hierarchy (LRM 23.6)
// stands in the runtime's tree, and no class outside its unit extends it, so
// those are defined together in the unit's header; a class the source declared
// (LRM 8.3) is defined in a file of its own, which a class extending it
// includes.
[[nodiscard]] inline auto ClassDeclarationFileOf(
    std::string_view unit_name, const support::DefPath& class_path)
    -> std::string {
  return IsNestedClass(class_path) ? UnitSignatureFileOf(unit_name)
                                   : UnitForwardFileOf(unit_name);
}

[[nodiscard]] inline auto ClassDefinitionFileOf(
    std::string_view unit_name, const support::DefPath& class_path)
    -> std::string {
  return support::NamesScopeClass(class_path)
             ? UnitSignatureFileOf(unit_name)
             : UnitClassFileOf(
                   unit_name, CppPublishedClassName(unit_name, class_path));
}

// The name the struct any unit reaches as `path` of its unit is declared
// under, in the unit's types namespace below. The declaring unit and every
// unit naming the struct ask here with the same path, so they spell it alike
// with no record between them.
//
// A struct the unit's own scope declares under a name keeps that name, so the
// output reads as the source does. Every other shares that namespace with
// structs of other scopes that may carry its name -- one a generate block, a
// class or a subroutine declares (LRM 6.22) -- or has none, and takes a name
// made from its whole path.
[[nodiscard]] inline auto CppStructNameOf(const support::DefPath& path)
    -> CppName {
  const std::span<const support::DefPathData> steps{path.data};
  const auto from_whole_path = [&]() -> CppName {
    return MintedPathName{.what = "struct", .steps = steps};
  };
  if (steps.size() != 1) {
    return from_whole_path();
  }
  return std::visit(
      Overloaded{
          [](const support::TypeStep& type) -> CppName {
            return ToCppName(type.name);
          },
          [&](const support::UnnamedTypeStep&) { return from_whole_path(); },
          [&](const support::GenerateBlockStep&) { return from_whole_path(); },
          [&](const support::ClassStep&) { return from_whole_path(); },
          [&](const support::SubroutineStep&) { return from_whole_path(); },
          [&](const support::NamedBlockStep&) { return from_whole_path(); },
          [&](const support::UnnamedBlockStep&) { return from_whole_path(); }},
      steps.front());
}

// The name of a struct this unit declares.
[[nodiscard]] inline auto CppStructName(const mir::StructDecl& decl)
    -> CppName {
  return CppStructNameOf(decl.path);
}

// The namespace inside a unit's own that the structs the source declares are
// declared in, `sv_types`. A struct takes its name from the type names of the
// scope declaring it, and the unit's namespace also holds the class an instance
// of the unit is, which takes the unit's name from the name space of design
// elements (LRM 3.13); keeping the two apart is what lets a module declare a
// struct under its own name.
[[nodiscard]] inline auto CppStructTypesNamespace() -> MintedWord {
  return MintedWord{.word = "types"};
}

// A struct as every unit names it, `::Unit::sv_types::Name`. A value of it is
// held whole, so text naming it needs the struct defined ahead of it, and
// writing the name says which file that is.
struct StructRef {
  std::string_view unit_name;
  CppName name;
};

[[nodiscard]] inline auto CppStructRef(
    std::string_view unit_name, const support::DefPath& path) -> StructRef {
  return StructRef{.unit_name = unit_name, .name = CppStructNameOf(path)};
}

inline void WriteOne(TargetText& out, const StructRef& ref) {
  out.Require(UnitStructFileOf(ref.unit_name, ref.name));
  Write(
      out, CppUnitScope(ref.unit_name), "::", CppStructTypesNamespace(),
      "::", ref.name);
}

// The name of a closure's type, `sv_closure_<n>`: a position in the unit's
// list of closures, since the source names none.
[[nodiscard]] inline auto CppClosureName(mir::ClosureId id) -> MintedName {
  return MintedCppName("closure", id.value);
}

// The name of a closure capture, `sv_capture_<n>`. A capture is a member of
// the closure's type, reached from its body through the closure itself, and
// the body's parameters and locals carry source names, so it gets a name no
// source name can produce.
[[nodiscard]] inline auto CppClosureCaptureName(mir::FieldId slot)
    -> MintedName {
  return MintedCppName("capture", slot.value);
}

// The function a closure whose body completes as a coroutine is started
// through, `sv_start`. It takes the closure by value, so the captures live in
// the coroutine's own frame for as long as the execution does.
[[nodiscard]] inline auto CppClosureStartName() -> MintedWord {
  return MintedWord{.word = "start"};
}

// The closure that function is handed, `sv_closure`. The body's own locals
// carry source names, so it gets a name no source name can produce.
[[nodiscard]] inline auto CppStartedClosureName() -> MintedWord {
  return MintedWord{.word = "closure"};
}

// The name of an enumeration's member table, `sv_enum_<n>`: a position in the
// unit's list of them, since the source names none.
[[nodiscard]] inline auto CppEnumTableName(mir::EnumTableId table)
    -> MintedName {
  return MintedCppName("enum", table.value);
}

// The name of a constant value the unit uses, `sv_const_<n>`: a position in the
// unit's list of constants, since the source wrote the value and no name.
[[nodiscard]] inline auto CppIntegralConstantName(mir::IntegralConstantId c)
    -> MintedName {
  return MintedCppName("const", c.value);
}

// The name a struct gives the library product it is built on, `sv_base`,
// through which it takes that product's constructors as its own. A member of
// the struct, so no source name reaches it.
[[nodiscard]] inline auto CppTupleBaseName() -> MintedWord {
  return MintedWord{.word = "base"};
}

// The member a struct answers an operation on its whole value with: the
// operator, or the identifier the library's entry for the question declares on
// every value, so the library's own code asking a value of the struct finds it
// by the name it asks every value by.
[[nodiscard]] inline auto CppStructMethodName(support::ValueOperation operation)
    -> VerbatimName {
  return std::visit(
      Overloaded{
          [](support::ValueOperator op) -> VerbatimName {
            switch (op) {
              case support::ValueOperator::kEquality:
                return VerbatimName{.text = "operator=="};
              case support::ValueOperator::kInequality:
                return VerbatimName{.text = "operator!="};
            }
            throw InternalError("backend::cpp: unknown value operator");
          },
          [](support::BuiltinFn fn) -> VerbatimName {
            return std::visit(
                Overloaded{
                    [](const support::Method& m) -> VerbatimName {
                      return VerbatimName{.text = m.identifier};
                    },
                    [](const support::StaticFactory& f) -> VerbatimName {
                      return VerbatimName{.text = f.identifier};
                    },
                    [](const support::FreeFunction&) -> VerbatimName {
                      throw InternalError(
                          "backend::cpp: a struct answers a question the "
                          "library asks of a value or of its type, and this "
                          "entry is declared on neither -- please report this "
                          "as a bug");
                    }},
                support::RuntimeEntryOf(fn).declaration);
          }},
      operation);
}

// The name of a class's definition, `sv_definition`. Another unit names it as
// `Class::sv_definition` for a class it only knows by name, so it is a fixed
// word rather than a position.
[[nodiscard]] inline auto CppDefinitionName() -> MintedWord {
  return MintedWord{.word = "definition"};
}

// The name of a constant a class holds, `sv_data_<n>`: a position in the
// class's list of constants, since the source names none.
[[nodiscard]] inline auto CppClassConstantName(mir::ClassConstantId constant)
    -> MintedName {
  return MintedCppName("data", constant.value);
}

// A DPI-C linkage name, written exactly as given. It is a C identifier (LRM
// 35.4) that the user's own C code also spells, so it cannot be escaped or
// prefixed.
[[nodiscard]] inline auto CppForeignSymbolName(std::string_view linkage_name)
    -> VerbatimName {
  return VerbatimName{.text = linkage_name};
}

// The name of one of a unit's fixed entries, such as `sv_create`. The source
// names none of them, but another unit calls them, so each has a fixed word
// both units write.
[[nodiscard]] inline auto CppMintedEntryName(mir::MintedEntry entry)
    -> MintedWord {
  switch (entry) {
    case mir::MintedEntry::kInstallStorage:
      return MintedWord{.word = "install_namespace_storage"};
    case mir::MintedEntry::kInitializeStorage:
      return MintedWord{.word = "initialize_namespace_storage"};
    case mir::MintedEntry::kMakeObject:
      return MintedWord{.word = "create"};
  }
  throw InternalError("backend::cpp: unknown minted entry");
}

// The name of a function in a unit's namespace, chosen by how other code
// reaches it: its DPI-C linkage name, its source name, its fixed entry word, or
// `sv_body_<n>` if nothing outside the unit reaches it.
[[nodiscard]] inline auto CppUnitCallableName(
    const mir::CompilationUnit& unit, mir::CallableId body) -> CppName {
  return std::visit(
      Overloaded{
          [](const mir::ReachedByLinkageName& r) -> CppName {
            return CppForeignSymbolName(r.name);
          },
          [](const mir::ReachedByName& r) -> CppName {
            return ToCppName(r.name);
          },
          [](const mir::ReachedByMintedEntry& r) -> CppName {
            return CppMintedEntryName(r.entry);
          },
          [](const mir::ReachedByPosition& r) -> CppName {
            return MintedCppName("body", r.slot.value);
          }},
      mir::NamespaceReachOf(unit, body));
}

}  // namespace lyra::backend::cpp
