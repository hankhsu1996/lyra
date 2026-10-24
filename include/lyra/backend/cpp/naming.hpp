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
#include "lyra/mir/bring_up_entry.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_constant_id.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/closure_id.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/enum_table_id.hpp"
#include "lyra/mir/external_class.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/integral_constant_id.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/record_decl.hpp"
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
// only makes it read worse, while missing one breaks the build. `main` is
// listed as well: the program's entry function takes it at the global scope
// ([basic.start.main]), where a unit's namespace is declared and after which
// its file is named.
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
     "main",
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
// `sv_type_b1_g_t7_entry_t` for a structure `entry_t` declared there. `what`
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

// The namespace what only one body of a unit states is in: one inside the
// unit's, named after the body, so the body's own text sees the unit's
// declarations and another body's stands apart from it. A body named as the
// unit is -- one whose code uses no parameter value -- has the unit's
// namespace, since a namespace inside it of the same name would be taken by
// the class of the unit's instance.
[[nodiscard]] inline auto BodyNamespaceOf(
    std::string_view unit_name, std::string_view body_name)
    -> std::optional<SourceName> {
  if (body_name == unit_name) return std::nullopt;
  return ToCppName(body_name);
}

// The qualifier for a reference into that namespace, `::Top` or `::Top::Body`.
struct BodyScope {
  std::string_view unit_name;
  std::string_view body_name;
};

void WriteOne(TargetText& out, BodyScope scope);

[[nodiscard]] inline auto CppBodyScope(
    std::string_view unit_name, std::string_view body_name) -> BodyScope {
  return BodyScope{.unit_name = unit_name, .body_name = body_name};
}

// The files a unit is emitted as. Both the unit writing a file and every unit
// including it compute the name from here, so a unit can include a file of
// another unit without seeing how that unit was emitted. A file name is a
// string rather than output text, because what uses it first is the code that
// writes the file to disk.
//
// Every body of a unit writes the unit's files and only one body writes its
// own, so no two kinds of file may be named alike: each but the unit's own
// header ends in a word naming its kind, and no name a file is made from
// holds a dot.

// `Top.hpp`: the classes of the unit's scopes, the class of a generate block
// declared inside the class of the scope holding it. It carries the unit's
// name alone, since it is the unit as the source declared it.
[[nodiscard]] inline auto UnitSignatureFileOf(std::string_view unit_name)
    -> std::string {
  return TextOf(ToCppName(unit_name), ".hpp");
}

// `Top.opening.hpp`: the cells and functions the unit's namespace declares. Of
// the program it includes only forward files and the files of the types its
// declarations name, so any class file, of this unit or another, can include
// it first.
[[nodiscard]] inline auto UnitOpeningFileOf(std::string_view unit_name)
    -> std::string {
  return TextOf(ToCppName(unit_name), ".opening.hpp");
}

// `Body.body.hpp`: the entry that makes an object of one body, which only that
// body writes. A body whose code uses no parameter value is named as its unit
// is, so this name stands apart from the unit's files by its kind alone.
[[nodiscard]] inline auto BodyHeaderFileOf(std::string_view body_name)
    -> std::string {
  return TextOf(ToCppName(body_name), ".body.hpp");
}

// `Body.cpp`: the translation unit of one body, defining everything the
// headers declare.
[[nodiscard]] inline auto BodyCodeFileOf(std::string_view body_name)
    -> std::string {
  return TextOf(ToCppName(body_name), ".cpp");
}

// `Top.forward.hpp`: every class another unit may name that is declared in no
// other class, declared and not defined. It includes nothing, so any file can
// include it first, and a unit holding another's class by pointer takes its
// name from here rather than declaring it again.
[[nodiscard]] inline auto UnitForwardFileOf(std::string_view unit_name)
    -> std::string {
  return TextOf(ToCppName(unit_name), ".forward.hpp");
}

// `Top.Pair.types.hpp`: one struct or union a unit declares, alone in its
// file, which a value of it needs complete wherever it is held. It includes the
// file of each it holds by value, so whichever file is included first, every
// one is defined after what it is built from. None can hold itself by value, so
// those files form no cycle -- while two units' may each hold the other's, so a
// file holding all of one unit's would.
[[nodiscard]] inline auto UnitRecordFileOf(
    std::string_view unit_name, const CppName& record_name) -> std::string {
  return TextOf(ToCppName(unit_name), ".", record_name, ".types.hpp");
}

// `Top.Base.class.hpp`: one class the source declared, alone in its file,
// which a class of another unit extending it needs complete. It includes the
// file of each class it derives from, so whichever file is included first,
// every class is defined after its bases and only once.
[[nodiscard]] inline auto UnitClassFileOf(
    std::string_view unit_name, const CppName& declared_as) -> std::string {
  return TextOf(ToCppName(unit_name), ".", declared_as, ".class.hpp");
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

[[nodiscard]] inline auto CppVirtualSlotName(
    const mir::CompilationUnit& unit, const mir::VirtualSlot& slot) -> CppName;

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
          [&](const mir::OverridesVirtualSlot& taken) -> CppName {
            return CppVirtualSlotName(unit, taken.overridden);
          },
          [](const mir::OverridesLibraryVirtual& taken) -> CppName {
            return VerbatimName{
                .text = support::LibraryVirtualName(taken.function)};
          }},
      *role);
}

// The name of the virtual method a slot names, which a virtual call through it
// and an override of it both spell.
[[nodiscard]] inline auto CppVirtualSlotName(
    const mir::CompilationUnit& unit, const mir::VirtualSlot& slot) -> CppName {
  return std::visit(
      Overloaded{
          [&](const mir::LocalVirtualSlot& local) -> CppName {
            return CppClassCallableName(
                unit, unit.GetClass(local.owner_class), local.slot);
          },
          [&](const mir::ExternalVirtualSlot& external) -> CppName {
            return CppExternalBehaviorName(
                unit, external.unit_name, external.class_path,
                external.ordinal);
          }},
      slot);
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

// The name the struct or union any unit reaches as `path` of its unit is
// declared under, in the unit's types namespace below. The declaring unit and
// every unit naming it ask here with the same path, so they spell it alike with
// nothing passed between them.
//
// One the unit's own scope declares under a name keeps that name, so the
// output reads as the source does. Every other shares that namespace with types
// of other scopes that may carry its name -- one a generate block, a class or a
// subroutine declares (LRM 6.22) -- or has none, and takes a name made from its
// whole path.
[[nodiscard]] inline auto CppRecordNameOf(const support::DefPath& path)
    -> CppName {
  const std::span<const support::DefPathData> steps{path.data};
  const auto from_whole_path = [&]() -> CppName {
    return MintedPathName{.what = "type", .steps = steps};
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

// The name of a struct or a union this unit declares.
[[nodiscard]] inline auto CppRecordName(const mir::RecordDecl& decl)
    -> CppName {
  return CppRecordNameOf(decl.path);
}

// The namespace inside a unit's own that the structs and unions the source
// declares are declared in, `sv_types`. Each takes its name from the type names
// of the scope declaring it, and the unit's namespace also holds the class an
// instance of the unit is, which takes the unit's name from the name space of
// design elements (LRM 3.13); keeping the two apart is what lets a module
// declare a type under its own name.
[[nodiscard]] inline auto CppRecordTypesNamespace() -> MintedWord {
  return MintedWord{.word = "types"};
}

// A struct or a union as every unit names it, `::Unit::sv_types::Name`. A
// value of it is held whole, so text naming it needs the type defined ahead of
// it, and writing the name says which file that is.
struct RecordQualifiedName {
  std::string_view unit_name;
  CppName name;
};

[[nodiscard]] inline auto CppRecordQualifiedName(
    std::string_view unit_name, const support::DefPath& path)
    -> RecordQualifiedName {
  return RecordQualifiedName{
      .unit_name = unit_name, .name = CppRecordNameOf(path)};
}

inline void WriteOne(TargetText& out, const RecordQualifiedName& ref) {
  out.Require(UnitRecordFileOf(ref.unit_name, ref.name));
  Write(
      out, CppUnitScope(ref.unit_name), "::", CppRecordTypesNamespace(),
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

// The function a closure whose body takes the closure over is called through,
// `sv_call_once`, as Rust's `FnOnce::call_once` takes `self`: it is handed the
// box the closure was made in, so the captures live for as long as the call
// does, a started execution included.
[[nodiscard]] inline auto CppClosureCallOnceName() -> MintedWord {
  return MintedWord{.word = "call_once"};
}

// The member function a class's constructor is, `sv_construct`, entered on an
// object already created whole, so the object is of its own class while every
// constructor runs (LRM 8.17) and a construction left partway leaves it to
// whatever refers to it (LRM 8.29).
[[nodiscard]] inline auto CppConstructorMemberName() -> MintedWord {
  return MintedWord{.word = "construct"};
}

// The constant naming how a closure is run and ended,
// `sv_closure_definition_<n>`, which the runtime reaches the closure through.
[[nodiscard]] inline auto CppClosureDefinitionName(mir::ClosureId id)
    -> MintedName {
  return MintedCppName("closure_definition", id.value);
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

// The name a struct or a union gives the library type it is built on,
// `sv_base`, through which it takes that type's constructors as its own. A
// member of the declared type, so no source name reaches it.
[[nodiscard]] inline auto CppRecordBaseName() -> MintedWord {
  return MintedWord{.word = "base"};
}

// The library type's accessor for one component, `Component<I>()`, which a
// field step to a component of a struct, a union or a tuple is spelled through.
// It is overloaded on the value category of what it is asked of, so reading a
// component, writing it and moving it out of an expiring value are the one
// access, as a C++ member access is.
[[nodiscard]] inline auto CppTupleComponentAccessor() -> VerbatimName {
  return VerbatimName{.text = "Component"};
}

// The library tagged union's accessor for the position of the member it
// holds, `Discriminant()`, which a read of a tag is spelled through.
[[nodiscard]] inline auto CppDiscriminantAccessor() -> VerbatimName {
  return VerbatimName{.text = "Discriminant"};
}

// The member a type answers an operation on its whole value with -- a record's
// method, or the library array template's own: the operator, or the identifier
// the library's entry for the question declares on every value, so the
// library's own code asking a value of the type finds it by the name it asks
// every value by.
[[nodiscard]] inline auto CppValueOperationMemberName(
    support::ValueOperation operation) -> VerbatimName {
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
                          "backend::cpp: a type's own operation answers a "
                          "question the library asks of a value or of its "
                          "type, and this entry is declared on neither -- "
                          "please report this as a bug");
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

// The name of one of a namespace unit's bring-up entries, such as
// `sv_install_namespace_storage`. The source names neither, but the design
// root calls them, so each has a fixed word both units write.
[[nodiscard]] inline auto CppBringUpEntryName(mir::BringUpEntry entry)
    -> MintedWord {
  switch (entry) {
    case mir::BringUpEntry::kInstallStorage:
      return MintedWord{.word = "install_namespace_storage"};
    case mir::BringUpEntry::kInitializeStorage:
      return MintedWord{.word = "initialize_namespace_storage"};
  }
  throw InternalError("backend::cpp: unknown bring-up entry");
}

// `sv_create`, the entry that makes an object of a body, in that body's
// namespace. The source names none, but the design root's tables name it, so
// it is a fixed word both units write.
[[nodiscard]] inline auto CppObjectEntryName() -> MintedWord {
  return MintedWord{.word = "create"};
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
          [](const mir::ReachedByBringUpEntry& r) -> CppName {
            return CppBringUpEntryName(r.entry);
          },
          [](const mir::ReachedAsObjectEntry&) -> CppName {
            return CppObjectEntryName();
          },
          [](const mir::ReachedByPosition& r) -> CppName {
            return MintedCppName("body", r.slot.value);
          }},
      mir::NamespaceReachOf(unit, body));
}

}  // namespace lyra::backend::cpp
