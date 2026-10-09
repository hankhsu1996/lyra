#pragma once

#include <cstdint>
#include <optional>
#include <string_view>

#include "lyra/backend/cpp/naming.hpp"
#include "lyra/backend/cpp/render_type.hpp"
#include "lyra/backend/cpp/target_text.hpp"
#include "lyra/base/internal_error.hpp"

namespace lyra::backend::cpp {

// Which C++ declaration a variable is written as. Each is one of the forms the
// language has, named as the language names it:
//
//   kNonStaticDataMember           T name{};                 in a class
//   kInlineStaticDataMember        inline static T name{};   in a class
//   kStaticDataMemberDeclaration   static T name;            in a class
//   kStaticDataMemberDefinition    T Class::name = value;    at namespace scope
//   kExternDeclaration             extern T name;            at namespace scope
//   kNamespaceScopeDefinition      T name = value;           at namespace scope
//
// An inline static data member is defined where the class is, however many
// files include it. One that is not inline is declared in the class and defined
// once outside it, which is what a member whose value names something only the
// code file declares has to be. A variable of the namespace another unit may
// name is declared `extern` in the header and defined in the code file.
enum class VariableForm : std::uint8_t {
  kNonStaticDataMember,
  kInlineStaticDataMember,
  kStaticDataMemberDeclaration,
  kStaticDataMemberDefinition,
  kExternDeclaration,
  kNamespaceScopeDefinition,
};

// A variable declaration, described by what the caller knows: which form it
// takes, whether it is const, its type and name, and for a definition written
// outside its class the class that qualifies the name. The specifiers and
// punctuation those imply are chosen in one place, below, so no two sites
// write the same declaration differently.
struct VariableDeclaration {
  VariableForm form = VariableForm::kNonStaticDataMember;
  bool is_const = false;
  CppType type;
  CppName name;
  std::optional<OwnClassPath> qualifier = std::nullopt;
};

[[nodiscard]] inline auto StorageSpecifiers(VariableForm form)
    -> std::string_view {
  switch (form) {
    case VariableForm::kNonStaticDataMember:
      return "";
    case VariableForm::kInlineStaticDataMember:
      return "inline static ";
    case VariableForm::kStaticDataMemberDeclaration:
      return "static ";
    case VariableForm::kStaticDataMemberDefinition:
      return "";
    case VariableForm::kExternDeclaration:
      return "extern ";
    case VariableForm::kNamespaceScopeDefinition:
      return "";
  }
  throw InternalError("backend::cpp: unknown variable form");
}

inline void WriteDeclarationUpToTheValue(
    TargetText& out, const VariableDeclaration& variable) {
  out.OpenLine();
  out += StorageSpecifiers(variable.form);
  if (variable.is_const) {
    out += "const ";
  }
  Write(out, variable.type, " ");
  if (variable.qualifier.has_value()) {
    Write(out, *variable.qualifier, "::");
  }
  Write(out, variable.name);
}

// A declaration with no value: `T name;` where it is only a declaration, and
// `T name{};` where it is a definition, so a scalar starts at zero rather than
// at whatever the memory held.
inline void WriteDeclaration(
    TargetText& out, const VariableDeclaration& variable) {
  WriteDeclarationUpToTheValue(out, variable);
  switch (variable.form) {
    case VariableForm::kStaticDataMemberDeclaration:
    case VariableForm::kExternDeclaration:
      break;
    case VariableForm::kNonStaticDataMember:
    case VariableForm::kInlineStaticDataMember:
    case VariableForm::kStaticDataMemberDefinition:
    case VariableForm::kNamespaceScopeDefinition:
      out += "{}";
      break;
  }
  out += ";\n";
}

// A definition with a value, `T name = value;`, where `write_value` writes the
// value.
template <typename WriteValue>
void WriteDeclaration(
    TargetText& out, const VariableDeclaration& variable,
    WriteValue write_value) {
  WriteDeclarationUpToTheValue(out, variable);
  out += " = ";
  write_value(out);
  out += ";\n";
}

// `namespace N {` and its closing `}  // namespace N`.
template <typename Name>
void OpenNamespace(TargetText& out, const Name& name) {
  Write(out, "namespace ", name, " {\n");
}

template <typename Name>
void CloseNamespace(TargetText& out, const Name& name) {
  Write(out, "}  // namespace ", name, "\n");
}

// Text built separately, added as a section of this file: set apart by a blank
// line, unless it is empty.
inline void AppendSection(TargetText& out, const TargetText& section) {
  const TargetText::Section placed(out);
  out += section;
}

// The same, inside the namespace `name`, which is opened only around text
// there is.
template <typename Name>
void AppendSectionInNamespace(
    TargetText& out, const Name& name, const TargetText& section) {
  if (section.View().empty()) {
    return;
  }
  const TargetText::Section placed(out);
  OpenNamespace(out, name);
  out += section;
  CloseNamespace(out, name);
}

}  // namespace lyra::backend::cpp
