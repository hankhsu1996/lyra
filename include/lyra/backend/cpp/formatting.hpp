#pragma once

#include <cstdint>
#include <optional>
#include <string_view>

#include "lyra/backend/cpp/naming.hpp"
#include "lyra/backend/cpp/render_type.hpp"
#include "lyra/backend/cpp/target_text.hpp"
#include "lyra/base/internal_error.hpp"

namespace lyra::backend::cpp {

// Who owns a declared variable: each object of a class, the class itself (a
// static property, LRM 8.9), or the unit's namespace (a package variable, LRM
// 26.2).
enum class CellOwner : std::uint8_t { kObject, kType, kNamespace };

// Whether this text is the variable's definition or only declares one defined
// elsewhere. A variable another unit may name is declared in the header and
// defined in the code file.
enum class CellText : std::uint8_t { kAnnounced, kDefined };

// A variable declaration, described by what the caller knows: owner,
// definition or not, const or not, type and name. The keywords and punctuation
// those imply are chosen in one place, below, so no two sites write the same
// declaration differently.
struct DeclaredCell {
  CellOwner owner = CellOwner::kObject;
  CellText text = CellText::kDefined;
  bool immutable = false;
  CppType type;
  CppName name;
  // The class the name is qualified with, `C::name`, for a definition written
  // outside its class.
  std::optional<CppName> qualifier;
};

// The keywords before the type. A definition outside its class takes none:
// `static` and `extern` go only where the variable is declared.
[[nodiscard]] inline auto CellKeywords(const DeclaredCell& cell)
    -> std::string_view {
  if (cell.qualifier.has_value()) {
    return "";
  }
  switch (cell.owner) {
    case CellOwner::kObject:
      return "";
    case CellOwner::kType:
      // `inline static` defines one cell for the whole program, however many
      // files include the class.
      return cell.text == CellText::kAnnounced ? "static " : "inline static ";
    case CellOwner::kNamespace:
      return cell.text == CellText::kAnnounced ? "extern " : "";
  }
  throw InternalError("backend::cpp: a declared cell belongs to no owner");
}

inline void WriteDeclarationUpToTheValue(
    TargetText& out, const DeclaredCell& cell) {
  out.OpenLine();
  out += CellKeywords(cell);
  if (cell.immutable) {
    out += "const ";
  }
  Write(out, cell.type, " ");
  if (cell.qualifier.has_value()) {
    Write(out, *cell.qualifier, "::");
  }
  Write(out, cell.name);
}

// A declaration with no value: `T name;`, or `T name{};` for a definition, so
// a scalar starts at zero rather than at whatever the memory held.
inline void WriteDeclaration(TargetText& out, const DeclaredCell& cell) {
  WriteDeclarationUpToTheValue(out, cell);
  if (cell.text == CellText::kDefined) {
    out += "{}";
  }
  out += ";\n";
}

// A definition with a value, `T name = value;`, where `write_value` writes the
// value.
template <typename WriteValue>
void WriteDeclaration(
    TargetText& out, const DeclaredCell& cell, WriteValue write_value) {
  WriteDeclarationUpToTheValue(out, cell);
  out += " = ";
  write_value(out);
  out += ";\n";
}

// `namespace N {` and its closing `}  // namespace N`.
inline void OpenNamespace(TargetText& out, SourceName name) {
  Write(out, "namespace ", name, " {\n");
}

inline void CloseNamespace(TargetText& out, SourceName name) {
  Write(out, "}  // namespace ", name, "\n");
}

// Text built separately, added as a section of this file: set apart by a blank
// line, unless it is empty.
inline void AppendSection(TargetText& out, const TargetText& section) {
  const TargetText::Section placed(out);
  out += section.View();
}

}  // namespace lyra::backend::cpp
