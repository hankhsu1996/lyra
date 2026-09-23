#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/backend/cpp/target_text.hpp"
#include "lyra/base/internal_error.hpp"

namespace lyra::backend::cpp {

// The comma-separated form of already-spelled parts. A type spelled out of
// other types composes their spellings, which are names rather than text of the
// program, so they are values and this joins them.
[[nodiscard]] inline auto JoinCommaSeparated(
    const std::vector<std::string>& parts) -> std::string {
  std::string out;
  for (const std::string& part : parts) {
    if (!out.empty()) out.append(", ");
    out.append(part);
  }
  return out;
}

// Whose cell a declaration brings into being: one that every object of a class
// holds, one the type itself owns (LRM 8.9), or one the unit's namespace owns
// (LRM 26.2).
enum class CellOwner : std::uint8_t { kObject, kType, kNamespace };

// Whether this text defines the cell or announces one defined elsewhere. A unit
// is emitted as two artifacts, so a cell a referrer may name is written twice,
// and only the definition establishes a value.
enum class CellText : std::uint8_t { kAnnounced, kDefined };

// One declared cell, in the parts a declaration is spelled from. A site states
// what it knows -- whose cell this is, whether this text defines it, whether
// its value is fixed, its type and its name -- and states nothing about
// keywords, punctuation, or where a qualifier goes. Those are this target's
// alone, and spelling them in one place is what stops two sites writing one
// declaration two ways.
struct DeclaredCell {
  CellOwner owner = CellOwner::kObject;
  CellText text = CellText::kDefined;
  bool immutable = false;
  std::string_view type;
  std::string_view name;
  // The declaration this text sits outside of, for a definition written apart
  // from the class that declares the cell. Its absence is also what says the
  // text sits inside that class, which is where a storage keyword is spelled
  // and where it is spelled once.
  std::optional<std::string_view> qualifier;
};

// The keywords this target wants before the cell's type. A definition written
// apart from its class repeats none of them, because a storage class is spelled
// where the member is declared, so that case is answered ahead of the rest.
[[nodiscard]] inline auto CellKeywords(const DeclaredCell& cell)
    -> std::string_view {
  if (cell.qualifier.has_value()) {
    return "";
  }
  switch (cell.owner) {
    case CellOwner::kObject:
      return "";
    case CellOwner::kType:
      // A type's cell is one for the whole program however many translation
      // units read the class, which is what the pair of keywords buys where the
      // member is declared.
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
  out += cell.type;
  out += " ";
  if (cell.qualifier.has_value()) {
    out += *cell.qualifier;
    out += "::";
  }
  out += cell.name;
}

// A declaration stating no value of its own: what the cell needs in front of
// it, its type, its name under whatever qualifies it, and the semicolon. A
// definition that states nothing still establishes a value -- the language's
// own default, spelled so a scalar is zeroed rather than left holding whatever
// the storage had.
inline void WriteDeclaration(TargetText& out, const DeclaredCell& cell) {
  WriteDeclarationUpToTheValue(out, cell);
  if (cell.text == CellText::kDefined) {
    out += "{}";
  }
  out += ";\n";
}

// A declaration whose value is an expression of the program, written where the
// declaration puts it rather than handed over as text.
template <typename WriteValue>
void WriteDeclaration(
    TargetText& out, const DeclaredCell& cell, WriteValue write_value) {
  WriteDeclarationUpToTheValue(out, cell);
  out += " = ";
  write_value(out);
  out += ";\n";
}

// A namespace enclosing what is written between the two. Opening one and
// closing it are the same decision seen twice -- the closing comment repeats
// the name -- so both are spelled here.
inline void OpenNamespace(TargetText& out, std::string_view name) {
  out += "namespace ";
  out += name;
  out += " {\n";
}

inline void CloseNamespace(TargetText& out, std::string_view name) {
  out += "}  // namespace ";
  out += name;
  out += "\n";
}

// Text already assembled elsewhere, placed as a section of this artifact.
inline void AppendSection(TargetText& out, const TargetText& section) {
  const TargetText::Section placed(out);
  out += section.View();
}

}  // namespace lyra::backend::cpp
