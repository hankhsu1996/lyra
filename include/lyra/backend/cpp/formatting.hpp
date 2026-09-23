#pragma once

#include <cstddef>
#include <cstdint>
#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/base/internal_error.hpp"

namespace lyra::backend::cpp {

[[nodiscard]] inline auto Indent(std::size_t level) -> std::string {
  std::string result(level * 2, ' ');
  return result;
}

// A blank line sets each section of an emitted body apart, and a section the
// subject has none of contributes nothing at all -- separator included -- so no
// caller asks whether its section is there.
inline void AppendSection(std::string& out, const std::string& section) {
  if (section.empty()) return;
  out += "\n";
  out += section;
}

// The comma-separated form of already-rendered parts. Empty parts yield the
// empty string, which is the form every such list takes when it has none.
[[nodiscard]] inline auto JoinCommaSeparated(
    const std::vector<std::string>& parts) -> std::string {
  std::string out;
  for (const std::string& part : parts) {
    if (!out.empty()) out.append(", ");
    out.append(part);
  }
  return out;
}

// The call form composed around the renders of a call's parts. One entry
// renders a call, so this is spelled once and the punctuation -- how many
// arguments there are and where the separators go -- is never a per-site
// decision. A construct that merely looks like a call, such as a
// member-initializer, is not one and does not reach here.
[[nodiscard]] inline auto CallOf(
    std::string_view callee, const std::vector<std::string>& args)
    -> std::string {
  return std::string{callee} + "(" + JoinCommaSeparated(args) + ")";
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
  // What the cell starts at, absent where the declaration states nothing. A
  // definition that states nothing still establishes a value: the language's
  // own default, spelled so a scalar is zeroed rather than left holding
  // whatever the storage had.
  std::optional<std::string_view> value;
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

// A declaration, whole: what the cell needs in front of it, its type, its name
// under whatever qualifies it, what it starts at, and the semicolon. Every site
// that declares storage comes here, so the shape of a declaration is one thing
// this target states rather than something each site arrives at.
[[nodiscard]] inline auto RenderDeclaration(
    const DeclaredCell& cell, std::size_t indent) -> std::string {
  std::string out = Indent(indent);
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
  if (cell.value.has_value()) {
    out += " = ";
    out += *cell.value;
  } else if (cell.text == CellText::kDefined) {
    out += "{}";
  }
  out += ";\n";
  return out;
}

// A namespace enclosing `body`, which is written whole and ends with its own
// newline. Opening one and closing it are the same decision seen twice -- the
// closing comment repeats the name -- so both are spelled here and a caller
// supplies only what goes inside.
[[nodiscard]] inline auto NamespaceBlockOf(
    std::string_view name, std::string_view body) -> std::string {
  return std::format(
      "namespace {} {{\n{}}}  // namespace {}\n", name, body, name);
}

}  // namespace lyra::backend::cpp
