#pragma once

#include <cstddef>
#include <format>
#include <string>
#include <string_view>
#include <vector>

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

// A value settled before any process runs, defined where every reference
// reaches it by name. `inline` gives it one definition across every translation
// unit that includes the header. `const` rather than `constexpr` because an
// initializer may name a runtime library value or an erased code address, and
// C++ admits neither in a constant expression; the storage is established
// before any process runs either way. A constant a class owns is written
// differently, because what its value may name depends on where the value sits
// relative to the class; that form is below and is named for its scope rather
// than selected by a flag.
[[nodiscard]] inline auto NamespaceConstantOf(
    std::string_view type, std::string_view name, std::string_view init)
    -> std::string {
  return std::format("inline const {} {} = {};\n", type, name, init);
}

// A class constant, declared in the class and valued after it. The value names
// what the class holds either way, and one written after the class may also
// name a member declared below the constant -- which an initializer written in
// place cannot -- so this shape serves every one of them and there is no second
// shape to choose between.
//
// It is settled before any process runs and not before that, for the reason the
// namespace form gives above: an initializer holding an erased code address is
// no constant expression, so neither `constexpr` nor `constinit` is available
// to any of these. Nothing reads one during static initialization -- a constant
// of another class is reached by taking its address, which is constant whatever
// its value costs to build.
[[nodiscard]] inline auto ClassConstantDeclOf(
    std::string_view type, std::string_view name) -> std::string {
  return std::format("static const {} {};\n", type, name);
}

[[nodiscard]] inline auto ClassConstantDefOf(
    std::string_view scope, std::string_view type, std::string_view name,
    std::string_view init) -> std::string {
  return std::format("const {} {}::{} = {};\n", type, scope, name, init);
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
