#pragma once

#include <algorithm>
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

// The comma-separated form of already-rendered parts: an argument list, a
// parameter list, a member-initializer list. Empty parts yield the empty
// string, which is the form each of those takes when it has none.
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
// unit that includes the header, which is the header-only, link-by-name model
// the emitted callables already use. `const` rather than `constexpr` because an
// initializer may name a runtime library value or an erased code address, and
// C++ admits neither in a constant expression; the storage is established
// before any process runs either way.
//
// The two forms differ only in the keyword a class needs to say what a
// namespace says by having no instances to be per-instance of, so each is
// named for its scope rather than selected by a flag.
[[nodiscard]] inline auto NamespaceConstantOf(
    std::string_view type, std::string_view name, std::string_view init)
    -> std::string {
  return std::format("inline const {} {} = {};\n", type, name, init);
}

[[nodiscard]] inline auto ClassConstantOf(
    std::string_view type, std::string_view name, std::string_view init)
    -> std::string {
  return std::format("inline static const {} {} = {};\n", type, name, init);
}

// A unit or class name spelled as a C++ identifier. A source name is already a
// valid identifier, but a synthesized name may carry a `$` (the design-root
// unit) that C++ does not admit; mapping it to `_` yields the token the emitted
// class, its constructor, and every reference to its type share.
[[nodiscard]] inline auto ToCppName(std::string_view name) -> std::string {
  std::string out{name};
  std::ranges::replace(out, '$', '_');
  return out;
}

}  // namespace lyra::backend::cpp
