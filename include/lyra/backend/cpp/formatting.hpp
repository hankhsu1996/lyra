#pragma once

#include <algorithm>
#include <cstddef>
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
