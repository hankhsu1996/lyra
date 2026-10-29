#pragma once

#include <optional>
#include <string>
#include <string_view>

namespace lyra::test {

// Where a dump a run wrote differs from the one a case expects, read as the
// standard reads a dump (LRM 21.7.2, 21.7.4): the scopes by kind and name, what
// each name is declared as, and at each time the sections opened and the value
// each name holds. White space, the identifier codes, the header commands and
// a vector written shortened to the bits that do not extend to the left are
// the tool's and never a difference. The first difference found, or nothing
// where the two record the same; a written dump that does not parse differs.
auto DumpDifference(std::string_view expected, std::string_view written)
    -> std::optional<std::string>;

// Why `text` cannot be read as a dump, or nothing where it can. A case's own
// expected dump is checked by it when the case is loaded, since one that does
// not parse is a fault of the case and never of a tool.
auto DumpUnreadable(std::string_view text) -> std::optional<std::string>;

}  // namespace lyra::test
