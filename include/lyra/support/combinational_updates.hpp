#pragma once

#include <cstdint>
#include <functional>
#include <map>
#include <optional>
#include <string>
#include <string_view>

namespace lyra::support {

// What triggers an `always_comb` or `always_latch` (LRM 9.2.2.2, 9.2.2.3) when
// another such procedure's execution updates what it reads. A library states
// it, the command line selects it, HIR-to-MIR states it in the wait each such
// procedure builds, and the runtime carries out what that wait says.
enum class CombinationalUpdates : std::uint8_t {
  // Each update is an event (LRM 4.3, 4.5, 4.9.3), as the standard reads.
  kPerChange,
  // The updates one such procedure's execution makes to what another reads
  // are coalesced: the reader is triggered by the net change the execution
  // leaves, and a value written and written back is no change to it. Two
  // procedures each assigning a default and then a value, each reading the
  // other's output, then settle where under the standard's reading a schedule
  // running each to its stop never does. A procedure with side effects can run
  // fewer times than the standard says, so it is a dialect a design is built
  // in, not a reading of the standard.
  kPerInterval,
};

// The dialect each library of a build is compiled in. A library is written
// for one reading of the standard and keeps it whatever uses it, so one that
// states nothing is read as the standard reads; the command line's answer,
// either way, is every library's.
struct CombinationalUpdatesByLibrary {
  std::optional<CombinationalUpdates> every_library;
  std::map<std::string, CombinationalUpdates, std::less<>> by_library;

  [[nodiscard]] auto Of(std::string_view library) const
      -> CombinationalUpdates {
    if (every_library.has_value()) {
      return *every_library;
    }
    const auto stated = by_library.find(library);
    return stated != by_library.end() ? stated->second
                                      : CombinationalUpdates::kPerChange;
  }
};

}  // namespace lyra::support
