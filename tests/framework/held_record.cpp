#include "held_record.hpp"

#include <algorithm>
#include <iterator>
#include <set>
#include <string>

namespace lyra::test {

auto HoldToRecord(
    const std::set<std::string>& recorded, const std::set<std::string>& found)
    -> Departures {
  Departures departures;
  std::ranges::set_difference(
      found, recorded, std::back_inserter(departures.not_recorded));
  std::ranges::set_difference(
      recorded, found, std::back_inserter(departures.no_longer_found));
  return departures;
}

}  // namespace lyra::test
