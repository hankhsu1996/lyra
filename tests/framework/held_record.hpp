#pragma once

#include <filesystem>
#include <map>
#include <set>
#include <string>
#include <vector>
#include <yaml-cpp/yaml.h>

namespace lyra::test {

// A file mapping a name to something said of it, read as `Said`. A file holding
// only its own explanation parses to nothing and maps nothing.
template <typename Said>
auto LoadByName(const std::filesystem::path& yaml)
    -> std::map<std::string, Said> {
  std::map<std::string, Said> by_name;
  for (const auto& entry : YAML::LoadFile(yaml.string())) {
    by_name.emplace(entry.first.as<std::string>(), entry.second.as<Said>());
  }
  return by_name;
}

// Where what a run found of one subject and what its record lists part ways.
struct Departures {
  // Found, and not in the record: something new fell short.
  std::vector<std::string> not_recorded;
  // In the record, and not found: it no longer falls short, so its line goes.
  std::vector<std::string> no_longer_found;

  [[nodiscard]] auto Empty() const -> bool {
    return not_recorded.empty() && no_longer_found.empty();
  }
};

// Holds one subject to exactly what its record lists. A record kept this way
// only ever shrinks: a new shortfall fails the run until it is fixed, and one
// that is fixed fails the run until its line is taken out, so what is left in
// the file is what is still owed.
auto HoldToRecord(
    const std::set<std::string>& recorded, const std::set<std::string>& found)
    -> Departures;

}  // namespace lyra::test
