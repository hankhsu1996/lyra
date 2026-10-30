#pragma once

#include <expected>
#include <filesystem>
#include <optional>
#include <string>
#include <vector>

namespace conformance::publish {

// One night's records, to be published as one run of the suite on each
// configuration. A record is a run of one tool under one set of options, named
// by whoever ran it; the night is published only where every record is a whole
// run of the suite as it is read here, at the revision named, and no two are
// of one configuration, since a verdict means anything only beside the case it
// judged and the configuration that gave it.
struct Request {
  // The suite the records were judged against, as this program reads it.
  std::filesystem::path suite;
  // The revision of that suite, which every record must name as its own.
  std::string revision;
  // Where the suite's files can be read at any revision, so a reader can open
  // the case a verdict is about; nothing where there is no such place.
  std::optional<std::string> repository;
  // Where the run that wrote the records can be read, its logs above all.
  std::optional<std::string> run;
  std::vector<std::filesystem::path> records;
  // Where the published run is written for the page to read.
  std::filesystem::path site;
};

// Publishes the night, or says why it may not be published and writes
// nothing.
auto Publish(const Request& request) -> std::expected<void, std::string>;

}  // namespace conformance::publish
