#include "conformance/publish/publish.hpp"

#include <algorithm>
#include <cctype>
#include <cstddef>
#include <expected>
#include <filesystem>
#include <format>
#include <fstream>
#include <ios>
#include <iterator>
#include <map>
#include <nlohmann/json.hpp>
#include <optional>
#include <set>
#include <string>
#include <string_view>
#include <system_error>
#include <utility>
#include <vector>

#include "conformance/framework/case.hpp"
#include "conformance/framework/results.hpp"

namespace conformance::publish {
namespace {

using Json = nlohmann::ordered_json;

// A record read whole, with the name of the configuration it is a run of, as
// the run gave it and as a file name can hold it, and where in it each case's
// entry is.
struct Record {
  std::filesystem::path file;
  Json json;
  std::string name;
  std::string key;
  std::map<std::string, std::size_t> position;
};

auto ReadJson(const std::filesystem::path& file)
    -> std::expected<Json, std::string> {
  std::ifstream in(file, std::ios::binary);
  if (!in) {
    return std::unexpected(std::format("'{}' cannot be read", file.string()));
  }
  Json json = Json::parse(in, nullptr, false);
  if (json.is_discarded()) {
    return std::unexpected(
        std::format("'{}' is not a whole JSON document", file.string()));
  }
  return json;
}

auto WriteJson(const std::filesystem::path& file, const Json& json)
    -> std::expected<void, std::string> {
  std::error_code made;
  std::filesystem::create_directories(file.parent_path(), made);
  std::ofstream out(file, std::ios::binary);
  out << json.dump(2) << '\n';
  if (made || !out) {
    return std::unexpected(std::format("could not write '{}'", file.string()));
  }
  return {};
}

// The name in lower case, with every run of anything but a letter or a digit
// made one dash.
auto KeyOf(std::string_view name) -> std::string {
  std::string key;
  for (const char c : name) {
    if (std::isalnum(static_cast<unsigned char>(c)) != 0) {
      key += static_cast<char>(std::tolower(static_cast<unsigned char>(c)));
    } else if (!key.empty() && key.back() != '-') {
      key += '-';
    }
  }
  while (key.ends_with('-')) {
    key.pop_back();
  }
  return key;
}

// Why a record is not one whole run of this suite at this revision, where it
// is not.
auto Refusal(
    const Record& record, std::string_view revision,
    const std::set<std::string>& ids) -> std::optional<std::string> {
  const Json& json = record.json;
  const std::string file = record.file.string();
  if (json.value("format_version", 0) != framework::kResultsFormatVersion) {
    return std::format(
        "'{}' is written in a format this does not read ({}, where this reads "
        "{})",
        file, json.value("format_version", 0),
        framework::kResultsFormatVersion);
  }
  if (!json.contains("selection") || !json["selection"].is_null()) {
    return std::format(
        "'{}' is a run of named cases, not of the whole suite", file);
  }
  const Json suite_revision = json["suite"].value("revision", Json());
  if (!suite_revision.is_string() ||
      suite_revision.get<std::string>() != revision) {
    return std::format(
        "'{}' was judged against the suite at revision {}, not at {}", file,
        suite_revision.is_string() ? suite_revision.get<std::string>()
                                   : std::string{"not stated"},
        revision);
  }
  // A case the suite does not hold says the record was judged on other cases
  // than these, which is the larger fault, so it is named first.
  for (const auto& [id, at] : record.position) {
    if (!ids.contains(id)) {
      return std::format(
          "'{}' judges '{}', which the suite at {} does not hold", file, id,
          revision);
    }
  }
  for (const std::string& id : ids) {
    if (!record.position.contains(id)) {
      return std::format("'{}' has no verdict on '{}'", file, id);
    }
  }
  // A case the harness could not hand to the tool leaves the run short of
  // the whole suite as surely as a case left out.
  for (const Json& entry : json["cases"]) {
    if (entry["verdict"]["kind"] == "not run") {
      return std::format(
          "'{}' did not run '{}': {}", file, entry["id"].get<std::string>(),
          entry["verdict"].value("why", "no reason is given"));
    }
  }
  return std::nullopt;
}

// The first field a record has and this document lacks, by its path in the
// record, or nothing where the document has every field the publisher reads
// in the shape the runner writes it. Past this, each is read without a test.
auto MissingField(const Json& json) -> std::optional<std::string> {
  const auto field = [](const Json& in, const char* key) -> const Json* {
    if (!in.is_object()) {
      return nullptr;
    }
    const auto found = in.find(key);
    return found == in.end() ? nullptr : &*found;
  };
  for (const char* key :
       {"format_version", "configuration", "tool", "host", "limits",
        "selection", "started", "ended"}) {
    if (field(json, key) == nullptr) {
      return std::string(key);
    }
  }
  for (const char* key : {"revision", "directory"}) {
    if (field(json["suite"], key) == nullptr) {
      return std::format("suite.{}", key);
    }
  }
  const Json* cases = field(json, "cases");
  if (cases == nullptr || !cases->is_array()) {
    return std::string("cases");
  }
  for (std::size_t at = 0; at < cases->size(); ++at) {
    const Json& entry = (*cases)[at];
    const Json* id = field(entry, "id");
    if (id == nullptr || !id->is_string()) {
      return std::format("cases[{}].id", at);
    }
    for (const char* part : {"verdict", "end"}) {
      const Json* within = field(entry, part);
      const Json* kind = within == nullptr ? nullptr : field(*within, "kind");
      if (kind == nullptr || !kind->is_string()) {
        return std::format("cases[{}].{}.kind", at, part);
      }
    }
  }
  return std::nullopt;
}

// The record in `file`, or why it is none: a document that does not parse,
// one lacking a field a record has, one naming no configuration it can be
// addressed by, or one judging a case twice.
auto ReadRecord(const std::filesystem::path& file)
    -> std::expected<Record, std::string> {
  auto json = ReadJson(file);
  if (!json) {
    return std::unexpected(json.error());
  }
  if (const auto missing = MissingField(*json)) {
    return std::unexpected(
        std::format(
            "'{}' is not a record: it has no {}", file.string(), *missing));
  }
  Record record{
      .file = file,
      .json = *std::move(json),
      .name = {},
      .key = {},
      .position = {}};
  const Json name = record.json.value("configuration", Json());
  if (!name.is_string() || name.get<std::string>().empty()) {
    return std::unexpected(
        std::format("'{}' names no configuration", file.string()));
  }
  record.name = name.get<std::string>();
  record.key = KeyOf(record.name);
  if (record.key.empty()) {
    return std::unexpected(
        std::format(
            "'{}' names the configuration '{}', which has no letter or digit "
            "to make its address of",
            file.string(), record.name));
  }
  const Json& cases = record.json["cases"];
  for (std::size_t at = 0; at < cases.size(); ++at) {
    const std::string id = cases[at]["id"].get<std::string>();
    if (!record.position.emplace(id, at).second) {
      return std::unexpected(
          std::format("'{}' judges '{}' more than once", file.string(), id));
    }
  }
  return record;
}

// The case's entry in the record. Every record has one, since a record
// without one is refused.
auto EntryOf(const std::string& id, const Record& record) -> const Json& {
  return record.json["cases"][record.position.at(id)];
}

// Where the case lies in the repository the suite is read from, as its
// files are found at the revision named.
auto PathOf(const Request& request, const framework::Case& c) -> std::string {
  return c.directory.lexically_relative(request.suite.parent_path())
      .generic_string();
}

// What the case says it checks, as its entry source states it in the comment
// it opens with: the paragraphs of that comment, past the directives, each
// joined into one line. Nothing where the case opens with no such comment.
auto StatementOf(const framework::Case& c) -> Json {
  std::ifstream in(c.entry);
  Json paragraphs = Json::array();
  std::string paragraph;
  std::string line;
  const auto close = [&] {
    if (!paragraph.empty()) {
      paragraphs.push_back(std::move(paragraph));
      paragraph.clear();
    }
  };
  while (std::getline(in, line)) {
    if (!line.starts_with("//")) {
      break;
    }
    std::string_view text = std::string_view(line).substr(2);
    while (text.starts_with(' ')) {
      text.remove_prefix(1);
    }
    if (text.starts_with('@')) {
      continue;
    }
    if (text.empty()) {
      close();
      continue;
    }
    if (!paragraph.empty()) {
      paragraph += ' ';
    }
    paragraph += text;
  }
  close();
  return paragraphs;
}

// Every file of the case, by its name in the case's directory, with its text,
// so a reader sees what the case states and checks beside what each tool did.
auto SourcesOf(const framework::Case& c) -> std::expected<Json, std::string> {
  std::vector<std::filesystem::path> files;
  std::error_code listed;
  for (const auto& entry :
       std::filesystem::recursive_directory_iterator(c.directory, listed)) {
    if (entry.is_regular_file()) {
      files.push_back(entry.path());
    }
  }
  if (listed) {
    return std::unexpected(
        std::format(
            "could not list '{}': {}", c.directory.string(), listed.message()));
  }
  // The entry source first, as a reader starts there, and the rest by name.
  std::ranges::sort(files, [&](const auto& a, const auto& b) {
    return std::pair(a != c.entry, a) < std::pair(b != c.entry, b);
  });
  Json sources = Json::array();
  for (const std::filesystem::path& file : files) {
    std::ifstream in(file, std::ios::binary);
    std::string text{
        std::istreambuf_iterator<char>(in), std::istreambuf_iterator<char>()};
    if (!in.good() && !in.eof()) {
      return std::unexpected(std::format("could not read '{}'", file.string()));
    }
    sources.push_back(
        {{"name", file.lexically_relative(c.directory).generic_string()},
         {"text", std::move(text)}});
  }
  return sources;
}

// What every page of the site reads first: which run this is, where its files
// came from, how each configuration ran, and each case's verdict and how its
// run ended on each, with no evidence, which stays beside each case.
auto Summarize(
    const Request& request, const std::vector<Record>& records,
    const framework::Suite& suite) -> Json {
  std::string started = records.front().json["started"].get<std::string>();
  Json configurations = Json::array();
  for (const Record& record : records) {
    const Json& json = record.json;
    started = std::min(started, json["started"].get<std::string>());
    configurations.push_back(
        {{"key", record.key},
         {"name", record.name},
         {"tool", json["tool"]},
         {"suite_directory", json["suite"]["directory"]},
         {"host", json["host"]},
         {"limits", json["limits"]},
         {"started", json["started"]},
         {"ended", json["ended"]},
         {"record", std::format("records/{}.json", record.key)}});
  }

  Json cases = Json::array();
  for (const auto* found : {&suite.cases, &suite.helper_checks}) {
    for (const framework::Case& c : *found) {
      Json results = Json::array();
      for (const Record& record : records) {
        const Json& entry = EntryOf(c.id, record);
        results.push_back(
            {{"verdict", entry["verdict"]["kind"]},
             {"why", entry["verdict"].value("why", Json())},
             {"end", entry["end"]["kind"]}});
      }
      cases.push_back(
          {{"id", c.id},
           {"path", PathOf(request, c)},
           {"helper_check", found == &suite.helper_checks},
           {"results", std::move(results)}});
    }
  }
  return {
      {"run",
       {{"source", request.run ? Json(*request.run) : Json()},
        {"started", started}}},
      {"suite",
       {{"revision", request.revision},
        {"repository",
         request.repository ? Json(*request.repository) : Json()}}},
      {"configurations", std::move(configurations)},
      {"cases", std::move(cases)}};
}

}  // namespace

auto Publish(const Request& request) -> std::expected<void, std::string> {
  if (request.records.empty()) {
    return std::unexpected("no record was named");
  }
  const framework::Suite suite = framework::LoadSuite(request.suite);
  std::set<std::string> ids;
  for (const auto* found : {&suite.cases, &suite.helper_checks}) {
    for (const framework::Case& c : *found) {
      ids.insert(c.id);
    }
  }
  if (ids.empty()) {
    return std::unexpected(
        std::format("no case of the suite is in '{}'", request.suite.string()));
  }

  std::vector<Record> records;
  for (const std::filesystem::path& file : request.records) {
    auto record = ReadRecord(file);
    if (!record) {
      return std::unexpected(record.error());
    }
    records.push_back(*std::move(record));
  }
  std::ranges::sort(records, {}, &Record::key);
  for (std::size_t i = 0; i < records.size(); ++i) {
    if (auto refused = Refusal(records[i], request.revision, ids)) {
      return std::unexpected(*std::move(refused));
    }
    if (i == 0 || records[i].key != records[i - 1].key) {
      continue;
    }
    const Record& first = records[i - 1];
    if (first.name == records[i].name) {
      return std::unexpected(
          std::format(
              "'{}' and '{}' are both runs of '{}'", first.file.string(),
              records[i].file.string(), records[i].name));
    }
    return std::unexpected(
        std::format(
            "'{}' and '{}' are runs of '{}' and '{}', whose addresses are both "
            "'{}'",
            first.file.string(), records[i].file.string(), first.name,
            records[i].name, records[i].key));
  }

  const Json summary = Summarize(request, records, suite);
  for (const Record& record : records) {
    const std::filesystem::path to =
        request.site / "records" / std::format("{}.json", record.key);
    std::error_code copied;
    std::filesystem::create_directories(to.parent_path(), copied);
    if (!copied) {
      std::filesystem::copy_file(
          record.file, to, std::filesystem::copy_options::overwrite_existing,
          copied);
    }
    if (copied) {
      return std::unexpected(
          std::format(
              "could not write '{}': {}", to.string(), copied.message()));
    }
  }
  for (const auto* found : {&suite.cases, &suite.helper_checks}) {
    for (const framework::Case& c : *found) {
      // By configuration, since a page holding this beside a summary of
      // another night must find a configuration by its key and never by its
      // place.
      Json results = Json::object();
      for (const Record& record : records) {
        results[record.key] = EntryOf(c.id, record);
      }
      auto sources = SourcesOf(c);
      if (!sources) {
        return std::unexpected(sources.error());
      }
      if (auto written = WriteJson(
              request.site / "cases" / std::format("{}.json", c.id),
              {{"id", c.id},
               {"path", PathOf(request, c)},
               {"helper_check", found == &suite.helper_checks},
               {"statement", StatementOf(c)},
               {"requirement", framework::Describe(c.claim)},
               {"sources", *std::move(sources)},
               {"results", std::move(results)}});
          !written) {
        return written;
      }
    }
  }
  if (auto written = WriteJson(request.site / "summary.json", summary);
      !written) {
    return written;
  }

  return {};
}

}  // namespace conformance::publish
