// What may be published of a night's runs, and what a published run leaves:
// one whole run of the suite per configuration, each named by whoever ran it,
// all on the revision of the suite read here and kept apart by configuration,
// and nothing at all where any record falls short.

#include "conformance/publish/publish.hpp"

#include <filesystem>
#include <fstream>
#include <gtest/gtest.h>
#include <iterator>
#include <nlohmann/json.hpp>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "conformance/framework/process.hpp"

namespace conformance::publish {
namespace {

using Json = nlohmann::json;

constexpr std::string_view kRevision =
    "1111111111111111111111111111111111111111";
constexpr std::string_view kCaseSource =
    "module Top;\n"
    "  initial $display(\"All checks passed\");\n"
    "endmodule\n";

void WriteText(const std::filesystem::path& file, std::string_view text) {
  std::filesystem::create_directories(file.parent_path());
  std::ofstream(file, std::ios::binary) << text;
}

auto ReadText(const std::filesystem::path& file) -> std::string {
  std::ifstream in(file, std::ios::binary);
  return {std::istreambuf_iterator<char>(in), std::istreambuf_iterator<char>()};
}

auto ReadJson(const std::filesystem::path& file) -> Json {
  return Json::parse(ReadText(file));
}

// A run of the whole suite on one configuration, as the runner writes it.
auto ARecord(
    std::string configuration, std::string tool,
    std::vector<std::string> options) -> Json {
  Json cases = Json::array();
  for (const std::string_view id :
       {"01_first/one", "02_second/two", "harness/three"}) {
    cases.push_back(
        {{"id", id},
         {"seconds", 0.5},
         {"verdict", {{"kind", "conforms"}}},
         {"end", {{"kind", "ran"}, {"exit_code", 0}}},
         {"steps", Json::array()}});
  }
  return {
      {"format_version", 1},
      {"suite", {{"revision", kRevision}, {"directory", "/somewhere"}}},
      {"configuration", std::move(configuration)},
      {"tool",
       {{"name", std::move(tool)},
        {"revision", nullptr},
        {"programs", Json::array()},
        {"options", std::move(options)}}},
      {"environment", Json::object()},
      {"host", "Linux"},
      {"limits", {{"jobs", 4}}},
      {"selection", nullptr},
      {"started", "2026-10-30T05:03:46Z"},
      {"ended", "2026-10-30T05:10:16Z"},
      {"cases", std::move(cases)}};
}

auto Verilator() -> Json {
  return ARecord("Verilator", "verilator", {});
}

// A suite of two cases in two clauses and one check of its helpers, and a
// place for everything a publication writes, apart for each test.
class Publishing : public testing::Test {
 protected:
  void SetUp() override {
    auto made = framework::MakeScratchDir();
    ASSERT_TRUE(made.has_value());
    root_ = *made;
    const std::filesystem::path suite = root_ / "conformance";
    WriteText(suite / "cases/01_first/one/main.sv", kCaseSource);
    WriteText(suite / "cases/02_second/two/main.sv", kCaseSource);
    WriteText(suite / "selftest/harness/three/main.sv", kCaseSource);
    std::filesystem::create_directories(suite / "harness");
  }

  void TearDown() override {
    std::filesystem::remove_all(root_);
  }

  [[nodiscard]] auto Root() const -> const std::filesystem::path& {
    return root_;
  }

  [[nodiscard]] auto Site() const -> std::filesystem::path {
    return root_ / "site";
  }

  [[nodiscard]] auto Write(std::string_view name, const Json& record) const
      -> std::filesystem::path {
    const std::filesystem::path file = root_ / "records" / name;
    WriteText(file, record.dump(2));
    return file;
  }

  [[nodiscard]] auto Ask(std::vector<std::filesystem::path> records) const
      -> Request {
    return Request{
        .suite = root_ / "conformance",
        .revision = std::string(kRevision),
        .repository = "https://example.org/suite",
        .run = "https://example.org/runs/7",
        .records = std::move(records),
        .site = Site()};
  }

  // Asks for a publication that has to be refused, and checks it left nothing
  // behind.
  void ExpectRefused(const Request& request, std::string_view because) const {
    const auto published = Publish(request);
    ASSERT_FALSE(published.has_value());
    EXPECT_NE(published.error().find(because), std::string::npos)
        << published.error();
    EXPECT_FALSE(std::filesystem::exists(Site()));
  }

 private:
  std::filesystem::path root_;
};

TEST_F(Publishing, ANightIsPublishedOneConfigurationApartFromAnother) {
  Json llvm = ARecord("Lyra, LLVM backend", "lyra", {"--backend", "llvm"});
  llvm["cases"][1]["verdict"] = {
      {"kind", "does not conform"}, {"why", "the tool refused it"}};
  llvm["cases"][1]["end"] = {{"kind", "tool failed"}, {"how", "refused"}};
  const std::filesystem::path llvm_file = Write("llvm.json", llvm);
  const std::filesystem::path verilator_file =
      Write("verilator.json", Verilator());

  ASSERT_TRUE(Publish(Ask({verilator_file, llvm_file})).has_value());

  const Json summary = ReadJson(Site() / "summary.json");
  EXPECT_EQ(summary["suite"]["revision"], kRevision);
  EXPECT_EQ(summary["suite"]["repository"], "https://example.org/suite");
  EXPECT_EQ(summary["run"]["source"], "https://example.org/runs/7");
  EXPECT_EQ(summary["run"]["started"], "2026-10-30T05:03:46Z");
  ASSERT_EQ(summary["configurations"].size(), 2U);
  EXPECT_EQ(summary["configurations"][0]["name"], "Lyra, LLVM backend");
  EXPECT_EQ(summary["configurations"][0]["key"], "lyra-llvm-backend");
  EXPECT_EQ(
      summary["configurations"][0]["tool"]["options"],
      (Json{"--backend", "llvm"}));
  EXPECT_EQ(summary["configurations"][1]["name"], "Verilator");

  ASSERT_EQ(summary["cases"].size(), 3U);
  const Json& two = summary["cases"][1];
  EXPECT_EQ(two["id"], "02_second/two");
  EXPECT_EQ(two["path"], "conformance/cases/02_second/two");
  EXPECT_EQ(two["helper_check"], false);
  EXPECT_EQ(two["results"][0]["verdict"], "does not conform");
  EXPECT_EQ(two["results"][0]["end"], "tool failed");
  EXPECT_EQ(two["results"][0]["why"], "the tool refused it");
  EXPECT_TRUE(two["results"][1]["why"].is_null());
  EXPECT_EQ(two["results"][1]["verdict"], "conforms");
  const Json& three = summary["cases"][2];
  EXPECT_EQ(three["path"], "conformance/selftest/harness/three");
  EXPECT_EQ(three["helper_check"], true);

  // Each record is published as the run wrote it, and each case's evidence
  // from every configuration is kept beside the case.
  EXPECT_EQ(
      ReadText(Site() / "records/lyra-llvm-backend.json"), ReadText(llvm_file));
  const Json detail = ReadJson(Site() / "cases/02_second/two.json");
  ASSERT_EQ(detail["results"].size(), 2U);
  EXPECT_EQ(
      detail["results"]["lyra-llvm-backend"]["verdict"]["why"],
      "the tool refused it");
  EXPECT_EQ(detail["results"]["verilator"]["verdict"]["kind"], "conforms");

  // What the case requires is published with it, and so is every file it is
  // made of, the source it is entered through first.
  EXPECT_EQ(
      detail["requirement"],
      "The program must run and pass every check it makes.");
  ASSERT_EQ(detail["sources"].size(), 1U);
  EXPECT_EQ(detail["sources"][0]["name"], "main.sv");
  EXPECT_EQ(detail["sources"][0]["text"], kCaseSource);
}

TEST_F(Publishing, ACaseIsPublishedWithEveryFileItIsMadeOf) {
  WriteText(
      Root() / "conformance/cases/02_second/two/a_helper.sv", "package P;\n");
  WriteText(
      Root() / "conformance/cases/02_second/two/main.sv",
      "// @rejected:\n//\n// A program the standard\n// forbids (LRM "
      "1.2).\n//\n"
      "// A second paragraph.\nmodule Top; endmodule\n");
  ASSERT_TRUE(Publish(Ask({Write("verilator.json", Verilator())})).has_value());

  const Json detail = ReadJson(Site() / "cases/02_second/two.json");
  EXPECT_EQ(
      detail["requirement"], "A tool must refuse this program before it runs.");
  EXPECT_EQ(
      detail["statement"],
      (Json{
          "A program the standard forbids (LRM 1.2).", "A second paragraph."}));
  ASSERT_EQ(detail["sources"].size(), 2U);
  EXPECT_EQ(detail["sources"][0]["name"], "main.sv");
  EXPECT_EQ(detail["sources"][1]["name"], "a_helper.sv");
  EXPECT_EQ(detail["sources"][1]["text"], "package P;\n");
}

TEST_F(Publishing, ARecordOfAnotherRevisionOfTheSuiteIsRefused) {
  Json record = Verilator();
  record["suite"]["revision"] = "2222222222222222222222222222222222222222";
  ExpectRefused(Ask({Write("other.json", record)}), "revision");
}

TEST_F(Publishing, ARecordWithoutARevisionOfTheSuiteIsRefused) {
  Json record = Verilator();
  record["suite"]["revision"] = nullptr;
  ExpectRefused(Ask({Write("unknown.json", record)}), "revision");
}

TEST_F(Publishing, ARecordOfSomeCasesIsRefused) {
  Json record = Verilator();
  record["selection"] = Json::array({"01_first/one"});
  ExpectRefused(Ask({Write("some.json", record)}), "named cases");
}

TEST_F(Publishing, ARecordMissingACaseOfTheSuiteIsRefused) {
  Json record = Verilator();
  record["cases"].erase(1);
  ExpectRefused(Ask({Write("short.json", record)}), "02_second/two");
}

TEST_F(Publishing, ARecordOfACaseTheSuiteDoesNotHoldIsRefused) {
  Json record = Verilator();
  record["cases"][0]["id"] = "01_first/gone";
  ExpectRefused(Ask({Write("stale.json", record)}), "01_first/gone");
}

TEST_F(Publishing, ARecordJudgingACaseTwiceIsRefused) {
  Json record = Verilator();
  record["cases"].push_back(record["cases"][0]);
  ExpectRefused(Ask({Write("twice.json", record)}), "more than once");
}

TEST_F(Publishing, ARecordInAnotherFormatIsRefused) {
  Json record = Verilator();
  record["format_version"] = 2;
  ExpectRefused(Ask({Write("newer.json", record)}), "format");
}

TEST_F(Publishing, ARecordNamingNoConfigurationIsRefused) {
  Json record = Verilator();
  record["configuration"] = nullptr;
  ExpectRefused(Ask({Write("unnamed.json", record)}), "names no configuration");
}

TEST_F(Publishing, ARecordNamingAConfigurationWithNoAddressIsRefused) {
  Json record = Verilator();
  record["configuration"] = "+++";
  ExpectRefused(Ask({Write("odd.json", record)}), "no letter or digit");
}

TEST_F(Publishing, TwoRecordsOfOneConfigurationAreRefused) {
  ExpectRefused(
      Ask({Write("a.json", Verilator()), Write("b.json", Verilator())}),
      "both runs of 'Verilator'");
}

TEST_F(Publishing, TwoConfigurationsSharingAnAddressAreRefused) {
  ExpectRefused(
      Ask(
          {Write("c.json", ARecord("Lyra, C backend", "lyra", {})),
           Write("cpp.json", ARecord("Lyra, C++ backend", "lyra", {}))}),
      "whose addresses are both 'lyra-c-backend'");
}

TEST_F(Publishing, ARecordWithACaseNotRunIsRefused) {
  Json record = Verilator();
  record["cases"][1]["verdict"] = {
      {"kind", "not run"}, {"why", "no directory to run it in"}};
  ExpectRefused(
      Ask({Write("cut.json", record)}),
      "did not run '02_second/two': no directory to run it in");
}

TEST_F(Publishing, AFileThatIsNoRecordIsRefused) {
  const std::filesystem::path file = Write("torn.json", Verilator());
  WriteText(file, R"({"format_version": 1, "suite")");
  ExpectRefused(Ask({file}), "torn.json");
}

TEST_F(Publishing, ADocumentWithoutTheShapeOfARecordIsRefused) {
  ExpectRefused(Ask({Write("list.json", Json::array())}), "no format_version");
  Json no_tool = Verilator();
  no_tool.erase("tool");
  ExpectRefused(Ask({Write("no_tool.json", no_tool)}), "no tool");
  Json no_end = Verilator();
  no_end["cases"][2].erase("end");
  ExpectRefused(Ask({Write("no_end.json", no_end)}), "no cases[2].end.kind");
  Json bare_case = Verilator();
  bare_case["cases"][0] = "01_first/one";
  ExpectRefused(Ask({Write("bare.json", bare_case)}), "no cases[0].id");
}

}  // namespace
}  // namespace conformance::publish
