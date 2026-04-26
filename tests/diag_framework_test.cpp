#include <cstdint>
#include <gtest/gtest.h>
#include <stdexcept>
#include <string>
#include <vector>
#include <yaml-cpp/yaml.h>

#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_manager.hpp"
#include "lyra/diag/source_span.hpp"
#include "tests/diag_framework/diag_matcher.hpp"
#include "tests/diag_framework/expected_diag.hpp"
#include "tests/diag_framework/yaml_parser.hpp"

namespace {

using lyra::diag::DiagCode;
using lyra::diag::DiagKind;
using lyra::diag::Diagnostic;
using lyra::diag::SourceManager;
using lyra::diag::SourceSpan;
using lyra::diag::UnsupportedCategory;
using lyra::test::ExpectedDiag;
using lyra::test::MatchAll;
using lyra::test::MatchContext;
using lyra::test::MatchOne;
using lyra::test::NormalizeCasePath;

auto MakeUnsupported(
    SourceSpan span, DiagCode code, std::string msg, UnsupportedCategory cat)
    -> Diagnostic {
  return Diagnostic::Unsupported(span, code, std::move(msg), cat);
}

TEST(DiagMatcher, CodeMustMatch) {
  SourceManager mgr;
  const MatchContext ctx{.mgr = &mgr, .case_dir = "/case"};
  const auto actual = MakeUnsupported(
      SourceSpan{}, DiagCode::kUnsupportedQueueType, "msg",
      UnsupportedCategory::kType);

  EXPECT_TRUE(
      MatchOne(actual, ExpectedDiag(DiagCode::kUnsupportedQueueType), ctx));
  EXPECT_FALSE(
      MatchOne(actual, ExpectedDiag(DiagCode::kUnsupportedEnumType), ctx));
}

TEST(DiagMatcher, KindOptional) {
  SourceManager mgr;
  const MatchContext ctx{.mgr = &mgr, .case_dir = "/case"};
  const auto actual = MakeUnsupported(
      SourceSpan{}, DiagCode::kUnsupportedQueueType, "msg",
      UnsupportedCategory::kType);

  EXPECT_TRUE(
      MatchOne(actual, ExpectedDiag(DiagCode::kUnsupportedQueueType), ctx));

  ExpectedDiag exp_match(DiagCode::kUnsupportedQueueType);
  exp_match.kind = DiagKind::kUnsupported;
  EXPECT_TRUE(MatchOne(actual, exp_match, ctx));

  ExpectedDiag exp_wrong(DiagCode::kUnsupportedQueueType);
  exp_wrong.kind = DiagKind::kError;
  EXPECT_FALSE(MatchOne(actual, exp_wrong, ctx));
}

TEST(DiagMatcher, CategoryMustMatchWhenSet) {
  SourceManager mgr;
  const MatchContext ctx{.mgr = &mgr, .case_dir = "/case"};
  const auto actual = MakeUnsupported(
      SourceSpan{}, DiagCode::kUnsupportedQueueType, "msg",
      UnsupportedCategory::kType);

  ExpectedDiag exp_match(DiagCode::kUnsupportedQueueType);
  exp_match.category = UnsupportedCategory::kType;
  EXPECT_TRUE(MatchOne(actual, exp_match, ctx));

  ExpectedDiag exp_wrong(DiagCode::kUnsupportedQueueType);
  exp_wrong.category = UnsupportedCategory::kFeature;
  EXPECT_FALSE(MatchOne(actual, exp_wrong, ctx));
}

TEST(DiagMatcher, FileLineRequireSourceSpan) {
  SourceManager mgr;
  const MatchContext ctx{.mgr = &mgr, .case_dir = "/case"};
  const auto unknown_span_actual = MakeUnsupported(
      SourceSpan{}, DiagCode::kUnsupportedQueueType, "msg",
      UnsupportedCategory::kType);

  ExpectedDiag exp_file(DiagCode::kUnsupportedQueueType);
  exp_file.file = "x.sv";
  EXPECT_FALSE(MatchOne(unknown_span_actual, exp_file, ctx));

  ExpectedDiag exp_line(DiagCode::kUnsupportedQueueType);
  exp_line.line = 1;
  EXPECT_FALSE(MatchOne(unknown_span_actual, exp_line, ctx));
}

TEST(DiagMatcher, FileAndLineMatch) {
  SourceManager mgr;
  const std::string content = "module M;\n  bit q [$];\nendmodule\n";
  const auto fid = mgr.AddFile("/case/main.sv", content);
  const auto begin = static_cast<std::uint32_t>(content.find("bit q [$]"));
  const SourceSpan span{.file_id = fid, .begin = begin, .end = begin + 9};
  const MatchContext ctx{.mgr = &mgr, .case_dir = "/case"};

  const auto actual = MakeUnsupported(
      span, DiagCode::kUnsupportedQueueType, "queue",
      UnsupportedCategory::kType);

  ExpectedDiag exp_match(DiagCode::kUnsupportedQueueType);
  exp_match.file = "main.sv";
  exp_match.line = 2;
  EXPECT_TRUE(MatchOne(actual, exp_match, ctx));

  ExpectedDiag exp_wrong_line(DiagCode::kUnsupportedQueueType);
  exp_wrong_line.line = 1;
  EXPECT_FALSE(MatchOne(actual, exp_wrong_line, ctx));

  ExpectedDiag exp_wrong_file(DiagCode::kUnsupportedQueueType);
  exp_wrong_file.file = "other.sv";
  EXPECT_FALSE(MatchOne(actual, exp_wrong_file, ctx));
}

TEST(DiagMatchAll, EmptyExpectedAlwaysPasses) {
  SourceManager mgr;
  const MatchContext ctx{.mgr = &mgr, .case_dir = "/case"};
  std::vector<Diagnostic> actuals;
  std::vector<ExpectedDiag> expecteds;
  EXPECT_FALSE(MatchAll(actuals, expecteds, ctx).has_value());

  actuals.push_back(MakeUnsupported(
      SourceSpan{}, DiagCode::kUnsupportedQueueType, "msg",
      UnsupportedCategory::kType));
  EXPECT_FALSE(MatchAll(actuals, expecteds, ctx).has_value());
}

TEST(DiagMatchAll, SubsetUnordered) {
  SourceManager mgr;
  const MatchContext ctx{.mgr = &mgr, .case_dir = "/case"};
  std::vector<Diagnostic> actuals;
  actuals.push_back(MakeUnsupported(
      SourceSpan{}, DiagCode::kUnsupportedEnumType, "enum",
      UnsupportedCategory::kType));
  actuals.push_back(MakeUnsupported(
      SourceSpan{}, DiagCode::kUnsupportedQueueType, "queue",
      UnsupportedCategory::kType));

  std::vector<ExpectedDiag> expecteds{
      ExpectedDiag(DiagCode::kUnsupportedQueueType),
  };
  EXPECT_FALSE(MatchAll(actuals, expecteds, ctx).has_value());
}

TEST(DiagMatchAll, BacktrackingFindsAssignment) {
  SourceManager mgr;
  const MatchContext ctx{.mgr = &mgr, .case_dir = "/case"};

  const std::string content = "a\nb\nc\n";
  const auto fid = mgr.AddFile("/case/x.sv", content);
  const SourceSpan span_line1{.file_id = fid, .begin = 0, .end = 1};
  const SourceSpan span_line2{.file_id = fid, .begin = 2, .end = 3};

  std::vector<Diagnostic> actuals;
  actuals.push_back(MakeUnsupported(
      span_line1, DiagCode::kUnsupportedQueueType, "q",
      UnsupportedCategory::kType));
  actuals.push_back(MakeUnsupported(
      span_line2, DiagCode::kUnsupportedQueueType, "q",
      UnsupportedCategory::kType));

  ExpectedDiag strong(DiagCode::kUnsupportedQueueType);
  strong.file = "x.sv";
  strong.line = 2;

  std::vector<ExpectedDiag> expecteds{
      ExpectedDiag(DiagCode::kUnsupportedQueueType),  // weak
      strong,
  };
  EXPECT_FALSE(MatchAll(actuals, expecteds, ctx).has_value());
}

TEST(DiagMatchAll, FailsWhenNoAssignmentExists) {
  SourceManager mgr;
  const MatchContext ctx{.mgr = &mgr, .case_dir = "/case"};
  std::vector<Diagnostic> actuals;
  actuals.push_back(MakeUnsupported(
      SourceSpan{}, DiagCode::kUnsupportedQueueType, "queue",
      UnsupportedCategory::kType));

  std::vector<ExpectedDiag> expecteds{
      ExpectedDiag(DiagCode::kUnsupportedQueueType),
      ExpectedDiag(DiagCode::kUnsupportedEnumType),
  };
  const auto result = MatchAll(actuals, expecteds, ctx);
  ASSERT_TRUE(result.has_value());
  EXPECT_NE(
      result->find("structured diagnostic match failed"), std::string::npos);
}

TEST(NormalizeCasePathFn, RelativeUnderCaseDir) {
  EXPECT_EQ(NormalizeCasePath("/case/sub/main.sv", "/case"), "sub/main.sv");
  EXPECT_EQ(NormalizeCasePath("/case/main.sv", "/case"), "main.sv");
}

TEST(NormalizeCasePathFn, OutsideCaseDirFallsBack) {
  const auto out = NormalizeCasePath("/elsewhere/main.sv", "/case");
  EXPECT_FALSE(out.starts_with(".."));
}

TEST(YamlParserFn, ValidMinimal) {
  YAML::Node n = YAML::Load("code: unsupported_queue_type");
  const auto e = lyra::test::ParseExpectedDiag(n);
  EXPECT_EQ(e.code, DiagCode::kUnsupportedQueueType);
  EXPECT_FALSE(e.kind.has_value());
  EXPECT_FALSE(e.category.has_value());
  EXPECT_FALSE(e.file.has_value());
  EXPECT_FALSE(e.line.has_value());
}

TEST(YamlParserFn, ValidFull) {
  const auto* yaml = R"(
code: unsupported_queue_type
kind: unsupported
category: type
file: main.sv
line: 7
)";
  YAML::Node n = YAML::Load(yaml);
  const auto e = lyra::test::ParseExpectedDiag(n);
  EXPECT_EQ(e.code, DiagCode::kUnsupportedQueueType);
  ASSERT_TRUE(e.kind.has_value());
  EXPECT_EQ(*e.kind, DiagKind::kUnsupported);
  ASSERT_TRUE(e.category.has_value());
  EXPECT_EQ(*e.category, UnsupportedCategory::kType);
  EXPECT_EQ(e.file, "main.sv");
  EXPECT_EQ(e.line, 7);
}

TEST(YamlParserFn, MissingCodeThrows) {
  YAML::Node n = YAML::Load("kind: error");
  EXPECT_THROW(lyra::test::ParseExpectedDiag(n), std::runtime_error);
}

TEST(YamlParserFn, BadKindThrows) {
  YAML::Node n = YAML::Load("code: unsupported_queue_type\nkind: not_a_kind");
  EXPECT_THROW(lyra::test::ParseExpectedDiag(n), std::runtime_error);
}

TEST(YamlParserFn, NoteKindRejected) {
  YAML::Node n = YAML::Load("code: unsupported_queue_type\nkind: note");
  EXPECT_THROW(lyra::test::ParseExpectedDiag(n), std::runtime_error);
}

TEST(YamlParserFn, CategoryWithExplicitNonUnsupportedKindThrows) {
  YAML::Node n =
      YAML::Load("code: unsupported_queue_type\nkind: error\ncategory: type");
  EXPECT_THROW(lyra::test::ParseExpectedDiag(n), std::runtime_error);
}

TEST(YamlParserFn, CategoryWithoutKindIsAllowed) {
  YAML::Node n = YAML::Load("code: unsupported_queue_type\ncategory: type");
  const auto e = lyra::test::ParseExpectedDiag(n);
  EXPECT_FALSE(e.kind.has_value());
  ASSERT_TRUE(e.category.has_value());
  EXPECT_EQ(*e.category, UnsupportedCategory::kType);
}

TEST(YamlParserFn, BadCategoryThrows) {
  YAML::Node n = YAML::Load("code: unsupported_queue_type\ncategory: weird");
  EXPECT_THROW(lyra::test::ParseExpectedDiag(n), std::runtime_error);
}

TEST(YamlParserFn, ZeroLineThrows) {
  YAML::Node n = YAML::Load("code: unsupported_queue_type\nline: 0");
  EXPECT_THROW(lyra::test::ParseExpectedDiag(n), std::runtime_error);
}

TEST(YamlParserFn, NegativeLineThrows) {
  YAML::Node n = YAML::Load("code: unsupported_queue_type\nline: -3");
  EXPECT_THROW(lyra::test::ParseExpectedDiag(n), std::runtime_error);
}

TEST(YamlParserFn, BadCodeThrows) {
  YAML::Node n = YAML::Load("code: nonexistent_code");
  EXPECT_THROW(lyra::test::ParseExpectedDiag(n), std::runtime_error);
}

TEST(YamlParserFn, NonMapEntryThrows) {
  YAML::Node n = YAML::Load("just a string");
  EXPECT_THROW(lyra::test::ParseExpectedDiag(n), std::runtime_error);
}

}  // namespace
