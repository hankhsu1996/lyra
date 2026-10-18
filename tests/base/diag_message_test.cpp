#include "lyra/base/diag_message.hpp"

#include <gtest/gtest.h>
#include <optional>
#include <string>

namespace {

using lyra::base::BrokenMessageRule;
using lyra::base::DiagMessage;
using lyra::base::MessageRule;

// The checks run when Lyra is built, so these say what the build accepts and
// refuses, one rule at a time.
static_assert(!BrokenMessageRule("'randcase' is not yet supported"));
static_assert(
    !BrokenMessageRule("DPI-C link input '{}' is not a readable file"));
static_assert(!BrokenMessageRule(
    "the size given to a dynamic array's new[] is "
    "negative"));
static_assert(!BrokenMessageRule("no input files; looked in '{}' ({})"));
static_assert(!BrokenMessageRule("{}"));
static_assert(!BrokenMessageRule("'a. b' names no file"));
static_assert(!BrokenMessageRule("the lower bound is above the upper one"));

static_assert(BrokenMessageRule("") == MessageRule::kSaysSomething);
static_assert(
    BrokenMessageRule("Not a count") == MessageRule::kStartsLowercase);
static_assert(
    BrokenMessageRule("not a count.") == MessageRule::kEndsWithoutPunctuation);
static_assert(
    BrokenMessageRule("not a count;") == MessageRule::kEndsWithoutPunctuation);
static_assert(
    BrokenMessageRule("linking failed: {}") ==
    MessageRule::kHoldsNoColonBeforeASpace);
static_assert(
    BrokenMessageRule("the run ended. It ran out") ==
    MessageRule::kIsOneSentence);
static_assert(
    BrokenMessageRule("compiling failed\n{}") == MessageRule::kIsOneLine);
static_assert(
    BrokenMessageRule("`randcase` is not yet supported") ==
    MessageRule::kQuotesANameInSingleQuotes);
static_assert(
    BrokenMessageRule("\"x\" is unknown") ==
    MessageRule::kQuotesANameInSingleQuotes);
static_assert(
    BrokenMessageRule("the value isn't known") ==
    MessageRule::kHoldsNoContraction);
static_assert(
    BrokenMessageRule("'randcase' is not yet supported (LRM 18.16)") ==
    MessageRule::kCitesNoClauseOfTheStandard);
static_assert(
    BrokenMessageRule("a closure is not yet supported here") ==
    MessageRule::kUsesNoWordForTheCompilersInsides);
static_assert(
    BrokenMessageRule("the front end gave it no meaning") ==
    MessageRule::kUsesNoWordForTheCompilersInsides);
static_assert(
    BrokenMessageRule("'randcase' is unsupported") ==
    MessageRule::kSaysAGapIsNotYetSupported);
static_assert(
    BrokenMessageRule("'randcase' is not supported") ==
    MessageRule::kSaysAGapIsNotYetSupported);

// A word inside a quoted name is the user's, not the message's.
static_assert(!BrokenMessageRule("no module named 'closure'"));

TEST(DiagMessage, IsTheLiteralWithItsArgumentsFormattedIn) {
  const DiagMessage message("'{}' is not a count", std::string{"x"});
  EXPECT_EQ(message.Text(), "'x' is not a count");
}

}  // namespace
