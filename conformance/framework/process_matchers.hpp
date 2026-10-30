#pragma once

#include <gmock/gmock.h>
#include <ostream>

#include "conformance/framework/process.hpp"

namespace conformance::framework {

// What an assertion shows of an outcome it rejected: how it ended. The streams
// are long, so a caller adds whichever one says why.
void PrintTo(const ProcessOutcome& outcome, std::ostream* out);

// The child exited, with a code `code` accepts; a child that did not exit
// matches no code.
auto ExitedWithCode(const testing::Matcher<int>& code)
    -> testing::Matcher<const ProcessOutcome&>;

}  // namespace conformance::framework
