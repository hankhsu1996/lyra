#pragma once

#include "tests/framework/llvm_backend.hpp"  // For TestResult, ExtractedValue
#include "tests/framework/test_case.hpp"

namespace lyra::test {

// Run test using MIR interpreter backend
auto RunMirInterpreter(const TestCase& test_case) -> TestResult;

}  // namespace lyra::test
