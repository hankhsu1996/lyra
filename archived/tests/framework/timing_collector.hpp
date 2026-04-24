#pragma once

#include <string>
#include <utility>
#include <vector>

#include "tests/framework/test_result.hpp"

namespace lyra::test {

class TimingCollector {
 public:
  void Record(const std::string& name, const TestTimings& t);
  void PrintSummary() const;

 private:
  std::vector<std::pair<std::string, TestTimings>> entries_;
};

auto GetTimingCollector() -> TimingCollector&;
auto IsTimingEnabled() -> bool;
void SetTimingEnabled(bool enabled);

}  // namespace lyra::test
