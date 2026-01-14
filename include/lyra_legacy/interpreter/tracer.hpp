#pragma once

#include <iostream>
#include <ostream>
#include <string>
#include <vector>

#include <fmt/core.h>

namespace lyra::interpreter {

using SimulationTime = uint64_t;

struct TraceRecord {
  SimulationTime time;
  std::string message;

  [[nodiscard]] auto ToString() const -> std::string {
    return fmt::format("[{}] {}", time, message);
  }
};

class Tracer {
 public:
  explicit Tracer(SimulationTime& current_time) : current_time_(current_time) {
  }

  void Record(std::string message) {
    TraceRecord record{.time = current_time_, .message = std::move(message)};
    events_.emplace_back(std::move(record));
  }

  void Clear() {
    events_.clear();
  }

  void Dump(std::ostream& os) const {
    for (const auto& record : events_) {
      os << record.ToString() << '\n';
    }
  }

 private:
  std::reference_wrapper<SimulationTime> current_time_;
  std::vector<TraceRecord> events_;
};

}  // namespace lyra::interpreter
