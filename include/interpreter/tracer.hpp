#pragma once

#include <optional>
#include <ostream>
#include <string>
#include <vector>

#include <fmt/format.h>

namespace lyra::interpreter {

using SimulationTime = uint64_t;

struct TraceEvent {
  std::string name;
  std::optional<SimulationTime> time{};
  std::string detail;

  [[nodiscard]] auto ToString() const -> std::string {
    std::string time_str = time.has_value() ? std::to_string(*time) : "-";
    return fmt::format("[{}] {} | {}", time_str, name, detail);
  }
};

class Tracer {
 public:
  Tracer() = default;

  void AddEvent(TraceEvent event) {
    events_.emplace_back(std::move(event));
  }

  void Clear() {
    events_.clear();
  }

  void Dump(std::ostream& os) const {
    for (const auto& event : events_) {
      os << event.ToString() << '\n';
    }
  }

 private:
  std::vector<TraceEvent> events_;
};

}  // namespace lyra::interpreter
