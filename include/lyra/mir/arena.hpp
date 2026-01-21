#pragma once

#include <vector>

#include "lyra/mir/handle.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/routine.hpp"

namespace lyra::mir {

class Arena final {
 public:
  Arena() = default;
  ~Arena() = default;

  Arena(const Arena&) = delete;
  auto operator=(const Arena&) -> Arena& = delete;

  Arena(Arena&&) = default;
  auto operator=(Arena&&) -> Arena& = default;

  auto AddPlace(Place place) -> PlaceId {
    PlaceId id{static_cast<uint32_t>(places_.size())};
    places_.push_back(std::move(place));
    return id;
  }

  // Creates a derived place by appending a projection to an existing place.
  // Pure structural operation - does not compute or store types.
  auto DerivePlace(PlaceId base, Projection proj) -> PlaceId {
    const Place& base_place = places_[base.value];
    Place new_place{
        .root = base_place.root,
        .projections = base_place.projections,
    };
    new_place.projections.push_back(std::move(proj));
    return AddPlace(std::move(new_place));
  }

  auto AddProcess(Process proc) -> ProcessId {
    ProcessId id{static_cast<uint32_t>(processes_.size())};
    processes_.push_back(std::move(proc));
    return id;
  }

  auto AddFunction(Function func) -> FunctionId {
    FunctionId id{static_cast<uint32_t>(functions_.size())};
    functions_.push_back(std::move(func));
    return id;
  }

  // Reserve a FunctionId for later filling (pre-allocation for recursion)
  auto ReserveFunction() -> FunctionId {
    FunctionId id{static_cast<uint32_t>(functions_.size())};
    functions_.emplace_back();  // Placeholder
    return id;
  }

  // Fill in a previously reserved function
  void SetFunction(FunctionId id, Function func) {
    functions_[id.value] = std::move(func);
  }

  [[nodiscard]] auto operator[](PlaceId id) const -> const Place& {
    return places_[id.value];
  }

  [[nodiscard]] auto operator[](ProcessId id) const -> const Process& {
    return processes_[id.value];
  }

  [[nodiscard]] auto operator[](FunctionId id) const -> const Function& {
    return functions_[id.value];
  }

  [[nodiscard]] auto PlaceCount() const -> size_t {
    return places_.size();
  }

 private:
  std::vector<Place> places_;
  std::vector<Process> processes_;
  std::vector<Function> functions_;
};

}  // namespace lyra::mir
