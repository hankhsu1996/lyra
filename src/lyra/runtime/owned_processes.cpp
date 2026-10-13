#include "lyra/runtime/owned_processes.hpp"

#include <memory>
#include <utility>

#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/trigger.hpp"

namespace lyra::runtime {

OwnedProcesses::~OwnedProcesses() {
  while (OwnedPlace* place = ended_.PopFront()) {
    place->held->Release();
  }
  while (OwnedPlace* place = live_.PopFront()) {
    place->held->Release();
  }
}

void OwnedProcesses::Adopt(std::shared_ptr<RuntimeProcess> process) {
  RuntimeProcess& adopted = *process;
  adopted.owned_by_ = this;
  adopted.owned_place_.held = std::move(process);
  live_.PushBack(adopted.owned_place_);
}

void OwnedProcesses::Ended(RuntimeProcess& process, RuntimeEffects& effects) {
  process.owned_place_.Unlink();
  ended_.PushBack(process.owned_place_);
  effects.WakeParkedOn(all_ended_, Change::Whole());
}

}  // namespace lyra::runtime
