#pragma once

#include "lyra/mir/module_unit.hpp"
#include "lyra/mir/process.hpp"

namespace lyra::projection::cpp {

class RenderContext {
 public:
  RenderContext(const mir::ModuleUnit& unit, const mir::Process& process)
      : unit_(&unit), process_(&process) {
  }

  [[nodiscard]] auto Unit() const -> const mir::ModuleUnit& {
    return *unit_;
  }

  [[nodiscard]] auto Process() const -> const mir::Process& {
    return *process_;
  }

 private:
  const mir::ModuleUnit* unit_;
  const mir::Process* process_;
};

}  // namespace lyra::projection::cpp
