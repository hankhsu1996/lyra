#pragma once

#include <cstddef>
#include <vector>

#include "lyra/mir/interp/runtime_value.hpp"

namespace lyra::mir::interp {

// Frame holds runtime storage for a single activation (process or function).
// Storage is indexed by PlaceRoot (Local/Temp only - design storage is shared).
class Frame {
 public:
  Frame(size_t num_locals, size_t num_temps);

  [[nodiscard]] auto GetLocal(int id) -> RuntimeValue&;
  [[nodiscard]] auto GetLocal(int id) const -> const RuntimeValue&;
  [[nodiscard]] auto GetTemp(int id) -> RuntimeValue&;
  [[nodiscard]] auto GetTemp(int id) const -> const RuntimeValue&;

  [[nodiscard]] auto NumLocals() const -> size_t {
    return locals_.size();
  }
  [[nodiscard]] auto NumTemps() const -> size_t {
    return temps_.size();
  }

 private:
  std::vector<RuntimeValue> locals_;
  std::vector<RuntimeValue> temps_;
};

}  // namespace lyra::mir::interp
