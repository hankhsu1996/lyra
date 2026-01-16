#pragma once

#include <cstddef>
#include <vector>

#include "lyra/mir/interp/runtime_value.hpp"
#include "lyra/mir/place.hpp"

namespace lyra::mir::interp {

// Frame holds runtime storage for a single activation (process or function).
// Storage is indexed by PlaceRoot (Local/Temp).
class Frame {
 public:
  // Construct frame with given number of locals and temps
  Frame(size_t num_locals, size_t num_temps);

  // Access storage by root kind and id
  [[nodiscard]] auto GetLocal(int id) -> RuntimeValue&;
  [[nodiscard]] auto GetLocal(int id) const -> const RuntimeValue&;
  [[nodiscard]] auto GetTemp(int id) -> RuntimeValue&;
  [[nodiscard]] auto GetTemp(int id) const -> const RuntimeValue&;

  // Resolve a PlaceRoot to its storage
  [[nodiscard]] auto Resolve(const PlaceRoot& root) -> RuntimeValue&;
  [[nodiscard]] auto Resolve(const PlaceRoot& root) const
      -> const RuntimeValue&;

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
