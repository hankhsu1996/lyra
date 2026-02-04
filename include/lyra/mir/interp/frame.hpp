#pragma once

#include <cstddef>
#include <vector>

#include "lyra/common/type.hpp"
#include "lyra/mir/interp/runtime_value.hpp"

namespace lyra::mir::interp {

// Frame holds runtime storage for a single activation (process or function).
// Storage is indexed by PlaceRoot (Local/Temp only - design storage is shared).
class Frame {
 public:
  // Construct with pre-initialized storage vectors.
  Frame(
      std::vector<RuntimeValue> locals, std::vector<RuntimeValue> temps,
      std::vector<TypeId> temp_types);

  [[nodiscard]] auto GetLocal(int id) -> RuntimeValue&;
  [[nodiscard]] auto GetLocal(int id) const -> const RuntimeValue&;
  [[nodiscard]] auto GetTemp(int id) -> RuntimeValue&;
  [[nodiscard]] auto GetTemp(int id) const -> const RuntimeValue&;
  [[nodiscard]] auto GetTempType(int id) const -> TypeId;

  [[nodiscard]] auto NumLocals() const -> size_t {
    return locals_.size();
  }
  [[nodiscard]] auto NumTemps() const -> size_t {
    return temps_.size();
  }

 private:
  std::vector<RuntimeValue> locals_;
  std::vector<RuntimeValue> temps_;
  std::vector<TypeId> temp_types_;
};

}  // namespace lyra::mir::interp
