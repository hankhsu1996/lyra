#pragma once

#include <cstddef>
#include <format>
#include <utility>
#include <vector>

#include "lyra/base/id_range.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/registry.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/function_id.hpp"

namespace lyra::lir {

// What a function is said to be where it is declared, before its body exists,
// so a call lowered first already knows it: whether a call of it can be
// unwound out of, as LLVM's `nounwind` is an attribute of a function's
// declaration and rustc's `FnAbi::can_unwind` is a fact of a callee apart
// from its body.
struct FunctionAttributes {
  bool can_unwind = true;
};

// The functions of a unit, each declared with its attributes and defined later
// by its body, as an LLVM module holds a function with its attributes whether
// or not it has a body yet.
class FunctionRegistry {
 public:
  auto Declare(FunctionAttributes attributes) -> FunctionId {
    attributes_.push_back(attributes);
    return functions_.Declare();
  }

  void Define(FunctionId id, Function function) {
    functions_.Define(id, std::move(function));
  }

  // Declares a function and defines it in one step, for one nothing names
  // before its body exists.
  auto Add(FunctionAttributes attributes, Function function) -> FunctionId {
    const FunctionId id = Declare(attributes);
    Define(id, std::move(function));
    return id;
  }

  [[nodiscard]] auto Attributes(FunctionId id) const -> FunctionAttributes {
    if (id.value >= attributes_.size()) {
      throw InternalError(
          std::format(
              "lir: function {} is out of range; the unit declares {} -- "
              "please report this as a bug",
              id.value, attributes_.size()));
    }
    return attributes_[id.value];
  }

  [[nodiscard]] auto Get(FunctionId id) const -> const Function& {
    return functions_.Get(id);
  }

  [[nodiscard]] auto IsDefined(FunctionId id) const -> bool {
    return functions_.IsDefined(id);
  }

  [[nodiscard]] auto size() const -> std::size_t {
    return functions_.size();
  }

  [[nodiscard]] auto Ids() const -> base::IdRange<FunctionId> {
    return functions_.Ids();
  }

  [[nodiscard]] auto begin() const
      -> base::Registry<Function, FunctionId>::Iterator {
    return functions_.begin();
  }

  [[nodiscard]] auto end() const
      -> base::Registry<Function, FunctionId>::Iterator {
    return functions_.end();
  }

 private:
  base::Registry<Function, FunctionId> functions_;
  std::vector<FunctionAttributes> attributes_;
};

}  // namespace lyra::lir
