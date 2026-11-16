#pragma once

#include <vector>

#include "lyra/hir/type_id.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/support/data_object.hpp"

namespace lyra::hir {

// One data object a scope declares that a reader reaching the design by name
// finds (LRM 21.7.1.2): a variable, a net or a named event, a static variable a
// block, a task or a function of the scope writes among them (LRM 6.21). It is
// its cell, as the scope's own code reaches it, which names the declaration --
// and so the block it is written in -- and is what is read for its value and
// watched for a change of it; the type a value of it has; and each port of the
// scope connecting it (LRM 21.7.4.2).
struct DescribedDataObject {
  ValueTarget cell;
  TypeId type;
  std::vector<support::DescribedPort> ports;

  auto operator==(const DescribedDataObject&) const -> bool = default;
};

}  // namespace lyra::hir
