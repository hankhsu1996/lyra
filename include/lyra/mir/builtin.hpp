#pragma once

namespace lyra::mir {

enum class BuiltinMethod {
  // Dynamic array operations
  kNewArray,     // new[size] or new[size](init) - returns array
  kArraySize,    // arr.size() - returns int
  kArrayDelete,  // arr.delete() - void, effectful
};

}  // namespace lyra::mir
