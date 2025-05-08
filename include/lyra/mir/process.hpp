#pragma once

#include <memory>

#include "lyra/mir/statement.hpp"

namespace lyra::mir {

enum class ProcessKind { kInitial, kAlways };

class Process {
 public:
  std::string name;
  ProcessKind process_kind;
  std::unique_ptr<Statement> body;
};

}  // namespace lyra::mir
