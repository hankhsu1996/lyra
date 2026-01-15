#pragma once

#include <ostream>

#include "lyra/mir/arena.hpp"
#include "lyra/mir/design.hpp"

namespace lyra::mir {

class Dumper {
 public:
  Dumper(const Arena* arena, std::ostream* out);

  void Dump(const Design& design);
  void Dump(const Module& module);
  void Dump(const Package& package);
  void Dump(ProcessId id);
  void Dump(FunctionId id);
  void Dump(BasicBlockId id);

 private:
  void PrintIndent();
  void Indent();
  void Dedent();

  const Arena* arena_;
  std::ostream* out_;
  int indent_ = 0;
};

}  // namespace lyra::mir
