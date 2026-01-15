#pragma once

#include <ostream>

#include "lyra/common/constant_arena.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/design.hpp"

namespace lyra::hir {

class Dumper {
 public:
  Dumper(
      const Arena* arena, const TypeArena* types,
      const ConstantArena* constants, const SymbolTable* symbols,
      std::ostream* out);

  void Dump(const Design& design);
  void Dump(const Module& module);
  void Dump(const Package& package);
  void Dump(ProcessId id);
  void Dump(FunctionId id);
  void Dump(TaskId id);
  void Dump(StatementId id);
  void Dump(ExpressionId id);

 private:
  void PrintIndent();
  void Indent();
  void Dedent();

  [[nodiscard]] auto TypeString(TypeId id) const -> std::string;
  [[nodiscard]] auto SymbolName(SymbolId id) const -> std::string;
  [[nodiscard]] auto ConstantString(ConstId id) const -> std::string;

  const Arena* arena_;
  const TypeArena* types_;
  const ConstantArena* constants_;
  const SymbolTable* symbols_;
  std::ostream* out_;
  int indent_ = 0;
};

}  // namespace lyra::hir
