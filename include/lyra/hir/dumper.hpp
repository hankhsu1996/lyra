#pragma once

#include <ostream>
#include <span>

#include "lyra/common/constant_arena.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/design.hpp"
#include "lyra/hir/module_body.hpp"
#include "lyra/hir/package.hpp"

namespace lyra::hir {

class Dumper {
 public:
  Dumper(
      const Arena* arena, const TypeArena* types,
      const ConstantArena* constants, const SymbolTable* symbols,
      std::ostream* out);

  void Dump(
      const Design& design, std::span<const ModuleBody> module_bodies,
      std::span<const Package> packages);
  void Dump(const Module& module);
  void Dump(const ModuleBody& body);
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
  // Module bodies to resolve hir::Module::body_id against while dumping.
  // Set by Dump(Design, ...) before visiting elements.
  std::span<const ModuleBody> current_module_bodies_;
};

}  // namespace lyra::hir
