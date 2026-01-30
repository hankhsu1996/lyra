#pragma once

#include <ostream>
#include <string>

#include "lyra/common/type_arena.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/design.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/rhs.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::mir {

class Dumper {
 public:
  Dumper(const Arena* arena, const TypeArena* type_arena, std::ostream* out);

  void Dump(const Design& design);
  void Dump(const Module& module);
  void Dump(const Package& package);
  void Dump(ProcessId id);
  void Dump(FunctionId id);
  void DumpBlock(const BasicBlock& bb, uint32_t index);

 private:
  void PrintIndent();
  void Indent();
  void Dedent();

  [[nodiscard]] auto FormatProjection(const Projection& proj) const
      -> std::string;
  [[nodiscard]] auto FormatIndexOperand(const Operand& op) const -> std::string;
  [[nodiscard]] auto FormatPlace(PlaceId id) const -> std::string;
  [[nodiscard]] auto FormatOperand(const Operand& op) const -> std::string;
  [[nodiscard]] auto FormatRvalue(const Rvalue& rv) const -> std::string;
  [[nodiscard]] auto FormatRhs(const RightHandSide& rhs) const -> std::string;
  [[nodiscard]] auto FormatEffect(const EffectOp& op) const -> std::string;
  [[nodiscard]] auto FormatType(TypeId id) const -> std::string;

  const Arena* arena_;
  const TypeArena* type_arena_;
  std::ostream* out_;
  int indent_ = 0;
};

}  // namespace lyra::mir
