#include "lyra/mir/dumper.hpp"

#include <cassert>
#include <format>
#include <variant>

#include "lyra/common/operator.hpp"

namespace lyra::mir {

namespace {

auto FormatTerminator(const Terminator& term) -> std::string {
  switch (term.kind) {
    case Terminator::Kind::kJump:
      if (!term.targets.empty()) {
        return std::format("jump bb{}", term.targets[0].value);
      }
      return "jump <?>)";
    case Terminator::Kind::kBranch:
      if (term.targets.size() >= 2) {
        return std::format(
            "branch bb{}, bb{}", term.targets[0].value, term.targets[1].value);
      }
      return "branch <?>";
    case Terminator::Kind::kSwitch:
      return "switch";
    case Terminator::Kind::kDelay:
      return "delay";
    case Terminator::Kind::kWait:
      return "wait";
    case Terminator::Kind::kReturn:
      return "return";
    case Terminator::Kind::kFinish:
      return "finish";
    case Terminator::Kind::kRepeat:
      return "repeat";
  }
  return "<?>";
}

}  // namespace

Dumper::Dumper(
    const Arena* arena, const TypeArena* type_arena, std::ostream* out)
    : arena_(arena), type_arena_(type_arena), out_(out) {
}

void Dumper::PrintIndent() {
  for (int i = 0; i < indent_; ++i) {
    *out_ << "  ";
  }
}

void Dumper::Indent() {
  ++indent_;
}

void Dumper::Dedent() {
  assert(indent_ > 0);
  --indent_;
}

void Dumper::Dump(const Design& design) {
  *out_ << "Design {\n";
  Indent();
  for (const auto& element : design.elements) {
    std::visit([this](const auto& elem) { Dump(elem); }, element);
  }
  Dedent();
  *out_ << "}\n";
}

void Dumper::Dump(const Module& module) {
  PrintIndent();
  *out_ << "Module {\n";
  Indent();

  for (ProcessId id : module.processes) {
    Dump(id);
  }
  for (FunctionId id : module.functions) {
    Dump(id);
  }

  Dedent();
  PrintIndent();
  *out_ << "}\n";
}

void Dumper::Dump(const Package& /*package*/) {
  PrintIndent();
  *out_ << "Package {}\n";
}

void Dumper::Dump(ProcessId id) {
  const Process& proc = (*arena_)[id];
  PrintIndent();

  const char* kind_str =
      proc.kind == ProcessKind::kOnce ? "process.once" : "process.loop";
  *out_ << kind_str << " {\n";
  Indent();

  for (BasicBlockId bb_id : proc.blocks) {
    Dump(bb_id);
  }

  Dedent();
  PrintIndent();
  *out_ << "}\n";
}

void Dumper::Dump(FunctionId id) {
  const Function& func = (*arena_)[id];
  PrintIndent();
  *out_ << "function {\n";
  Indent();

  for (BasicBlockId bb_id : func.blocks) {
    Dump(bb_id);
  }

  Dedent();
  PrintIndent();
  *out_ << "}\n";
}

void Dumper::Dump(BasicBlockId id) {
  const BasicBlock& bb = (*arena_)[id];
  PrintIndent();
  *out_ << std::format("bb{}: {{\n", bb.id.value);
  Indent();

  for (const Instruction& instr : bb.instructions) {
    PrintIndent();
    std::visit(
        [this](const auto& i) {
          using T = std::decay_t<decltype(i)>;
          if constexpr (std::is_same_v<T, Assign>) {
            *out_ << std::format(
                "{} = {}\n", FormatPlace(i.target), FormatOperand(i.source));
          } else if constexpr (std::is_same_v<T, Compute>) {
            *out_ << std::format(
                "{} = {}\n", FormatPlace(i.target), FormatRvalue(i.value));
          }
        },
        instr);
  }

  PrintIndent();
  *out_ << FormatTerminator(bb.terminator) << "\n";

  Dedent();
  PrintIndent();
  *out_ << "}\n";
}

auto Dumper::FormatPlace(PlaceId id) const -> std::string {
  const Place& place = (*arena_)[id];
  const char* prefix = nullptr;
  switch (place.root.kind) {
    case PlaceRoot::Kind::kLocal:
      prefix = "_";
      break;
    case PlaceRoot::Kind::kTemp:
      prefix = "%";
      break;
    case PlaceRoot::Kind::kDesign:
      prefix = "@";
      break;
  }
  std::string result = std::format("{}{}", prefix, place.root.id);

  for (const Projection& proj : place.projections) {
    switch (proj.kind) {
      case Projection::Kind::kField:
        result += std::format(".{}", proj.operand);
        break;
      case Projection::Kind::kIndex:
        result += std::format("[{}]", proj.operand);
        break;
      case Projection::Kind::kSlice:
        result += std::format("[slice:{}]", proj.operand);
        break;
      case Projection::Kind::kDeref:
        result += ".*";
        break;
    }
  }

  result += std::format(": {}", FormatType(place.root.type));
  return result;
}

auto Dumper::FormatOperand(const Operand& op) const -> std::string {
  switch (op.kind) {
    case Operand::Kind::kConst: {
      const auto& constant = std::get<Constant>(op.payload);
      return std::visit(
          [this, &constant](const auto& val) -> std::string {
            using T = std::decay_t<decltype(val)>;
            if constexpr (std::is_same_v<T, IntegralConstant>) {
              if (val.value.empty()) {
                return std::format("const(0: {})", FormatType(constant.type));
              }
              return std::format(
                  "const({}: {})", val.value[0], FormatType(constant.type));
            } else if constexpr (std::is_same_v<T, StringConstant>) {
              return std::format(
                  "const(\"{}\": {})", val.value, FormatType(constant.type));
            } else {
              return std::format(
                  "const(<aggregate>: {})", FormatType(constant.type));
            }
          },
          constant.value);
    }
    case Operand::Kind::kUse: {
      PlaceId place_id = std::get<PlaceId>(op.payload);
      const Place& place = (*arena_)[place_id];
      const char* prefix = nullptr;
      switch (place.root.kind) {
        case PlaceRoot::Kind::kLocal:
          prefix = "_";
          break;
        case PlaceRoot::Kind::kTemp:
          prefix = "%";
          break;
        case PlaceRoot::Kind::kDesign:
          prefix = "@";
          break;
      }
      return std::format("use({}{})", prefix, place.root.id);
    }
    case Operand::Kind::kPoison:
      return "poison";
  }
  return "<?>";
}

auto Dumper::FormatRvalue(const Rvalue& rv) const -> std::string {
  std::string result;

  switch (rv.kind) {
    case RvalueKind::kUnary:
      result = std::format("unary({})", ToString(static_cast<UnaryOp>(rv.op)));
      break;
    case RvalueKind::kBinary:
      result =
          std::format("binary({})", ToString(static_cast<BinaryOp>(rv.op)));
      break;
    case RvalueKind::kCast:
      result = "cast";
      break;
    case RvalueKind::kCall:
      result = std::format("call(op={})", rv.op);
      break;
  }

  for (const Operand& operand : rv.operands) {
    result += ", ";
    result += FormatOperand(operand);
  }

  return result;
}

auto Dumper::FormatType(TypeId id) const -> std::string {
  if (type_arena_ == nullptr) {
    return std::format("type[{}]", id.value);
  }
  return ToString((*type_arena_)[id]);
}

}  // namespace lyra::mir
