#include "lyra/mir/dumper.hpp"

#include <cassert>
#include <format>
#include <variant>

#include "lyra/common/system_function.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/operator.hpp"
#include "lyra/mir/place_type.hpp"

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
      if (term.termination_info) {
        const auto& info = *term.termination_info;
        const char* kind_str = nullptr;
        switch (info.kind) {
          case TerminationKind::kFinish:
            kind_str = "finish";
            break;
          case TerminationKind::kStop:
            kind_str = "stop";
            break;
          case TerminationKind::kFatal:
            kind_str = "fatal";
            break;
          case TerminationKind::kExit:
            kind_str = "exit";
            break;
        }
        return std::format("terminate({}, {})", kind_str, info.level);
      }
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
          } else if constexpr (std::is_same_v<T, Effect>) {
            *out_ << FormatEffect(i.op) << "\n";
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

auto Dumper::FormatProjectionOperand(
    const std::variant<int, Operand>& operand) const -> std::string {
  if (const auto* constant_index = std::get_if<int>(&operand)) {
    return std::format("{}", *constant_index);
  }
  const auto& op = std::get<Operand>(operand);
  switch (op.kind) {
    case Operand::Kind::kConst: {
      const auto& constant = std::get<Constant>(op.payload);
      if (const auto* integral =
              std::get_if<IntegralConstant>(&constant.value)) {
        auto value = integral->value.empty() ? 0UL : integral->value[0];
        return std::format("idx=const:{}", value);
      }
      return "idx=const:?";
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
      return std::format("idx=use:{}{}", prefix, place.root.id);
    }
    case Operand::Kind::kPoison:
      return "idx=poison";
  }
  return "idx=?";
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
        result += std::format(".{}", FormatProjectionOperand(proj.operand));
        break;
      case Projection::Kind::kIndex:
        result += std::format("[{}]", FormatProjectionOperand(proj.operand));
        break;
      case Projection::Kind::kSlice:
        result +=
            std::format("[slice:{}]", FormatProjectionOperand(proj.operand));
        break;
      case Projection::Kind::kDeref:
        result += ".*";
        break;
    }
  }

  TypeId place_type =
      type_arena_ != nullptr ? TypeOfPlace(*type_arena_, place) : TypeId{0};
  result += std::format(": {}", FormatType(place_type));
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
      std::string result = std::format("{}{}", prefix, place.root.id);
      for (const Projection& proj : place.projections) {
        switch (proj.kind) {
          case Projection::Kind::kField:
            result += std::format(".{}", FormatProjectionOperand(proj.operand));
            break;
          case Projection::Kind::kIndex:
            result +=
                std::format("[{}]", FormatProjectionOperand(proj.operand));
            break;
          case Projection::Kind::kSlice:
            result += std::format(
                "[slice:{}]", FormatProjectionOperand(proj.operand));
            break;
          case Projection::Kind::kDeref:
            result += ".*";
            break;
        }
      }
      return std::format("use({})", result);
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
    case RvalueKind::kCast: {
      const auto* cast_info = std::get_if<CastInfo>(&rv.info);
      if (cast_info != nullptr) {
        result = std::format(
            "cast({} -> {})", FormatType(cast_info->source_type),
            FormatType(cast_info->target_type));
      } else {
        result = "cast";
      }
      break;
    }
    case RvalueKind::kCall:
      if (const auto* user = std::get_if<UserCallInfo>(&rv.info)) {
        result = std::format("call(func[{}])", user->callee.value);
      } else if (const auto* sys = std::get_if<SystemCallInfo>(&rv.info)) {
        result = std::format("syscall({})", sys->opcode);
      } else {
        result = std::format("call(op={})", rv.op);
      }
      break;
    case RvalueKind::kAggregate: {
      const auto* info = std::get_if<AggregateInfo>(&rv.info);
      std::string type_str =
          info != nullptr ? FormatType(info->result_type) : "?";
      result = "aggregate<" + type_str + ">(";
      bool first = true;
      for (const auto& op : rv.operands) {
        if (!first) {
          result += ", ";
        }
        first = false;
        result += FormatOperand(op);
      }
      result += ")";
      return result;
    }
  }

  for (const Operand& operand : rv.operands) {
    result += ", ";
    result += FormatOperand(operand);
  }

  return result;
}

auto Dumper::FormatEffect(const EffectOp& op) const -> std::string {
  return std::visit(
      [this](const auto& effect_op) -> std::string {
        using T = std::decay_t<decltype(effect_op)>;
        if constexpr (std::is_same_v<T, DisplayEffect>) {
          std::string result;
          switch (effect_op.radix) {
            case PrintRadix::kDecimal:
              result = effect_op.append_newline ? "$display" : "$write";
              break;
            case PrintRadix::kBinary:
              result = effect_op.append_newline ? "$displayb" : "$writeb";
              break;
            case PrintRadix::kOctal:
              result = effect_op.append_newline ? "$displayo" : "$writeo";
              break;
            case PrintRadix::kHex:
              result = effect_op.append_newline ? "$displayh" : "$writeh";
              break;
          }
          for (const Operand& arg : effect_op.args) {
            result += ", ";
            result += FormatOperand(arg);
          }
          return result;
        } else {
          return "unknown_effect";
        }
      },
      op);
}

auto Dumper::FormatType(TypeId id) const -> std::string {
  if (type_arena_ == nullptr) {
    return std::format("type[{}]", id.value);
  }
  return ToString((*type_arena_)[id]);
}

}  // namespace lyra::mir
