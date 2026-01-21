#include "lyra/mir/dumper.hpp"

#include <cassert>
#include <format>
#include <type_traits>
#include <variant>

#include "lyra/common/format.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/severity.hpp"
#include "lyra/mir/builtin.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/operator.hpp"
#include "lyra/mir/place_type.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::mir {

namespace {

auto FormatTerminator(const Terminator& term) -> std::string {
  return std::visit(
      [](const auto& t) -> std::string {
        using T = std::decay_t<decltype(t)>;
        if constexpr (std::is_same_v<T, Jump>) {
          return std::format("jump bb{}", t.target.value);
        } else if constexpr (std::is_same_v<T, Branch>) {
          return std::format(
              "branch %{} bb{}, bb{}", t.condition.value, t.then_target.value,
              t.else_target.value);
        } else if constexpr (std::is_same_v<T, Switch>) {
          std::string result = std::format("switch %{} [", t.selector.value);
          for (size_t i = 0; i < t.targets.size(); ++i) {
            if (i > 0) {
              result += ", ";
            }
            result += std::format("bb{}", t.targets[i].value);
          }
          result += "]";
          return result;
        } else if constexpr (std::is_same_v<T, QualifiedDispatch>) {
          const char* qualifier_str =
              (t.qualifier == DispatchQualifier::kUnique) ? "unique"
                                                          : "unique0";
          const char* stmt_str =
              (t.statement_kind == DispatchStatementKind::kIf) ? "if" : "case";
          std::string result = std::format("{}_{}(", qualifier_str, stmt_str);
          for (size_t i = 0; i < t.conditions.size(); ++i) {
            if (i > 0) {
              result += ", ";
            }
            result += std::format("%{}", t.conditions[i].value);
          }
          result += ") -> [";
          for (size_t i = 0; i < t.targets.size(); ++i) {
            if (i > 0) {
              result += ", ";
            }
            result += std::format("bb{}", t.targets[i].value);
          }
          result += "]";
          if (t.has_else) {
            result += " (has_else)";
          }
          return result;
        } else if constexpr (std::is_same_v<T, Delay>) {
          return "delay";
        } else if constexpr (std::is_same_v<T, Wait>) {
          return "wait";
        } else if constexpr (std::is_same_v<T, Return>) {
          return "return";
        } else if constexpr (std::is_same_v<T, Finish>) {
          const char* kind_str = nullptr;
          switch (t.kind) {
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
          return std::format("terminate({}, {})", kind_str, t.level);
        } else if constexpr (std::is_same_v<T, Repeat>) {
          return "repeat";
        } else {
          return "<?>";
        }
      },
      term);
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

  const char* kind_str = nullptr;
  switch (proc.kind) {
    case ProcessKind::kOnce:
      kind_str = "process.once";
      break;
    case ProcessKind::kFinal:
      kind_str = "process.final";
      break;
    case ProcessKind::kLooping:
      kind_str = "process.loop";
      break;
  }
  *out_ << kind_str << " {\n";
  Indent();

  for (uint32_t i = 0; i < proc.blocks.size(); ++i) {
    DumpBlock(proc.blocks[i], i);
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

  for (uint32_t i = 0; i < func.blocks.size(); ++i) {
    DumpBlock(func.blocks[i], i);
  }

  Dedent();
  PrintIndent();
  *out_ << "}\n";
}

void Dumper::DumpBlock(const BasicBlock& bb, uint32_t index) {
  PrintIndent();
  *out_ << std::format("bb{}: {{\n", index);
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
          } else if constexpr (std::is_same_v<T, GuardedAssign>) {
            *out_ << std::format(
                "guarded_assign {} = {} if {}\n", FormatPlace(i.target),
                FormatOperand(i.source), FormatOperand(i.validity));
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

auto Dumper::FormatIndexOperand(const Operand& op) const -> std::string {
  switch (op.kind) {
    case Operand::Kind::kConst: {
      const auto& constant = std::get<Constant>(op.payload);
      if (const auto* integral =
              std::get_if<IntegralConstant>(&constant.value)) {
        auto value = integral->value.empty() ? 0UL : integral->value[0];
        return std::format("{}", value);
      }
      return "?";
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
      return std::format("{}{}", prefix, place.root.id);
    }
    case Operand::Kind::kPoison:
      return "poison";
  }
  return "?";
}

auto Dumper::FormatProjection(const Projection& proj) const -> std::string {
  return std::visit(
      Overloaded{
          [](const FieldProjection& f) {
            return std::format(".{}", f.field_index);
          },
          [this](const IndexProjection& i) {
            return std::format("[{}]", FormatIndexOperand(i.index));
          },
          [this](const SliceProjection& s) {
            return std::format(
                "[{}+:{}]", FormatIndexOperand(s.start), s.width);
          },
          [](const DerefProjection& /*d*/) { return std::string(".*"); },
          [this](const BitRangeProjection& b) {
            return std::format(
                "[bitrange:offset={}:width={}:elem={}]",
                FormatIndexOperand(b.bit_offset), b.width,
                FormatType(b.element_type));
          },
      },
      proj.info);
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
    result += FormatProjection(proj);
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
        result += FormatProjection(proj);
      }
      return std::format("use({})", result);
    }
    case Operand::Kind::kPoison:
      return "poison";
  }
  return "<?>";
}

auto Dumper::FormatRvalue(const Rvalue& rv) const -> std::string {
  std::string result = std::visit(
      Overloaded{
          [](const UnaryRvalueInfo& info) {
            return std::format("unary({})", ToString(info.op));
          },
          [](const BinaryRvalueInfo& info) {
            return std::format("binary({})", ToString(info.op));
          },
          [this](const CastRvalueInfo& info) {
            return std::format(
                "cast({} -> {})", FormatType(info.source_type),
                FormatType(info.target_type));
          },
          [](const SystemCallRvalueInfo& info) {
            return std::format("syscall({})", info.opcode);
          },
          [](const UserCallRvalueInfo& info) {
            return std::format("call(func[{}])", info.callee.value);
          },
          [this](const AggregateRvalueInfo& info) {
            return std::format("aggregate<{}>", FormatType(info.result_type));
          },
          [](const BuiltinCallRvalueInfo& info) {
            const char* method_name = "unknown";
            switch (info.method) {
              case BuiltinMethod::kNewArray:
                method_name = "new[]";
                break;
              case BuiltinMethod::kArraySize:
                method_name = "size";
                break;
              case BuiltinMethod::kArrayDelete:
                method_name = "delete";
                break;
              case BuiltinMethod::kQueueSize:
                method_name = "queue_size";
                break;
              case BuiltinMethod::kQueueDelete:
                method_name = "queue_delete";
                break;
              case BuiltinMethod::kQueueDeleteAt:
                method_name = "queue_delete_at";
                break;
              case BuiltinMethod::kQueuePushBack:
                method_name = "queue_push_back";
                break;
              case BuiltinMethod::kQueuePushFront:
                method_name = "queue_push_front";
                break;
              case BuiltinMethod::kQueuePopBack:
                method_name = "queue_pop_back";
                break;
              case BuiltinMethod::kQueuePopFront:
                method_name = "queue_pop_front";
                break;
              case BuiltinMethod::kQueueInsert:
                method_name = "queue_insert";
                break;
            }
            return std::format("builtin({})", method_name);
          },
          [](const IndexValidityRvalueInfo& info) {
            return std::format(
                "index_validity(bounds=[{}, {}], check_known={})",
                info.lower_bound, info.upper_bound, info.check_known);
          },
          [this](const GuardedUseRvalueInfo& info) {
            return std::format(
                "guarded_use({}, type={})", FormatPlace(info.place),
                FormatType(info.result_type));
          },
          [this](const ConcatRvalueInfo& info) {
            return std::format("concat<{}>", FormatType(info.result_type));
          },
      },
      rv.info);

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
          std::string result = effect_op.print_kind == PrintKind::kDisplay
                                   ? "$display"
                                   : "$write";
          result += "(";
          bool first = true;
          for (const FormatOp& op : effect_op.ops) {
            if (!first) {
              result += ", ";
            }
            first = false;
            if (op.kind == FormatKind::kLiteral) {
              result += std::format("literal(\"{}\")", op.literal);
            } else {
              const char* kind_str = "?";
              switch (op.kind) {
                case FormatKind::kDecimal:
                  kind_str = "%d";
                  break;
                case FormatKind::kHex:
                  kind_str = "%h";
                  break;
                case FormatKind::kBinary:
                  kind_str = "%b";
                  break;
                case FormatKind::kOctal:
                  kind_str = "%o";
                  break;
                case FormatKind::kString:
                  kind_str = "%s";
                  break;
                case FormatKind::kReal:
                  kind_str = "%f";
                  break;
                case FormatKind::kLiteral:
                  break;  // Already handled above
              }
              result +=
                  std::format("{}:{}", kind_str, FormatOperand(*op.value));
            }
          }
          result += ")";
          return result;
        } else if constexpr (std::is_same_v<T, SeverityEffect>) {
          std::string result;
          switch (effect_op.level) {
            case Severity::kInfo:
              result = "$info";
              break;
            case Severity::kWarning:
              result = "$warning";
              break;
            case Severity::kError:
              result = "$error";
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
