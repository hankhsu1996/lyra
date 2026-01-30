#include "lyra/mir/dumper.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <ostream>
#include <string>
#include <type_traits>
#include <variant>

#include "lyra/common/constant.hpp"
#include "lyra/common/format.hpp"
#include "lyra/common/integral_constant.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/math_fn.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/severity.hpp"
#include "lyra/common/system_tf.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/basic_block.hpp"
#include "lyra/mir/builtin.hpp"
#include "lyra/mir/design.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/module.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/operator.hpp"
#include "lyra/mir/package.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/place_type.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/mir/rvalue.hpp"
#include "lyra/mir/statement.hpp"
#include "lyra/mir/terminator.hpp"

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
          return std::format("delay #{} -> bb{}", t.ticks, t.resume.value);
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
          std::string result =
              std::format("terminate({}, {}", kind_str, t.level);
          if (t.message.has_value() && t.message->kind == Operand::Kind::kUse) {
            auto place_id = std::get<PlaceId>(t.message->payload);
            result += std::format(", %{}", place_id.value);
          }
          result += ")";
          return result;
        } else if constexpr (std::is_same_v<T, Repeat>) {
          return "repeat";
        } else {
          return "<?>";
        }
      },
      term.data);
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
  if (indent_ == 0) {
    throw common::InternalError("Dumper::Dedent", "indent underflow");
  }
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

  for (const Statement& stmt : bb.statements) {
    PrintIndent();
    std::visit(
        [this](const auto& i) {
          using T = std::decay_t<decltype(i)>;
          if constexpr (std::is_same_v<T, Assign>) {
            *out_ << std::format(
                "{} = {}\n", FormatPlace(i.dest), FormatRhs(i.rhs));
          } else if constexpr (std::is_same_v<T, GuardedAssign>) {
            *out_ << std::format(
                "guarded_assign {} = {} if {}\n", FormatPlace(i.dest),
                FormatRhs(i.rhs), FormatOperand(i.guard));
          } else if constexpr (std::is_same_v<T, Effect>) {
            *out_ << FormatEffect(i.op) << "\n";
          } else if constexpr (std::is_same_v<T, DeferredAssign>) {
            *out_ << std::format(
                "{} <= {}\n", FormatPlace(i.dest), FormatRhs(i.rhs));
          } else if constexpr (std::is_same_v<T, Call>) {
            // Format callee (user function or system TF)
            std::string callee_str = std::visit(
                [](const auto& c) -> std::string {
                  using C = std::decay_t<decltype(c)>;
                  if constexpr (std::is_same_v<C, FunctionId>) {
                    return std::format("@fn{}", c.value);
                  } else {
                    return ToString(c);
                  }
                },
                i.callee);

            // Format input args
            std::string args;
            for (size_t idx = 0; idx < i.in_args.size(); ++idx) {
              if (idx > 0) args += ", ";
              args += FormatOperand(i.in_args[idx]);
            }

            // Format output: ret.tmp -> ret.dest (if present)
            std::string output;
            if (i.ret) {
              output = FormatPlace(i.ret->tmp);
              if (i.ret->dest) {
                output += std::format(" -> {}", FormatPlace(*i.ret->dest));
              }
            }

            // Format writebacks
            std::string writebacks;
            for (const auto& wb : i.writebacks) {
              if (!writebacks.empty()) writebacks += ", ";
              const char* mode_str = "out";
              if (wb.mode == PassMode::kInOut) mode_str = "inout";
              if (wb.mode == PassMode::kRef) mode_str = "ref";
              writebacks += std::format(
                  "{}[{}]: {} -> {}", mode_str, wb.arg_index,
                  FormatPlace(wb.tmp), FormatPlace(wb.dest));
            }

            // Build output string
            if (!output.empty()) {
              *out_ << output << " = ";
            }
            *out_ << "call " << callee_str << "(" << args << ")";
            if (!writebacks.empty()) {
              *out_ << " writebacks=[" << writebacks << "]";
            }
            *out_ << "\n";
          } else if constexpr (std::is_same_v<T, BuiltinCall>) {
            std::string args;
            for (size_t idx = 0; idx < i.args.size(); ++idx) {
              if (idx > 0) args += ", ";
              args += FormatOperand(i.args[idx]);
            }
            if (i.dest) {
              *out_ << std::format(
                  "{} = builtin_call {}({}, {})\n", FormatPlace(*i.dest),
                  static_cast<int>(i.method), FormatPlace(i.receiver), args);
            } else {
              *out_ << std::format(
                  "builtin_call {}({}, {})\n", static_cast<int>(i.method),
                  FormatPlace(i.receiver), args);
            }
          }
        },
        stmt.data);
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
      common::Overloaded{
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
          [](const UnionMemberProjection& u) {
            return std::format(".union_member[{}]", u.member_index);
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
      common::Overloaded{
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
          [this](const BitCastRvalueInfo& info) {
            return std::format(
                "bitcast({} -> {})", FormatType(info.source_type),
                FormatType(info.target_type));
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
              case BuiltinMethod::kEnumNext:
                method_name = "enum_next";
                break;
              case BuiltinMethod::kEnumPrev:
                method_name = "enum_prev";
                break;
              case BuiltinMethod::kEnumName:
                method_name = "enum_name";
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
          [](const SFormatRvalueInfo& info) {
            return std::format(
                "sformat(fmt='{}', runtime={})",
                FormatKindToSpecChar(info.default_format),
                info.has_runtime_format);
          },
          [](const TestPlusargsRvalueInfo&) {
            return std::string("test_plusargs");
          },
          [](const RuntimeQueryRvalueInfo&) {
            return std::string("runtime_query");
          },
          [](const MathCallRvalueInfo& info) {
            return std::format("math_call({})", ToString(info.fn));
          },
          [](const SystemTfRvalueInfo& info) {
            return std::format("system_tf({})", ToString(info.opcode));
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
          const char* name = nullptr;
          if (effect_op.descriptor.has_value()) {
            name = effect_op.print_kind == PrintKind::kDisplay ? "$fdisplay"
                                                               : "$fwrite";
          } else {
            name = effect_op.print_kind == PrintKind::kDisplay ? "$display"
                                                               : "$write";
          }
          std::string result = name;
          result += "(";
          if (effect_op.descriptor) {
            result += FormatOperand(*effect_op.descriptor);
            if (!effect_op.ops.empty()) {
              result += ", ";
            }
          }
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
                case FormatKind::kTime:
                  kind_str = "%t";
                  break;
                case FormatKind::kChar:
                  kind_str = "%c";
                  break;
                case FormatKind::kModulePath:
                  kind_str = "%m";
                  break;
                case FormatKind::kLiteral:
                  break;  // Already handled above
              }
              if (op.value.has_value()) {
                result +=
                    std::format("{}:{}", kind_str, FormatOperand(*op.value));
              } else {
                result += kind_str;
              }
            }
          }
          result += ")";
          return result;
        } else if constexpr (std::is_same_v<T, SeverityEffect>) {
          std::string result;
          switch (effect_op.level) {
            case Severity::kInfo:
              result = "$info(";
              break;
            case Severity::kWarning:
              result = "$warning(";
              break;
            case Severity::kError:
              result = "$error(";
              break;
          }
          for (size_t i = 0; i < effect_op.ops.size(); ++i) {
            if (i > 0) result += ", ";
            const auto& op = effect_op.ops[i];
            std::string kind_str;
            switch (op.kind) {
              case FormatKind::kLiteral:
                result += std::format("\"{}\"", op.literal);
                continue;
              case FormatKind::kDecimal:
                kind_str = "d";
                break;
              case FormatKind::kHex:
                kind_str = "h";
                break;
              case FormatKind::kBinary:
                kind_str = "b";
                break;
              case FormatKind::kOctal:
                kind_str = "o";
                break;
              case FormatKind::kString:
                kind_str = "s";
                break;
              case FormatKind::kReal:
                kind_str = "f";
                break;
              case FormatKind::kTime:
                kind_str = "t";
                break;
              case FormatKind::kChar:
                kind_str = "c";
                break;
              case FormatKind::kModulePath:
                kind_str = "m";
                break;
            }
            if (op.value.has_value()) {
              result +=
                  std::format("{}:{}", kind_str, FormatOperand(*op.value));
            } else {
              result += kind_str;
            }
          }
          result += ")";
          return result;
        } else if constexpr (std::is_same_v<T, MemIOEffect>) {
          std::string result;
          if (effect_op.is_read) {
            result = effect_op.is_hex ? "$readmemh" : "$readmemb";
          } else {
            result = effect_op.is_hex ? "$writememh" : "$writememb";
          }
          result += std::format(
              "({}, @{})", FormatOperand(effect_op.filename),
              FormatPlace(effect_op.target));
          if (effect_op.start_addr) {
            result +=
                std::format(" start={}", FormatOperand(*effect_op.start_addr));
          }
          if (effect_op.end_addr) {
            result +=
                std::format(" end={}", FormatOperand(*effect_op.end_addr));
          }
          return result;
        } else if constexpr (std::is_same_v<T, TimeFormatEffect>) {
          return std::format(
              "$timeformat(units={}, precision={}, suffix=\"{}\", "
              "min_width={})",
              effect_op.units, effect_op.precision, effect_op.suffix,
              effect_op.min_width);
        } else if constexpr (std::is_same_v<T, SystemTfEffect>) {
          std::string result =
              std::format("system_tf({})", ToString(effect_op.opcode));
          for (const Operand& arg : effect_op.args) {
            result += ", ";
            result += FormatOperand(arg);
          }
          return result;
        } else if constexpr (std::is_same_v<T, StrobeEffect>) {
          return std::format("$strobe(thunk=func[{}])", effect_op.thunk.value);
        } else if constexpr (std::is_same_v<T, MonitorEffect>) {
          return std::format(
              "$monitor(setup=func[{}], check=func[{}], buf_size={})",
              effect_op.setup_thunk.value, effect_op.check_thunk.value,
              effect_op.prev_buffer_size);
        } else if constexpr (std::is_same_v<T, MonitorControlEffect>) {
          return std::format("$monitor{}()", effect_op.enable ? "on" : "off");
        } else {
          return "unknown_effect";
        }
      },
      op);
}

auto Dumper::FormatRhs(const RightHandSide& rhs) const -> std::string {
  return std::visit(
      common::Overloaded{
          [this](const Operand& op) { return FormatOperand(op); },
          [this](const Rvalue& rv) { return FormatRvalue(rv); },
      },
      rhs);
}

auto Dumper::FormatType(TypeId id) const -> std::string {
  if (type_arena_ == nullptr) {
    return std::format("type[{}]", id.value);
  }
  return ToString((*type_arena_)[id]);
}

}  // namespace lyra::mir
