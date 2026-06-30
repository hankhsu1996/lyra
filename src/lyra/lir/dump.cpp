#include "lyra/lir/dump.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <string>
#include <string_view>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lir {

namespace {

class LirDumper {
 public:
  explicit LirDumper(const CompilationUnit& unit) : unit_(&unit) {
  }

  auto Dump() -> std::string {
    Line("LirUnit");
    Indent();
    for (std::size_t i = 0; i < unit_->classes.size(); ++i) {
      DumpClass(ClassId{static_cast<std::uint32_t>(i)});
    }
    Dedent();
    return std::move(out_);
  }

 private:
  void DumpClass(ClassId id) {
    const Class& cls = unit_->classes.Get(id);
    const mir::ClassId mir_id{id.value};
    current_class_ = &unit_->source->GetClass(mir_id);
    Line(std::format("Class \"{}\" (#{})", cls.name, id.value));
    Indent();
    DumpFunction(cls.constructor);
    for (const Function& method : cls.methods) {
      DumpFunction(method);
    }
    Dedent();
    current_class_ = nullptr;
  }

  void DumpFunction(const Function& fn) {
    std::string params;
    for (std::size_t i = 0; i < fn.params.size(); ++i) {
      if (i != 0) {
        params += ", ";
      }
      const ValueId pid = fn.params[i];
      params += std::format("%{} {}", pid.value, fn.values.Get(pid).name);
    }
    Line(
        std::format(
            "fn \"{}\"({}) -> {}", fn.name, params,
            FormatType(fn.result_type)));
    Indent();
    for (std::size_t b = 0; b < fn.blocks.size(); ++b) {
      Line(std::format("bb{}:", b));
      Indent();
      const BasicBlock& block = fn.blocks[b];
      for (const Instr& instr : block.instrs) {
        Line(
            std::format(
                "%{} = {} : {}", instr.result.value, FormatInstr(instr.data),
                FormatType(fn.values.Get(instr.result).type)));
      }
      Line(FormatTerminator(block.terminator));
      Dedent();
    }
    Dedent();
  }

  [[nodiscard]] auto FormatInstr(const InstrData& data) const -> std::string {
    return std::visit(
        Overloaded{
            [&](const CallInstr& call) -> std::string {
              return std::format(
                  "call {}({})", FormatCallTarget(call.target),
                  FormatOperands(call.args));
            },
            [&](const AggregateInstr& agg) -> std::string {
              return std::format("aggregate({})", FormatOperands(agg.elements));
            }},
        data);
  }

  [[nodiscard]] static auto FormatTerminator(const Terminator& term)
      -> std::string {
    return std::visit(
        Overloaded{[&](const ReturnTerm& ret) -> std::string {
          std::string keyword =
              ret.is_coroutine ? "return.coroutine" : "return";
          if (ret.value.has_value()) {
            return std::format("{} {}", keyword, FormatOperand(*ret.value));
          }
          return keyword;
        }},
        term.data);
  }

  [[nodiscard]] auto FormatCallTarget(const CallTarget& target) const
      -> std::string {
    return std::visit(
        Overloaded{
            [&](const mir::Direct& d) -> std::string {
              return std::visit(
                  Overloaded{
                      [&](const mir::MethodId& m) -> std::string {
                        std::string_view name =
                            current_class_->methods.Get(m).name;
                        return std::format("{}", name);
                      },
                      [](const support::BuiltinFn& id) -> std::string {
                        return std::string(support::BuiltinFnName(id));
                      }},
                  d.target);
            },
            [](const mir::Construct&) -> std::string { return "Construct"; }},
        target);
  }

  [[nodiscard]] static auto FormatOperands(const std::vector<Operand>& ops)
      -> std::string {
    std::string out;
    for (std::size_t i = 0; i < ops.size(); ++i) {
      if (i != 0) {
        out += ", ";
      }
      out += FormatOperand(ops[i]);
    }
    return out;
  }

  [[nodiscard]] static auto FormatOperand(const Operand& op) -> std::string {
    return std::visit(
        Overloaded{
            [](const Use& use) -> std::string {
              return std::format("%{}", use.value.value);
            },
            [](const IntConst& c) -> std::string {
              const std::uint64_t word = c.value.value_words.empty()
                                             ? 0U
                                             : c.value.value_words.front();
              return std::format("int:{:#x}", word);
            },
            [](const StrConst& c) -> std::string {
              return std::format("str:\"{}\"", c.value);
            }},
        op);
  }

  [[nodiscard]] static auto FormatType(mir::TypeId type) -> std::string {
    return std::format("t{}", type.value);
  }

  void Line(std::string_view text) {
    out_.append(static_cast<std::size_t>(indent_) * 2, ' ');
    out_.append(text);
    out_.push_back('\n');
  }
  void Indent() {
    ++indent_;
  }
  void Dedent() {
    if (indent_ == 0) {
      throw InternalError("LirDumper: dedent below zero");
    }
    --indent_;
  }

  const CompilationUnit* unit_;
  const mir::Class* current_class_ = nullptr;
  std::string out_;
  int indent_ = 0;
};

}  // namespace

auto DumpLir(const CompilationUnit& unit) -> std::string {
  return LirDumper(unit).Dump();
}

}  // namespace lyra::lir
