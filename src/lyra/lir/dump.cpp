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
    Line(std::format("Class \"{}\" (#{})", cls.name, id.value));
    Indent();
    if (cls.base.has_value()) {
      Line(std::format("Base: {}", FormatBase(*cls.base)));
    }
    for (std::size_t i = 0; i < cls.members.size(); ++i) {
      Line(
          std::format(
              "member[{}] \"{}\" : {}", i, cls.members[i].name,
              FormatType(cls.members[i].type)));
    }
    DumpFunction(cls.constructor);
    for (const Function& method : cls.methods) {
      DumpFunction(method);
    }
    Dedent();
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
            },
            [&](const LoadInstr& load) -> std::string {
              return std::format("load {}", FormatPlace(load.place));
            },
            [&](const StoreInstr& store) -> std::string {
              return std::format(
                  "store {} = {}", FormatPlace(store.place),
                  FormatOperand(store.value));
            },
            [&](const AddrOfInstr& addr) -> std::string {
              return std::format("addrof {}", FormatPlace(addr.place));
            },
            [&](const BinaryInstr& bin) -> std::string {
              return std::format(
                  "{} {}, {}", BinaryOpName(bin.op), FormatOperand(bin.lhs),
                  FormatOperand(bin.rhs));
            },
            [&](const UnaryInstr& un) -> std::string {
              return std::format(
                  "{} {}", UnaryOpName(un.op), FormatOperand(un.operand));
            },
            [&](const BoolCastInstr& cast) -> std::string {
              return std::format("bool {}", FormatOperand(cast.operand));
            },
            [&](const PointerCastInstr& cast) -> std::string {
              return std::format("ptrcast {}", FormatOperand(cast.operand));
            }},
        data);
  }

  [[nodiscard]] static auto FormatTerminator(const Terminator& term)
      -> std::string {
    return std::visit(
        Overloaded{
            [&](const ReturnTerm& ret) -> std::string {
              if (ret.value.has_value()) {
                return std::format("return {}", FormatOperand(*ret.value));
              }
              return "return";
            },
            [](const BranchTerm& br) -> std::string {
              return std::format("br bb{}", br.target.value);
            },
            [&](const CondBranchTerm& br) -> std::string {
              return std::format(
                  "br {} ? bb{} : bb{}", FormatOperand(br.condition),
                  br.if_true.value, br.if_false.value);
            },
            [](const SuspendTerm& s) -> std::string {
              return std::format("suspend -> bb{}", s.resume.value);
            },
            [](const UnreachableTerm&) -> std::string {
              return "unreachable";
            }},
        term.data);
  }

  [[nodiscard]] static auto FormatBase(const Base& base) -> std::string {
    return std::visit(
        Overloaded{[](const RuntimeLibraryBase& r) -> std::string {
          switch (r.kind) {
            case RuntimeBaseKind::kInstance:
              return "Instance";
            case RuntimeBaseKind::kGenScope:
              return "GenScope";
            case RuntimeBaseKind::kScope:
              return "Scope";
          }
          return "?";
        }},
        base);
  }

  [[nodiscard]] auto FormatCallTarget(const CallTarget& target) const
      -> std::string {
    return std::visit(
        Overloaded{
            [](const BuiltinTarget& b) -> std::string {
              const std::string name{support::BuiltinFnName(b.fn)};
              if (!b.qualifier.has_value()) {
                return name;
              }
              return std::format("{}<{}>", name, FormatType(*b.qualifier));
            },
            [&](const MethodTarget& m) -> std::string {
              return unit_->classes.Get(m.method.class_id)
                  .methods[m.method.index]
                  .name;
            },
            [](const ConstructTarget&) -> std::string { return "Construct"; }},
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

  [[nodiscard]] static auto FormatPlace(const Place& place) -> std::string {
    std::string out = FormatOperand(place.base);
    for (const Projection& step : place.chain) {
      std::visit(
          Overloaded{
              [&](const DerefProjection&) { out += ".deref"; },
              [&](const MemberProjection& m) {
                out += std::format(".member({})", m.member.value);
              }},
          step);
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
            },
            [](const FuncRef& f) -> std::string {
              return std::format(
                  "funcref {}.{}", f.method.class_id.value, f.method.index);
            }},
        op);
  }

  [[nodiscard]] static auto FormatType(TypeId type) -> std::string {
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
  std::string out_;
  int indent_ = 0;
};

}  // namespace

auto DumpLir(const CompilationUnit& unit) -> std::string {
  return LirDumper(unit).Dump();
}

}  // namespace lyra::lir
