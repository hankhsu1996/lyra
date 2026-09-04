#include "lyra/lir/dump.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

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
    for (const ExternalUnitObjectId id : unit_->external_unit_objects.Ids()) {
      DumpExternalUnitObject(id);
    }
    for (const StaticStorage& storage : unit_->static_storage) {
      Line(
          std::format(
              "static \"{}\" : {}", storage.symbol, FormatType(storage.type)));
    }
    for (const ClassId id : unit_->classes.Ids()) {
      DumpClass(id);
    }
    for (const ClosureId id : unit_->closures.Ids()) {
      DumpClosure(id);
    }
    for (const Function& fn : unit_->functions) {
      DumpFunction(fn);
    }
    Dedent();
    return std::move(out_);
  }

 private:
  void DumpExternalUnitObject(ExternalUnitObjectId id) {
    const ExternalUnitObject& object = unit_->external_unit_objects.Get(id);
    Line(
        std::format(
            "ExternalUnitObject \"{}.{}\" (#{})", object.unit_name,
            object.class_name, id.value));
    Indent();
    for (std::size_t i = 0; i < object.members.size(); ++i) {
      Line(
          std::format(
              "member[{}] \"{}\" : {}", i, object.members[i].name,
              FormatType(object.members[i].type)));
    }
    Dedent();
  }

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
    DumpFunction(unit_->functions.Get(cls.constructor));
    for (const FunctionId method : cls.methods) {
      DumpFunction(unit_->functions.Get(method));
    }
    Dedent();
  }

  void DumpClosure(ClosureId id) {
    const Closure& closure = unit_->closures.Get(id);
    Line(std::format("Closure \"{}\" (#{})", closure.name, id.value));
    Indent();
    for (std::size_t i = 0; i < closure.captures.size(); ++i) {
      Line(
          std::format(
              "capture[{}] \"{}\" : {}", i, closure.captures[i].name,
              FormatType(closure.captures[i].type)));
    }
    DumpFunction(unit_->functions.Get(closure.invoke));
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
            [&](const ProductInstr& product) -> std::string {
              return std::format(
                  "product({})", FormatOperands(product.components));
            },
            [&](const ArrayInstr& array) -> std::string {
              return std::format("array({})", FormatOperands(array.elements));
            },
            [&](const UnionInstr& u) -> std::string {
              return std::format(
                  "union({}, {})", u.index.value, FormatOperand(u.value));
            },
            [&](const ValueCastInstr& cast) -> std::string {
              return std::format("valuecast {}", FormatOperand(cast.operand));
            },
            [&](const AggregateExtractInstr& extract) -> std::string {
              return std::format(
                  "aggregate_extract {}, {}", FormatOperand(extract.aggregate),
                  FormatSelector(extract.selector));
            },
            [&](const AggregateUpdateInstr& update) -> std::string {
              return std::format(
                  "aggregate_update {}, {}, {}",
                  FormatOperand(update.aggregate),
                  FormatSelector(update.selector),
                  FormatOperand(update.replacement));
            },
            [&](const TagTestInstr& test) -> std::string {
              return std::format(
                  "tag_test {}, {}", FormatOperand(test.aggregate),
                  test.index.value);
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
            },
            [&](const IntCastInstr& cast) -> std::string {
              return std::format("intcast {}", FormatOperand(cast.operand));
            }},
        data);
  }

  [[nodiscard]] auto FormatTerminator(const Terminator& term) const
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
        Overloaded{
            [](const IntraUnitBase& i) -> std::string {
              return std::format("Class[{}]", i.class_id.value);
            },
            [](const CrossUnitBase& e) -> std::string {
              return std::format(
                  "CrossUnit(\"{}::{}\")", e.unit_name, e.class_name);
            },
            [](const RuntimeBase& e) -> std::string {
              return std::format("Runtime(\"{}\")", e.symbol);
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
            [&](const FunctionTarget& f) -> std::string {
              return unit_->functions.Get(f.function).name;
            },
            [](const ConstructTarget&) -> std::string { return "Construct"; },
            [](const ForeignTarget& f) -> std::string {
              return std::format("extern {}", f.symbol);
            },
            [](const ValueCellTarget& f) -> std::string {
              return std::string{ValueCellOpName(f.op)};
            },
            [](const ControlEffectTarget& c) -> std::string {
              return std::string{ControlEffectOpName(c.op)};
            },
            [](const CoroutineTarget& c) -> std::string {
              return std::string{CoroutineOpName(c.op)};
            }},
        target);
  }

  [[nodiscard]] auto FormatOperands(const std::vector<Operand>& ops) const
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

  [[nodiscard]] auto FormatPlace(const Place& place) const -> std::string {
    std::string out = FormatOperand(place.base);
    for (const Projection& step : place.chain) {
      std::visit(
          Overloaded{
              [&](const DerefProjection&) { out += ".deref"; },
              [&](const MemberProjection& m) {
                out += std::format(
                    ".member({}:{})", FormatType(m.member.declared_by),
                    m.member.slot.value);
              }},
          step);
    }
    return out;
  }

  [[nodiscard]] auto FormatSelector(const AggregateSelector& selector) const
      -> std::string {
    return std::visit(
        Overloaded{
            [](const Component& c) -> std::string {
              return std::format("component {}", c.index.value);
            },
            [&](const ContainerElement& e) -> std::string {
              return std::format("element({})", FormatOperands(e.operands));
            },
            [&](const ContainerSlice& s) -> std::string {
              return std::format("slice({})", FormatOperands(s.operands));
            }},
        selector);
  }

  [[nodiscard]] auto FormatOperand(const Operand& op) const -> std::string {
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
            [](const RealConst& c) -> std::string {
              return std::format("real:{}", c.value);
            },
            [](const NullConst&) -> std::string { return "null"; },
            [](const BoolConst& c) -> std::string {
              return std::format("bool:{}", c.value ? "true" : "false");
            },
            [](const PackedTypeRef& c) -> std::string {
              return std::format("packedtype:t{}", c.integral.value);
            },
            [&](const FuncRef& f) -> std::string {
              return std::format(
                  "funcref {}", unit_->functions.Get(f.function).name);
            },
            [](const StaticRef& s) -> std::string {
              return std::format("staticref {}", s.symbol);
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
