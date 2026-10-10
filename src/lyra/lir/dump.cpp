#include "lyra/lir/dump.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/struct_id.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/def_path.hpp"
#include "lyra/support/value_operation.hpp"

namespace lyra::lir {

namespace {

class LirDumper {
 public:
  explicit LirDumper(const CompilationUnit& unit) : unit_(&unit) {
  }

  auto Dump() -> std::string {
    Line(std::format("LirUnit \"{}\"", unit_->name));
    Indent();
    // The pool every `t<N>` below indexes. Without it an id is a number the
    // reader cannot resolve at all, so the question it answers -- is this a
    // machine boolean or a one-bit integral value -- gets guessed instead, and
    // a guess about a type is indistinguishable from knowing.
    Line("Types:");
    Indent();
    for (const TypeId id : unit_->types.Ids()) {
      Line(std::format("[{}] {}", id.value, DescribeType(id)));
    }
    Dedent();
    for (const ExternalClass& cls : unit_->external_classes) {
      DumpExternalClass(cls);
    }
    for (const StaticStorage& storage : unit_->static_storage) {
      Line(
          std::format(
              "static \"{}\" : {}", storage.symbol, FormatType(storage.type)));
    }
    for (const GlobalConstant& constant : unit_->constants) {
      Line(
          std::format(
              "{} constant \"{}\" = {}", LinkageName(constant.linkage),
              constant.symbol, FormatConstant(constant.initializer)));
    }
    for (const ClassId id : unit_->classes.Ids()) {
      DumpClass(id);
    }
    for (const ClosureId id : unit_->closures.Ids()) {
      DumpClosure(id);
    }
    for (const ExternalStruct& external : unit_->external_structs) {
      DumpExternalStruct(external);
    }
    for (const StructId id : unit_->structs.Ids()) {
      DumpStruct(id);
    }
    for (const Function& fn : unit_->functions) {
      DumpFunction(fn);
    }
    Dedent();
    return std::move(out_);
  }

 private:
  void DumpStruct(StructId id) {
    const Struct& declared = unit_->structs.Get(id);
    Line(
        std::format(
            R"(Struct "{}" (#{}))", support::DisplayOf(declared.path),
            id.value));
    Indent();
    DumpElements(declared.elements);
    for (const StructMethod& method : declared.methods) {
      Line(
          std::format(
              R"("{}": {})", support::ValueOperationName(method.answers),
              unit_->functions.Get(method.function).name));
    }
    Dedent();
  }

  void DumpExternalStruct(const ExternalStruct& external) {
    Line(
        std::format(
            "ExternalStruct \"{}.{}\"", external.declaration.unit_name,
            support::DisplayOf(external.declaration.path)));
    Indent();
    DumpElements(external.elements);
    Dedent();
  }

  void DumpElements(const std::vector<TypeId>& elements) {
    for (std::size_t i = 0; i < elements.size(); ++i) {
      Line(std::format("part[{}] : {}", i, FormatType(elements[i])));
    }
  }

  void DumpExternalClass(const ExternalClass& cls) {
    Line(
        std::format(
            "ExternalClass \"{}.{}\"", cls.unit_name,
            support::DisplayOf(cls.class_path)));
    Indent();
    if (cls.base.has_value()) {
      Line(std::format("base: {}", FormatType(*cls.base)));
    }
    for (std::size_t i = 0; i < cls.members.size(); ++i) {
      Line(std::format("member[{}] : {}", i, FormatType(cls.members[i].type)));
    }
    DumpDispatch(cls.dispatch);
    for (const TypeId iface : cls.implements) {
      Line(std::format("implements: {}", FormatType(iface)));
    }
    Dedent();
  }

  void DumpClass(ClassId id) {
    const Class& cls = unit_->classes.Get(id);
    Line(
        std::format(
            "Class{} (#{})",
            cls.path.has_value()
                ? std::format(" \"{}\"", support::DisplayOf(*cls.path))
                : std::string{},
            id.value));
    Indent();
    if (cls.base.has_value()) {
      Line(std::format("base: {}", FormatType(*cls.base)));
    }
    for (std::size_t i = 0; i < cls.members.size(); ++i) {
      Line(std::format("member[{}] : {}", i, FormatType(cls.members[i].type)));
    }
    DumpDispatch(cls.dispatch);
    for (const TypeId iface : cls.implements) {
      Line(std::format("implements: {}", FormatType(iface)));
    }
    for (const ConformingBehavior& answered : cls.conforming) {
      Line(
          std::format(
              "conforms: {} <- {}",
              FormatStatedDispatchRef(answered.interface_behavior),
              answered.answered_by.has_value()
                  ? FormatStatedDispatchRef(*answered.answered_by)
                  : std::string{"nothing"}));
    }
    Dedent();
  }

  [[nodiscard]] static auto LinkageName(Linkage linkage) -> std::string_view {
    switch (linkage) {
      case Linkage::kInternal:
        return "internal";
      case Linkage::kExternal:
        return "external";
    }
    throw InternalError("lir dump: unknown linkage");
  }

  void DumpDispatch(const ClassDispatch& dispatch) {
    for (std::size_t i = 0; i < dispatch.introduces.size(); ++i) {
      Line(
          std::format(
              "introduces[{}]: {}", i,
              dispatch.introduces[i].value_or("nothing")));
    }
    for (const Override& overriding : dispatch.overrides) {
      Line(
          std::format(
              "overrides {}: {}", FormatStatedDispatchRef(overriding.behavior),
              overriding.body));
    }
  }

  [[nodiscard]] auto FormatConstant(const Constant& constant) const
      -> std::string {
    const auto list = [&](std::span<const Constant> parts) {
      std::string out;
      for (const Constant& part : parts) {
        if (!out.empty()) {
          out += ", ";
        }
        out += FormatConstant(part);
      }
      return out;
    };
    return std::visit(
        Overloaded{
            [](const ConstantInt& c) { return std::format("{}", c.value); },
            [](const ConstantNull&) { return std::string{"null"}; },
            [](const ConstantString& c) {
              return std::format("\"{}\"", c.text);
            },
            [&](const ConstantFunction& c) {
              return std::format("&{}", unit_->functions.Get(c.function).name);
            },
            [](const ConstantAddress& c) {
              return std::format("&{}", c.symbol);
            },
            [&](const ConstantRecord& c) {
              return std::format(
                  "{} {{{}}}",
                  Type{RuntimeLibraryType{.kind = c.kind}}.KindName(),
                  list(c.parts));
            },
            [&](const ConstantArray& c) {
              return std::format("[{}]", list(c.elements));
            }},
        constant.value);
  }

  void DumpClosure(ClosureId id) {
    const Closure& closure = unit_->closures.Get(id);
    Line(std::format("Closure (#{})", id.value));
    Indent();
    for (std::size_t i = 0; i < closure.captures.size(); ++i) {
      Line(
          std::format(
              "capture[{}] : {}", i, FormatType(closure.captures[i].type)));
    }
    Line(std::format("invoke: {}", unit_->functions.Get(closure.invoke).name));
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
    for (std::size_t v = 0; v < fn.variables.size(); ++v) {
      Line(std::format("var {}: {}", v, FormatType(fn.variables[v])));
    }
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
            [](const ReceiveDepartureInstr&) -> std::string {
              return "receive departure";
            },
            [&](const TupleInstr& tuple) -> std::string {
              return std::format("tuple({})", FormatOperands(tuple.components));
            },
            [](const OpenVariablesInstr&) -> std::string {
              return "open variables";
            },
            [&](const VariableAddressInstr& reached) -> std::string {
              return std::format(
                  "variable {}[{}]", FormatOperand(reached.variables),
                  reached.position.value);
            },
            [&](const CloseVariablesInstr& closed) -> std::string {
              return std::format(
                  "close variables {}", FormatOperand(closed.variables));
            },
            [&](const ClosureInstr& built) -> std::string {
              return std::format("closure({})", FormatOperands(built.captures));
            },
            [&](const ArrayInstr& array) -> std::string {
              return std::format("array({})", FormatOperands(array.elements));
            },
            [&](const CastInstr& cast) -> std::string {
              return std::format("cast {}", FormatOperand(cast.operand));
            },
            [&](const HandleCastInstr& cast) -> std::string {
              return std::format("handle_cast {}", FormatOperand(cast.operand));
            },
            [&](const DynamicCastInstr& cast) -> std::string {
              return std::format(
                  "dynamic_cast {}", FormatOperand(cast.operand));
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
              return std::format(
                  "suspend -> bb{} abandoned bb{}", s.resume.value,
                  s.abandoned.value);
            },
            [](const AbandonTerm&) -> std::string { return "abandon"; },
            [&](const DepartTerm& depart) -> std::string {
              return std::format("depart {}", FormatOperand(depart.departure));
            },
            [](const UnreachableTerm&) -> std::string { return "unreachable"; },
            [&](const DepartingCallInstr& call) -> std::string {
              return std::format(
                  "%{} = call {}({}) -> bb{} departs bb{}", call.result.value,
                  FormatCallTarget(call.target), FormatOperands(call.args),
                  call.returned.value, call.landing.value);
            }},
        term.data);
  }

  [[nodiscard]] static auto FormatStatedDispatchRef(StatedDispatchRef method)
      -> std::string {
    return std::format(
        "{}#{}", FormatType(method.introduced_by), method.ordinal.value);
  }

  [[nodiscard]] auto FormatCallTarget(const CallTarget& target) const
      -> std::string {
    return std::visit(
        Overloaded{
            [](const BuiltinTarget& b) -> std::string {
              return std::string{support::RuntimeEntryOf(b.fn).name};
            },
            [&](const FunctionTarget& f) -> std::string {
              return unit_->functions.Get(f.function).name;
            },
            [](const DispatchTarget& d) -> std::string {
              return std::format(
                  "dispatch {}", FormatStatedDispatchRef(d.method));
            },
            [&](const IndirectTarget& i) -> std::string {
              return std::format("through {}", FormatOperand(i.callee));
            },
            [](const ConstructTarget&) -> std::string { return "Construct"; },
            [](const LibraryConstructorTarget& c) -> std::string {
              return std::format(
                  "construct {}", support::RuntimeClassName(c.cls));
            },
            [](const SymbolTarget& s) -> std::string {
              return std::format("extern {}", s.symbol);
            },
            [](const ForeignTarget& f) -> std::string {
              return std::format("foreign {}", f.symbol);
            },
            [](const ValueCellTarget& f) -> std::string {
              return std::string{ValueCellOpName(f.op)};
            },
            [](const OpenWriteTarget& w) -> std::string {
              return std::string{OpenWriteOpName(w.op)};
            },
            [](const DesignatedBitsTarget& b) -> std::string {
              return std::string{DesignatedBitsOpName(b.op)};
            },
            [](const EndValueTarget&) -> std::string { return "end"; },
            [](const CopyValueTarget&) -> std::string { return "copy"; },
            [](const ControlEffectTarget& c) -> std::string {
              return std::string{ControlEffectOpName(c.op)};
            },
            [](const CoroutineTarget& c) -> std::string {
              return std::string{CoroutineOpName(c.op)};
            }},
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
                out += std::format(
                    ".member({}:{})", FormatType(m.member.declared_by),
                    m.member.slot.value);
              },
              [&](const ElementProjection& e) {
                out +=
                    std::format(".element({})", FormatOperands(e.coordinates));
              },
              [&](const ComponentProjection& c) {
                out += std::format(".component({})", c.index.value);
              }},
          step);
    }
    return out;
  }

  [[nodiscard]] static auto FormatSelector(const AggregateSelector& selector)
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
            [](const RealConst& c) -> std::string {
              return std::format("real:{}", c.value);
            },
            // Its type, where every other constant prints its value: referring
            // to nothing is the whole of a null's value, and which type's
            // nothing it is decides how it is realized.
            [](const NullConst& c) -> std::string {
              return std::format("null:{}", FormatType(c.type));
            },
            [](const BoolConst& c) -> std::string {
              return std::format("bool:{}", c.value ? "true" : "false");
            },
            [](const EnumTableRef& c) -> std::string {
              return std::format("enumtable:{}", c.table.value);
            },
            [](const IntegralConstantRef& c) -> std::string {
              return std::format("const:{}", c.constant.value);
            },
            [](const StaticRef& s) -> std::string {
              return std::format("staticref {}", s.symbol);
            },
            [](const DefinitionRef& c) -> std::string {
              return std::format("definition {}", FormatType(c.defined));
            }},
        op);
  }

  [[nodiscard]] static auto FormatType(TypeId type) -> std::string {
    return std::format("t{}", type.value);
  }

  // One type as the table states it: its kind, and for a type that stands for
  // storage or refers elsewhere, what it reaches, and for a struct, which
  // declaration it is. Those are the ones whose kind alone leaves the reader
  // where they started.
  [[nodiscard]] auto DescribeType(TypeId id) const -> std::string {
    const Type& type = unit_->types.Get(id);
    if (const std::optional<TypeId> target = type.DerefTarget()) {
      return std::format("{}({})", type.KindName(), FormatType(*target));
    }
    if (const auto* structure = type.As<StructType>()) {
      return std::format(
          "{}({})", type.KindName(), FormatStructRef(structure->declaration));
    }
    return std::string{type.KindName()};
  }

  [[nodiscard]] static auto FormatStructRef(const StructRef& ref)
      -> std::string {
    return std::visit(
        Overloaded{
            [](StructId id) { return std::format("#{}", id.value); },
            [](const TypeDeclarationRef& declared) {
              return std::format(
                  "{}.{}", declared.unit_name,
                  support::DisplayOf(declared.path));
            }},
        ref);
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
