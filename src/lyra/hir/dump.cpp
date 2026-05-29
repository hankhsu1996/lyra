#include "lyra/hir/dump.hpp"

#include <cstddef>
#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/method.hpp"
#include "lyra/hir/module_unit.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/hir/structural_hops.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/structural_var.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/unary_op.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::hir {

namespace {

class HirDumper {
 public:
  auto Dump(const std::vector<ModuleUnit>& units) -> std::string {
    for (const auto& u : units) {
      DumpUnit(u);
    }
    return std::move(out_);
  }

 private:
  void Line(std::string_view text) {
    out_.append(static_cast<std::size_t>(indent_) * 2, ' ');
    out_.append(text);
    out_.push_back('\n');
  }
  void Indent() {
    ++indent_;
  }
  void Dedent() {
    if (indent_ <= 0) {
      throw InternalError("HirDumper: indent underflow");
    }
    --indent_;
  }

  static auto FormatBitAtom(BitAtom a) -> std::string_view {
    switch (a) {
      case BitAtom::kBit:
        return "bit";
      case BitAtom::kLogic:
        return "logic";
      case BitAtom::kReg:
        return "reg";
    }
    throw InternalError("HirDumper::FormatBitAtom: unknown BitAtom");
  }

  static auto FormatSignedness(Signedness s) -> std::string_view {
    return s == Signedness::kSigned ? "signed" : "unsigned";
  }

  static auto FormatUniquePriorityCheck(
      std::optional<UniquePriorityCheck> check) -> std::string {
    if (!check.has_value()) return "";
    switch (*check) {
      case UniquePriorityCheck::kUnique:
        return "[unique]";
      case UniquePriorityCheck::kUnique0:
        return "[unique0]";
      case UniquePriorityCheck::kPriority:
        return "[priority]";
    }
    throw InternalError("HirDumper::FormatUniquePriorityCheck: unknown check");
  }

  static auto FormatBuiltinMethodKind(BuiltinMethodKind k) -> std::string_view {
    switch (k) {
      case BuiltinMethodKind::kEnumFirst:
        return "first";
      case BuiltinMethodKind::kEnumLast:
        return "last";
      case BuiltinMethodKind::kEnumNum:
        return "num";
      case BuiltinMethodKind::kEnumNext:
        return "next";
      case BuiltinMethodKind::kEnumPrev:
        return "prev";
      case BuiltinMethodKind::kEnumName:
        return "name";
      case BuiltinMethodKind::kNamedEventTrigger:
        return "trigger";
      case BuiltinMethodKind::kNamedEventAwait:
        return "await";
      case BuiltinMethodKind::kNamedEventTriggered:
        return "triggered";
    }
    throw InternalError(
        "HirDumper::FormatBuiltinMethodKind: unknown BuiltinMethodKind");
  }

  static auto FormatPackedForm(PackedArrayForm f) -> std::string_view {
    switch (f) {
      case PackedArrayForm::kExplicit:
        return "explicit";
      case PackedArrayForm::kByte:
        return "byte";
      case PackedArrayForm::kShortInt:
        return "shortint";
      case PackedArrayForm::kInt:
        return "int";
      case PackedArrayForm::kLongInt:
        return "longint";
      case PackedArrayForm::kInteger:
        return "integer";
      case PackedArrayForm::kTime:
        return "time";
    }
    throw InternalError("HirDumper::FormatPackedForm: unknown PackedArrayForm");
  }

  static auto FormatPackedDims(const std::vector<PackedRange>& dims)
      -> std::string {
    if (dims.empty()) {
      return "[]";
    }
    std::string out;
    for (const auto& d : dims) {
      out += std::format("[{}:{}]", d.left, d.right);
    }
    return out;
  }

  static auto FormatUnpackedDims(const std::vector<UnpackedRange>& dims)
      -> std::string {
    if (dims.empty()) {
      return "[]";
    }
    std::string out;
    for (const auto& d : dims) {
      out += std::format("[{}:{}]", d.left, d.right);
    }
    return out;
  }

  static auto FormatType(const Type& t) -> std::string {
    return std::visit(
        Overloaded{
            [](const PackedArrayType& p) -> std::string {
              return std::format(
                  "PackedArray(atom={}, signed={}, dims={}, form={})",
                  FormatBitAtom(p.atom), FormatSignedness(p.signedness),
                  FormatPackedDims(p.dims), FormatPackedForm(p.form));
            },
            [](const EnumType& e) -> std::string {
              std::string members;
              for (std::size_t i = 0; i < e.members.size(); ++i) {
                if (i > 0) members += ", ";
                members += std::format(
                    "{}={}", e.members[i].name,
                    FormatIntegralConstant(e.members[i].value));
              }
              return std::format(
                  "Enum(base=Type[{}], members=[{}])", e.base_type.value,
                  members);
            },
            [](const UnpackedArrayType& u) -> std::string {
              return std::format(
                  "UnpackedArray(elem=Type[{}], dims={})", u.element_type.value,
                  FormatUnpackedDims(u.dims));
            },
            [](const DynamicArrayType& d) -> std::string {
              return std::format(
                  "DynamicArray(elem=Type[{}])", d.element_type.value);
            },
            [](const QueueType& q) -> std::string {
              if (q.max_bound.has_value()) {
                return std::format(
                    "Queue(elem=Type[{}], max={})", q.element_type.value,
                    *q.max_bound);
              }
              return std::format("Queue(elem=Type[{}])", q.element_type.value);
            },
            [](const AssociativeArrayType& a) -> std::string {
              if (a.key_type.has_value()) {
                return std::format(
                    "AssociativeArray(elem=Type[{}], key=Type[{}])",
                    a.element_type.value, a.key_type->value);
              }
              return std::format(
                  "AssociativeArray(elem=Type[{}], key=wildcard)",
                  a.element_type.value);
            },
            [](const StringType&) -> std::string { return "StringType"; },
            [](const EventType&) -> std::string { return "EventType"; },
            [](const RealType&) -> std::string { return "RealType"; },
            [](const ShortRealType&) -> std::string { return "ShortRealType"; },
            [](const RealTimeType&) -> std::string { return "RealTimeType"; },
            [](const ChandleType&) -> std::string { return "ChandleType"; },
            [](const VoidType&) -> std::string { return "VoidType"; },
        },
        t.data);
  }

  static auto FormatUnaryOp(UnaryOp op) -> std::string {
    switch (op) {
      case UnaryOp::kPlus:
        return "Plus";
      case UnaryOp::kMinus:
        return "Minus";
      case UnaryOp::kBitwiseNot:
        return "BitwiseNot";
      case UnaryOp::kLogicalNot:
        return "LogicalNot";
      case UnaryOp::kReductionAnd:
        return "ReductionAnd";
      case UnaryOp::kReductionOr:
        return "ReductionOr";
      case UnaryOp::kReductionXor:
        return "ReductionXor";
      case UnaryOp::kReductionNand:
        return "ReductionNand";
      case UnaryOp::kReductionNor:
        return "ReductionNor";
      case UnaryOp::kReductionXnor:
        return "ReductionXnor";
    }
    throw InternalError("HirDumper: unknown UnaryOp");
  }

  static auto FormatBinaryOp(BinaryOp op) -> std::string {
    switch (op) {
      case BinaryOp::kAdd:
        return "Add";
      case BinaryOp::kSub:
        return "Sub";
      case BinaryOp::kMul:
        return "Mul";
      case BinaryOp::kDiv:
        return "Div";
      case BinaryOp::kMod:
        return "Mod";
      case BinaryOp::kPower:
        return "Power";
      case BinaryOp::kBitwiseAnd:
        return "BitwiseAnd";
      case BinaryOp::kBitwiseOr:
        return "BitwiseOr";
      case BinaryOp::kBitwiseXor:
        return "BitwiseXor";
      case BinaryOp::kBitwiseXnor:
        return "BitwiseXnor";
      case BinaryOp::kEquality:
        return "Equality";
      case BinaryOp::kInequality:
        return "Inequality";
      case BinaryOp::kCaseEquality:
        return "CaseEquality";
      case BinaryOp::kCaseInequality:
        return "CaseInequality";
      case BinaryOp::kWildcardEquality:
        return "WildcardEquality";
      case BinaryOp::kWildcardInequality:
        return "WildcardInequality";
      case BinaryOp::kGreaterEqual:
        return "GreaterEqual";
      case BinaryOp::kGreaterThan:
        return "GreaterThan";
      case BinaryOp::kLessEqual:
        return "LessEqual";
      case BinaryOp::kLessThan:
        return "LessThan";
      case BinaryOp::kLogicalAnd:
        return "LogicalAnd";
      case BinaryOp::kLogicalOr:
        return "LogicalOr";
      case BinaryOp::kLogicalImplication:
        return "LogicalImplication";
      case BinaryOp::kLogicalEquivalence:
        return "LogicalEquivalence";
      case BinaryOp::kLogicalShiftLeft:
        return "LogicalShiftLeft";
      case BinaryOp::kLogicalShiftRight:
        return "LogicalShiftRight";
      case BinaryOp::kArithmeticShiftLeft:
        return "ArithmeticShiftLeft";
      case BinaryOp::kArithmeticShiftRight:
        return "ArithmeticShiftRight";
    }
    throw InternalError("HirDumper: unknown BinaryOp");
  }

  static auto FormatLvalueRoot(
      const std::variant<StructuralVarRef, ProceduralVarRef, LoopVarRef>& root)
      -> std::string {
    return std::visit(
        Overloaded{
            [](const StructuralVarRef& r) -> std::string {
              return std::format(
                  "StructuralVar[{}](hops={})", r.var.value, r.hops.value);
            },
            [](const ProceduralVarRef& r) -> std::string {
              return std::format("ProceduralVar[{}]", r.var.value);
            },
            [](const LoopVarRef& r) -> std::string {
              return std::format(
                  "LoopVar[{}](hops={})", r.loop_var.value, r.hops.value);
            },
        },
        root);
  }

  static auto FormatLvalueSelector(const LvalueSelector& sel) -> std::string {
    return std::visit(
        Overloaded{
            [](const ElementLvalueSelector& s) -> std::string {
              return std::format("Element index=Expr[{}]", s.index.value);
            },
            [](const RangeLvalueSelector& s) -> std::string {
              const auto bounds = std::visit(
                  Overloaded{
                      [](const RangeConstantBounds& b) -> std::string {
                        return std::format(
                            "const msb=Expr[{}] lsb=Expr[{}]", b.msb_expr.value,
                            b.lsb_expr.value);
                      },
                      [](const RangeIndexedUpBounds& b) -> std::string {
                        return std::format(
                            "indexed_up base=Expr[{}] width=Expr[{}]",
                            b.base_index.value, b.width.value);
                      },
                      [](const RangeIndexedDownBounds& b) -> std::string {
                        return std::format(
                            "indexed_down base=Expr[{}] width=Expr[{}]",
                            b.base_index.value, b.width.value);
                      },
                  },
                  s.bounds);
              return std::format("Range {}", bounds);
            },
        },
        sel);
  }

  static auto FormatLvalue(const Lvalue& v) -> std::string {
    std::string out = FormatLvalueRoot(v.root);
    for (const auto& sel : v.selectors) {
      out += std::format(" [{}]", FormatLvalueSelector(sel));
    }
    return out;
  }

  static auto FormatTimeScale(TimeScale s) -> std::string_view {
    switch (s) {
      case TimeScale::kFs:
        return "fs";
      case TimeScale::kPs:
        return "ps";
      case TimeScale::kNs:
        return "ns";
      case TimeScale::kUs:
        return "us";
      case TimeScale::kMs:
        return "ms";
      case TimeScale::kS:
        return "s";
    }
    throw InternalError("HirDumper::FormatTimeScale: unknown TimeScale");
  }

  static auto FormatLiteralBase(IntegerLiteralBase b) -> std::string_view {
    switch (b) {
      case IntegerLiteralBase::kBinary:
        return "b";
      case IntegerLiteralBase::kOctal:
        return "o";
      case IntegerLiteralBase::kDecimal:
        return "d";
      case IntegerLiteralBase::kHexadecimal:
        return "h";
      case IntegerLiteralBase::kUnbased:
        return "unbased";
    }
    throw InternalError("HirDumper::FormatLiteralBase: unknown base");
  }

  static auto FormatIntegralConstant(const IntegralConstant& c) -> std::string {
    std::string out = std::format(
        "{}'{}", c.width, c.signedness == Signedness::kSigned ? 's' : 'u');
    if (c.state_kind == IntegralStateKind::kFourState) {
      out += "(four-state)";
    }
    out += "{value=";
    for (std::size_t i = 0; i < c.value_words.size(); ++i) {
      if (i != 0) out += ',';
      out += std::format("0x{:x}", c.value_words[i]);
    }
    if (c.state_kind == IntegralStateKind::kFourState) {
      out += ", state=";
      for (std::size_t i = 0; i < c.state_words.size(); ++i) {
        if (i != 0) out += ',';
        out += std::format("0x{:x}", c.state_words[i]);
      }
    }
    out += "}";
    return out;
  }

  static auto FormatPrimary(const Primary& p) -> std::string {
    return std::visit(
        Overloaded{
            [](const IntegerLiteral& lit) -> std::string {
              return std::format(
                  "IntegerLiteral(base={}, unsized={}, {})",
                  FormatLiteralBase(lit.base), lit.declared_unsized,
                  FormatIntegralConstant(lit.value));
            },
            [](const StringLiteral& lit) -> std::string {
              return std::format("StringLiteral(\"{}\")", lit.value);
            },
            [](const TimeLiteral& lit) -> std::string {
              return std::format(
                  "TimeLiteral(value={}, scale={})", lit.value,
                  FormatTimeScale(lit.scale));
            },
            [](const RealLiteral& lit) -> std::string {
              return std::format("RealLiteral({})", lit.value);
            },
            [](const StructuralVarRef& r) -> std::string {
              return std::format(
                  "StructuralVar[{}](hops={})", r.var.value, r.hops.value);
            },
            [](const ProceduralVarRef& r) -> std::string {
              return std::format("ProceduralVar[{}]", r.var.value);
            },
            [](const LoopVarRef& r) -> std::string {
              return std::format(
                  "LoopVar[{}](hops={})", r.loop_var.value, r.hops.value);
            },
        },
        p);
  }

  static auto FormatTimingControlHeader(const TimingControl& tc)
      -> std::string {
    return std::visit(
        Overloaded{
            [](const DelayControl& d) -> std::string {
              return std::format(
                  "DelayControl duration=Expr[{}]", d.duration.value);
            },
            [](const EventControl& e) -> std::string {
              std::string out = "EventControl triggers=[";
              for (std::size_t i = 0; i < e.triggers.size(); ++i) {
                if (i != 0) out += ", ";
                const char* edge = "any";
                switch (e.triggers[i].edge) {
                  case EventEdge::kAnyChange:
                    edge = "any";
                    break;
                  case EventEdge::kPosedge:
                    edge = "posedge";
                    break;
                  case EventEdge::kNegedge:
                    edge = "negedge";
                    break;
                  case EventEdge::kBothEdges:
                    edge = "edge";
                    break;
                }
                out += std::format(
                    "{{signal=Expr[{}] edge={}}}", e.triggers[i].signal.value,
                    edge);
              }
              out += "]";
              return out;
            },
            [](const ImplicitEventControl& ie) -> std::string {
              std::string out = "ImplicitEventControl sensitivity=[";
              for (std::size_t i = 0; i < ie.sensitivity_list.size(); ++i) {
                if (i != 0) out += ", ";
                const auto& r = ie.sensitivity_list[i];
                out += std::format(
                    "{{hops={} var=StructuralVar[{}] bits=[{}:{}]}}",
                    r.ref.hops.value, r.ref.var.value, r.bit_range.first,
                    r.bit_range.second);
              }
              out += "]";
              return out;
            },
            [](const NamedEventControl& n) -> std::string {
              return std::format(
                  "NamedEventControl event=Expr[{}]", n.event.value);
            },
        },
        tc);
  }

  static auto FormatConversionKind(ConversionKind k) -> std::string_view {
    switch (k) {
      case ConversionKind::kImplicit:
        return "implicit";
      case ConversionKind::kPropagated:
        return "propagated";
      case ConversionKind::kStreamingConcat:
        return "streaming-concat";
      case ConversionKind::kExplicit:
        return "explicit";
      case ConversionKind::kBitstreamCast:
        return "bitstream-cast";
    }
    throw InternalError("HirDumper::FormatConversionKind: unknown kind");
  }

  [[nodiscard]] auto FormatSubroutineRef(const SubroutineRef& callee) const
      -> std::string {
    return std::visit(
        Overloaded{
            [this](const StructuralSubroutineRef& u) -> std::string {
              const auto& owner = ResolveScope(u.hops);
              const auto& decl = owner.GetStructuralSubroutine(u.subroutine);
              return std::format(
                  "StructuralSubroutine[{}](hops={}) \"{}\"",
                  u.subroutine.value, u.hops.value, decl.name);
            },
            [](const SystemSubroutineRef& s) -> std::string {
              const auto& desc = support::LookupSystemSubroutine(s.id);
              return std::format(
                  "SystemSubroutine[{}] \"{}\"", s.id.value, desc.name);
            },
            [](const BuiltinMethodRef& b) -> std::string {
              return std::format(
                  "BuiltinMethod \"{}\"", FormatBuiltinMethodKind(b.kind));
            },
        },
        callee);
  }

  [[nodiscard]] auto ResolveScope(StructuralHops hops) const
      -> const StructuralScope& {
    if (hops.value >= scope_stack_.size()) {
      throw InternalError(
          "HirDumper::ResolveScope: StructuralHops out of range");
    }
    return *scope_stack_[scope_stack_.size() - 1 - hops.value];
  }

  static auto FormatInsideExprNode(const InsideExpr& in) -> std::string {
    std::string items;
    for (std::size_t i = 0; i < in.items.size(); ++i) {
      if (i != 0) items += ", ";
      items += std::visit(
          Overloaded{
              [](const ExprId& id) {
                return std::format("Expr[{}]", id.value);
              },
              [](const InsideRangePair& r) {
                return std::format(
                    "[Expr[{}]:Expr[{}]]", r.lo.value, r.hi.value);
              },
          },
          in.items[i]);
    }
    return std::format(
        "InsideExpr lhs=Expr[{}] items=[{}]", in.lhs.value, items);
  }

  [[nodiscard]] auto FormatExpr(const Expr& e) const -> std::string {
    std::string formatted = std::visit(
        Overloaded{
            [](const PrimaryExpr& p) -> std::string {
              return FormatPrimary(p.data);
            },
            [](const UnaryExpr& u) -> std::string {
              return std::format(
                  "UnaryExpr op={} operand=Expr[{}]", FormatUnaryOp(u.op),
                  u.operand.value);
            },
            [](const BinaryExpr& b) -> std::string {
              return std::format(
                  "BinaryExpr op={} lhs=Expr[{}] rhs=Expr[{}]",
                  FormatBinaryOp(b.op), b.lhs.value, b.rhs.value);
            },
            [](const ConditionalExpr& c) -> std::string {
              return std::format(
                  "ConditionalExpr cond=Expr[{}] then=Expr[{}] else=Expr[{}]",
                  c.condition.value, c.then_value.value, c.else_value.value);
            },
            [](const AssignExpr& a) -> std::string {
              const auto* kind_str = a.kind == AssignKind::kNonBlocking
                                         ? "nonblocking"
                                         : "blocking";
              return std::format(
                  "AssignExpr kind={} lhs={} rhs=Expr[{}]", kind_str,
                  FormatLvalue(a.lhs), a.rhs.value);
            },
            [this](const CallExpr& c) -> std::string {
              std::string args;
              for (std::size_t i = 0; i < c.arguments.size(); ++i) {
                if (i != 0) {
                  args += ", ";
                }
                args += std::format("Expr[{}]", c.arguments[i].value);
              }
              return std::format(
                  "CallExpr callee={} args=[{}]", FormatSubroutineRef(c.callee),
                  args);
            },
            [](const ConversionExpr& cv) -> std::string {
              return std::format(
                  "ConversionExpr kind={} operand=Expr[{}]",
                  FormatConversionKind(cv.kind), cv.operand.value);
            },
            [](const InsideExpr& in) -> std::string {
              return FormatInsideExprNode(in);
            },
            [](const ElementSelectExpr& sel) -> std::string {
              return std::format(
                  "ElementSelectExpr base=Expr[{}] index=Expr[{}]",
                  sel.base_value.value, sel.index.value);
            },
            [](const RangeSelectExpr& sel) -> std::string {
              return std::visit(
                  Overloaded{
                      [&](const RangeConstantBounds& b) -> std::string {
                        return std::format(
                            "RangeSelectExpr base=Expr[{}] kind=Constant "
                            "msb=Expr[{}] lsb=Expr[{}]",
                            sel.base_value.value, b.msb_expr.value,
                            b.lsb_expr.value);
                      },
                      [&](const RangeIndexedUpBounds& b) -> std::string {
                        return std::format(
                            "RangeSelectExpr base=Expr[{}] kind=IndexedUp "
                            "base_index=Expr[{}] width=Expr[{}]",
                            sel.base_value.value, b.base_index.value,
                            b.width.value);
                      },
                      [&](const RangeIndexedDownBounds& b) -> std::string {
                        return std::format(
                            "RangeSelectExpr base=Expr[{}] kind=IndexedDown "
                            "base_index=Expr[{}] width=Expr[{}]",
                            sel.base_value.value, b.base_index.value,
                            b.width.value);
                      },
                  },
                  sel.bounds);
            },
        },
        e.data);
    return std::format("type=Type[{}] {}", e.type.value, formatted);
  }

  [[nodiscard]] auto FormatProcExpr(const Process& p, ExprId id) const
      -> std::string {
    return FormatExpr(p.exprs.at(id.value));
  }

  [[nodiscard]] auto FormatScopeExpr(const StructuralScope& s, ExprId id) const
      -> std::string {
    return FormatExpr(s.GetExpr(id));
  }

  void DumpUnit(const ModuleUnit& u) {
    unit_ = &u;
    Line(std::format("ModuleUnit \"{}\"", u.name));
    Indent();

    Line("Types:");
    Indent();
    for (std::size_t i = 0; i < u.types.size(); ++i) {
      Line(std::format("[{}] {}", i, FormatType(u.types[i])));
    }
    Dedent();

    Line("Root:");
    Indent();
    DumpScope(u.root_scope);
    Dedent();

    Dedent();
  }

  void DumpScope(const StructuralScope& s) {
    scope_stack_.push_back(&s);
    Line("Scope:");
    Indent();
    for (std::size_t i = 0; i < s.structural_vars.size(); ++i) {
      const auto& v = s.structural_vars[i];
      std::string init_suffix;
      if (v.initializer.has_value()) {
        init_suffix = std::format(" init=Expr[{}]", v.initializer->value);
      }
      Line(
          std::format(
              "StructuralVar[{}] \"{}\" : Type[{}]{}", i, v.name, v.type.value,
              init_suffix));
    }
    for (std::size_t i = 0; i < s.loop_var_decls.size(); ++i) {
      const auto& lv = s.loop_var_decls[i];
      Line(
          std::format(
              "LoopVarDecl[{}] \"{}\" : Type[{}]", i, lv.name, lv.type.value));
    }
    for (std::size_t i = 0; i < s.structural_subroutines.size(); ++i) {
      const auto& d = s.structural_subroutines[i];
      Line(
          std::format(
              "StructuralSubroutine[{}] {} \"{}\" : Type[{}]", i,
              d.kind == SubroutineKind::kTask ? "task" : "function", d.name,
              d.result_type.value));
    }
    if (!s.exprs.empty()) {
      Line("Exprs:");
      Indent();
      for (std::size_t i = 0; i < s.exprs.size(); ++i) {
        Line(std::format("Expr[{}] {}", i, FormatExpr(s.exprs[i])));
      }
      Dedent();
    }
    for (const auto& p : s.processes) {
      DumpProcess(p);
    }
    for (const auto& g : s.generates) {
      DumpGenerate(s, g);
    }
    Dedent();
    scope_stack_.pop_back();
  }

  void DumpProcess(const Process& p) {
    switch (p.kind) {
      case ProcessKind::kInitial:
        Line("Process (Initial)");
        break;
      case ProcessKind::kFinal:
        Line("Process (Final)");
        break;
      case ProcessKind::kAlways:
        Line("Process (Always)");
        break;
      case ProcessKind::kAlwaysComb:
        Line("Process (AlwaysComb)");
        break;
      case ProcessKind::kAlwaysLatch:
        Line("Process (AlwaysLatch)");
        break;
      case ProcessKind::kAlwaysFf:
        Line("Process (AlwaysFf)");
        break;
    }
    Indent();
    if (!p.procedural_vars.empty()) {
      Line("ProceduralVars:");
      Indent();
      for (std::size_t i = 0; i < p.procedural_vars.size(); ++i) {
        const auto& lv = p.procedural_vars[i];
        Line(
            std::format(
                "ProceduralVar[{}] \"{}\" : Type[{}]", i, lv.name,
                lv.type.value));
      }
      Dedent();
    }
    if (!p.implicit_sensitivity_list.empty()) {
      Line("ImplicitSensitivityList:");
      Indent();
      for (const auto& r : p.implicit_sensitivity_list) {
        Line(
            std::format(
                "StructuralVarRef hops={} var=StructuralVar[{}] bits=[{}:{}]",
                r.ref.hops.value, r.ref.var.value, r.bit_range.first,
                r.bit_range.second));
      }
      Dedent();
    }
    if (!p.exprs.empty()) {
      Line("Exprs:");
      Indent();
      for (std::size_t i = 0; i < p.exprs.size(); ++i) {
        Line(std::format("Expr[{}] {}", i, FormatExpr(p.exprs[i])));
      }
      Dedent();
    }
    DumpStmt(p, p.root_stmt);
    Dedent();
  }

  void DumpVarDeclStmtNode(const Process& p, StmtId id, const VarDeclStmt& v) {
    Line(
        std::format(
            "Stmt[{}] VarDeclStmt var=ProceduralVar[{}]", id.value,
            v.var.value));
    if (v.init.has_value()) {
      Indent();
      Line(
          std::format(
              "init: Expr[{}] {}", v.init->value, FormatProcExpr(p, *v.init)));
      Dedent();
    }
  }

  void DumpExprStmtNode(const Process& p, StmtId id, const ExprStmt& e) {
    Line(
        std::format("Stmt[{}] ExprStmt expr=Expr[{}]", id.value, e.expr.value));
    Indent();
    Line(std::format("Expr[{}] {}", e.expr.value, FormatProcExpr(p, e.expr)));
    Dedent();
  }

  void DumpBlockStmtNode(const Process& p, StmtId id, const BlockStmt& b) {
    Line(
        std::format(
            "Stmt[{}] BlockStmt (count={})", id.value, b.statements.size()));
    Indent();
    for (const auto child : b.statements) {
      DumpStmt(p, child);
    }
    Dedent();
  }

  void DumpIfStmtNode(const Process& p, StmtId id, const IfStmt& i) {
    Line(
        std::format(
            "Stmt[{}] IfStmt{} cond=Expr[{}]", id.value,
            FormatUniquePriorityCheck(i.check), i.condition.value));
    Indent();
    Line(
        std::format(
            "Expr[{}] {}", i.condition.value, FormatProcExpr(p, i.condition)));
    Line("then:");
    Indent();
    DumpStmt(p, i.then_stmt);
    Dedent();
    if (i.else_stmt.has_value()) {
      Line("else:");
      Indent();
      DumpStmt(p, *i.else_stmt);
      Dedent();
    }
    Dedent();
  }

  void DumpCaseStmtNode(const Process& p, StmtId id, const CaseStmt& c) {
    Line(
        std::format(
            "Stmt[{}] CaseStmt{} cond=Expr[{}] (items={})", id.value,
            FormatUniquePriorityCheck(c.check), c.condition.value,
            c.items.size()));
    Indent();
    Line(
        std::format(
            "Expr[{}] {}", c.condition.value, FormatProcExpr(p, c.condition)));
    for (std::size_t k = 0; k < c.items.size(); ++k) {
      const auto& item = c.items[k];
      std::string labels_str;
      for (std::size_t m = 0; m < item.labels.size(); ++m) {
        if (m != 0) labels_str += ", ";
        labels_str += std::format("Expr[{}]", item.labels[m].value);
      }
      Line(std::format("item[{}] labels=[{}]", k, labels_str));
      Indent();
      for (const hir::ExprId label_id : item.labels) {
        Line(
            std::format(
                "Expr[{}] {}", label_id.value, FormatProcExpr(p, label_id)));
      }
      Line("stmt:");
      Indent();
      DumpStmt(p, item.stmt);
      Dedent();
      Dedent();
    }
    if (c.default_stmt.has_value()) {
      Line("default:");
      Indent();
      DumpStmt(p, *c.default_stmt);
      Dedent();
    }
    Dedent();
  }

  void DumpForStmtNode(const Process& p, StmtId id, const ForStmt& f) {
    Line(
        std::format(
            "Stmt[{}] ForStmt (init={}, step={})", id.value, f.init.size(),
            f.step.size()));
    Indent();
    for (std::size_t k = 0; k < f.init.size(); ++k) {
      std::visit(
          Overloaded{
              [&](const ForInitDecl& d) {
                Line(
                    std::format(
                        "init[{}]: Decl var=ProceduralVar[{}]", k,
                        d.var.value));
                if (d.init.has_value()) {
                  Indent();
                  Line(
                      std::format(
                          "Expr[{}] {}", d.init->value,
                          FormatProcExpr(p, *d.init)));
                  Dedent();
                }
              },
              [&](const ForInitExpr& e) {
                Line(
                    std::format(
                        "init[{}]: Expr[{}] {}", k, e.expr.value,
                        FormatProcExpr(p, e.expr)));
              },
          },
          f.init[k]);
    }
    if (f.condition.has_value()) {
      Line(
          std::format(
              "cond: Expr[{}] {}", f.condition->value,
              FormatProcExpr(p, *f.condition)));
    }
    for (std::size_t k = 0; k < f.step.size(); ++k) {
      Line(
          std::format(
              "step[{}]: Expr[{}] {}", k, f.step[k].value,
              FormatProcExpr(p, f.step[k])));
    }
    Line("body:");
    Indent();
    DumpStmt(p, f.body);
    Dedent();
    Dedent();
  }

  void DumpWhileStmtNode(const Process& p, StmtId id, const WhileStmt& w) {
    Line(
        std::format(
            "Stmt[{}] WhileStmt cond=Expr[{}]", id.value, w.condition.value));
    Indent();
    Line(
        std::format(
            "Expr[{}] {}", w.condition.value, FormatProcExpr(p, w.condition)));
    Line("body:");
    Indent();
    DumpStmt(p, w.body);
    Dedent();
    Dedent();
  }

  void DumpRepeatStmtNode(const Process& p, StmtId id, const RepeatStmt& r) {
    Line(
        std::format(
            "Stmt[{}] RepeatStmt count=Expr[{}]", id.value, r.count.value));
    Indent();
    Line(std::format("Expr[{}] {}", r.count.value, FormatProcExpr(p, r.count)));
    Line("body:");
    Indent();
    DumpStmt(p, r.body);
    Dedent();
    Dedent();
  }

  void DumpDoWhileStmtNode(const Process& p, StmtId id, const DoWhileStmt& d) {
    Line(
        std::format(
            "Stmt[{}] DoWhileStmt cond=Expr[{}]", id.value, d.condition.value));
    Indent();
    Line(
        std::format(
            "Expr[{}] {}", d.condition.value, FormatProcExpr(p, d.condition)));
    Line("body:");
    Indent();
    DumpStmt(p, d.body);
    Dedent();
    Dedent();
  }

  void DumpForeverStmtNode(const Process& p, StmtId id, const ForeverStmt& f) {
    Line(std::format("Stmt[{}] ForeverStmt", id.value));
    Indent();
    Line("body:");
    Indent();
    DumpStmt(p, f.body);
    Dedent();
    Dedent();
  }

  void DumpTimedStmtNode(const Process& p, StmtId id, const TimedStmt& t) {
    Line(std::format("Stmt[{}] TimedStmt", id.value));
    Indent();
    Line(std::format("timing: {}", FormatTimingControlHeader(t.timing)));
    Indent();
    std::visit(
        Overloaded{
            [&](const DelayControl& d) {
              Line(
                  std::format(
                      "Expr[{}] {}", d.duration.value,
                      FormatProcExpr(p, d.duration)));
            },
            [](const EventControl&) {},
            [](const ImplicitEventControl&) {},
            [](const NamedEventControl&) {},
        },
        t.timing);
    Dedent();
    Line("stmt:");
    Indent();
    DumpStmt(p, t.stmt);
    Dedent();
    Dedent();
  }

  void DumpStmt(const Process& p, StmtId id) {
    const auto& s = p.stmts.at(id.value);
    std::visit(
        Overloaded{
            [&](const EmptyStmt&) {
              Line(std::format("Stmt[{}] EmptyStmt", id.value));
            },
            [&](const VarDeclStmt& v) { DumpVarDeclStmtNode(p, id, v); },
            [&](const ExprStmt& e) { DumpExprStmtNode(p, id, e); },
            [&](const BlockStmt& b) { DumpBlockStmtNode(p, id, b); },
            [&](const IfStmt& i) { DumpIfStmtNode(p, id, i); },
            [&](const CaseStmt& c) { DumpCaseStmtNode(p, id, c); },
            [&](const ForStmt& f) { DumpForStmtNode(p, id, f); },
            [&](const WhileStmt& w) { DumpWhileStmtNode(p, id, w); },
            [&](const RepeatStmt& r) { DumpRepeatStmtNode(p, id, r); },
            [&](const DoWhileStmt& d) { DumpDoWhileStmtNode(p, id, d); },
            [&](const ForeverStmt& f) { DumpForeverStmtNode(p, id, f); },
            [&](const BreakStmt&) {
              Line(std::format("Stmt[{}] BreakStmt", id.value));
            },
            [&](const ContinueStmt&) {
              Line(std::format("Stmt[{}] ContinueStmt", id.value));
            },
            [&](const TimedStmt& t) { DumpTimedStmtNode(p, id, t); },
            [&](const EventTriggerStmt& et) {
              Line(
                  std::format(
                      "Stmt[{}] EventTriggerStmt event=Expr[{}]", id.value,
                      et.event.value));
            },
            [&](const WaitStmt& w) {
              std::string sens = "sensitivity=[";
              for (std::size_t i = 0; i < w.sensitivity_list.size(); ++i) {
                if (i != 0) sens += ", ";
                const auto& r = w.sensitivity_list[i];
                sens += std::format(
                    "{{hops={} var=StructuralVar[{}] bits=[{}:{}]}}",
                    r.ref.hops.value, r.ref.var.value, r.bit_range.first,
                    r.bit_range.second);
              }
              sens += "]";
              Line(
                  std::format(
                      "Stmt[{}] WaitStmt cond=Expr[{}] {}", id.value,
                      w.cond.value, sens));
              Indent();
              Line("body:");
              Indent();
              DumpStmt(p, w.body);
              Dedent();
              Dedent();
            },
        },
        s.data);
  }

  void DumpIfGenerateNode(
      const StructuralScope& owner, const Generate& g, const IfGenerate& ig) {
    Line(
        std::format(
            "Generate IfGenerate cond={}",
            FormatScopeExpr(owner, ig.condition)));
    Indent();
    Line("then_scope:");
    Indent();
    DumpScope(g.child_scopes.at(ig.then_scope.value));
    Dedent();
    if (ig.else_scope.has_value()) {
      Line("else_scope:");
      Indent();
      DumpScope(g.child_scopes.at(ig.else_scope->value));
      Dedent();
    } else {
      Line("else_scope: <none>");
    }
    Dedent();
  }

  void DumpCaseGenerateNode(
      const StructuralScope& owner, const Generate& g, const CaseGenerate& cg) {
    Line(
        std::format(
            "Generate CaseGenerate cond={}",
            FormatScopeExpr(owner, cg.condition)));
    Indent();
    for (std::size_t i = 0; i < cg.items.size(); ++i) {
      const auto& item = cg.items[i];
      std::string labels;
      for (std::size_t j = 0; j < item.labels.size(); ++j) {
        if (j != 0) labels += ", ";
        labels += FormatScopeExpr(owner, item.labels[j]);
      }
      Line(std::format("item[{}] labels=[{}]", i, labels));
      Indent();
      DumpScope(g.child_scopes.at(item.scope.value));
      Dedent();
    }
    if (cg.default_scope.has_value()) {
      Line("default_scope:");
      Indent();
      DumpScope(g.child_scopes.at(cg.default_scope->value));
      Dedent();
    } else {
      Line("default_scope: <none>");
    }
    Dedent();
  }

  void DumpLoopGenerateNode(const Generate& g, const LoopGenerate& lg) {
    Line(
        std::format(
            "Generate LoopGenerate loop_var=LoopVar[{}] initial=Expr[{}] "
            "stop=Expr[{}] iter=Expr[{}]",
            lg.loop_var.value, lg.initial.value, lg.stop.value, lg.iter.value));
    Indent();
    Line("scope:");
    Indent();
    DumpScope(g.child_scopes.at(lg.scope.value));
    Dedent();
    Dedent();
  }

  void DumpGenerate(const StructuralScope& owner, const Generate& g) {
    std::visit(
        Overloaded{
            [&](const IfGenerate& ig) { DumpIfGenerateNode(owner, g, ig); },
            [&](const CaseGenerate& cg) { DumpCaseGenerateNode(owner, g, cg); },
            [&](const LoopGenerate& lg) { DumpLoopGenerateNode(g, lg); },
        },
        g.data);
  }

  std::string out_;
  int indent_ = 0;
  std::vector<const StructuralScope*> scope_stack_;
  const ModuleUnit* unit_ = nullptr;
};

}  // namespace

auto DumpHir(const std::vector<ModuleUnit>& units) -> std::string {
  HirDumper dumper;
  return dumper.Dump(units);
}

}  // namespace lyra::hir
