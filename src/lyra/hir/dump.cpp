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
#include "lyra/hir/continuous_assign.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/module_unit.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/hir/structural_data_object.hpp"
#include "lyra/hir/structural_hops.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/unary_op.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::hir {

namespace {

auto ForkJoinModeLabel(JoinMode mode) -> std::string_view {
  switch (mode) {
    case JoinMode::kAll:
      return "join";
    case JoinMode::kAny:
      return "join_any";
    case JoinMode::kNone:
      return "join_none";
  }
  return "join";
}

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

  static auto FormatPackedArray(const PackedArrayType& p) -> std::string {
    return std::format(
        "PackedArray(dim=[{}:{}], elem=Type[{}], signed={}, form={})",
        p.dim.left, p.dim.right, p.element_type.value,
        FormatSignedness(p.signedness), FormatPackedForm(p.form));
  }

  static auto FormatType(const Type& t) -> std::string {
    return std::visit(
        Overloaded{
            [](const ScalarBitType& s) -> std::string {
              return std::format("ScalarBit(atom={})", FormatBitAtom(s.atom));
            },
            [](const PackedArrayType& p) -> std::string {
              return FormatPackedArray(p);
            },
            [](const PackedStructType& s) -> std::string {
              std::string fields;
              for (std::size_t i = 0; i < s.fields.size(); ++i) {
                if (i > 0) fields += ", ";
                fields += std::format(
                    "{}@bit{}+{}:Type[{}]", s.fields[i].name,
                    s.fields[i].bit_offset, s.fields[i].bit_width,
                    s.fields[i].type.value);
              }
              return std::format(
                  "PackedStruct(signed={}, four_state={}, fields=[{}])",
                  FormatSignedness(s.signedness), s.four_state, fields);
            },
            [](const PackedUnionType& u) -> std::string {
              std::string fields;
              for (std::size_t i = 0; i < u.fields.size(); ++i) {
                if (i > 0) fields += ", ";
                fields += std::format(
                    "{}@bit{}+{}:Type[{}]", u.fields[i].name,
                    u.fields[i].bit_offset, u.fields[i].bit_width,
                    u.fields[i].type.value);
              }
              return std::format(
                  "PackedUnion(signed={}, four_state={}, fields=[{}])",
                  FormatSignedness(u.signedness), u.four_state, fields);
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
            [](const UnpackedStructType& s) -> std::string {
              std::string fields;
              for (std::size_t i = 0; i < s.fields.size(); ++i) {
                if (i > 0) fields += ", ";
                fields += std::format(
                    "{}:Type[{}]", s.fields[i].name, s.fields[i].type.value);
              }
              return std::format("UnpackedStruct(fields=[{}])", fields);
            },
            [](const UnpackedUnionType& u) -> std::string {
              std::string fields;
              for (std::size_t i = 0; i < u.fields.size(); ++i) {
                if (i > 0) fields += ", ";
                fields += std::format(
                    "{}:Type[{}]", u.fields[i].name, u.fields[i].type.value);
              }
              return std::format(
                  "UnpackedUnion(tagged={}, fields=[{}])", u.tagged, fields);
            },
            [](const UnpackedArrayType& u) -> std::string {
              return std::format(
                  "UnpackedArray(elem=Type[{}], dim=[{}:{}])",
                  u.element_type.value, u.dim.left, u.dim.right);
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
              return std::format(
                  "AssociativeArray(elem=Type[{}], key=Type[{}])",
                  a.element_type.value, a.key_type.value);
            },
            [](const WildcardIndexType&) -> std::string {
              return "WildcardIndexType";
            },
            [](const StringType&) -> std::string { return "StringType"; },
            [](const EventType&) -> std::string { return "EventType"; },
            [](const RealType&) -> std::string { return "RealType"; },
            [](const ShortRealType&) -> std::string { return "ShortRealType"; },
            [](const RealTimeType&) -> std::string { return "RealTimeType"; },
            [](const ChandleType&) -> std::string { return "ChandleType"; },
            [](const ClassHandleType& c) -> std::string {
              return std::format(
                  "ClassHandleType(class=Class[{}])", c.class_id.value);
            },
            [](const NullType&) -> std::string { return "NullType"; },
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
            [](const NullLiteral&) -> std::string { return "NullLiteral"; },
            [](const DirectMemberRef& r) -> std::string {
              return std::format("DirectMember[{}]", r.var.value);
            },
            [](const ProceduralVarRef& r) -> std::string {
              return std::format("ProceduralVar[{}]", r.var.value);
            },
            [](const ClassPropertyRef& r) -> std::string {
              return std::format("ClassProperty[{}]", r.field_index);
            },
            [](const RoutedRef& r) -> std::string {
              return std::format("RoutedRef[{}]", r.id.value);
            },
            [](const IterationBindingRef& r) -> std::string {
              const char* role = r.role == IterationBindingRole::kElement
                                     ? "Element"
                                     : "Index";
              return std::format("Iteration[{}].{}", r.clause.value, role);
            },
        },
        p);
  }

  static auto FormatReferenceRoute(const ReferenceRoute& ref) -> std::string {
    return std::visit(
        Overloaded{
            [](const DirectMemberRef& r) -> std::string {
              return std::format("var=DirectMember[{}]", r.var.value);
            },
            [](const RoutedRef& r) -> std::string {
              return std::format("routed_ref={}", r.id.value);
            },
        },
        ref);
  }

  static auto FormatFootprint(
      const std::optional<std::pair<std::uint64_t, std::uint64_t>>& footprint)
      -> std::string {
    if (!footprint.has_value()) {
      return "[whole]";
    }
    return std::format("[{}:{}]", footprint->first, footprint->second);
  }

  static auto FormatEventEdge(support::EventEdge edge) -> std::string_view {
    switch (edge) {
      case support::EventEdge::kAnyChange:
        return "any";
      case support::EventEdge::kPosedge:
        return "posedge";
      case support::EventEdge::kNegedge:
        return "negedge";
      case support::EventEdge::kBothEdges:
        return "edge";
    }
    throw InternalError("HirDumper::FormatEventEdge: unknown EventEdge");
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
                out += std::format(
                    "{{signal=Expr[{}] edge={} sensitivity=[",
                    e.triggers[i].signal.value,
                    FormatEventEdge(e.triggers[i].edge));
                for (std::size_t j = 0;
                     j < e.triggers[i].sensitivity_list.size(); ++j) {
                  if (j != 0) out += ", ";
                  const auto& r = e.triggers[i].sensitivity_list[j];
                  out += std::format(
                      "{{{} bits={} edge={}}}", FormatReferenceRoute(r.ref),
                      FormatFootprint(r.footprint),
                      FormatEventEdge(r.edge_kind));
                }
                out += "]}";
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
                    "{{{} bits={} edge={}}}", FormatReferenceRoute(r.ref),
                    FormatFootprint(r.footprint), FormatEventEdge(r.edge_kind));
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
              const auto& decl = owner.structural_subroutines.Get(u.subroutine);
              return std::format(
                  "StructuralSubroutine[{}](hops={}) \"{}\"",
                  u.subroutine.value, u.hops.value, decl.name);
            },
            [](const MethodCallRef& m) -> std::string {
              return std::format(
                  "Method class=Class[{}] index={} recv=Expr[{}]",
                  m.class_id.value, m.method_index, m.receiver.value);
            },
            [](const SystemSubroutineRef& s) -> std::string {
              const auto& desc = support::LookupSystemSubroutine(s.id);
              return std::format(
                  "SystemSubroutine[{}] \"{}\"", s.id.value, desc.name);
            },
            [](const BuiltinMethodRef& b) -> std::string {
              return std::format(
                  "BuiltinFn \"{}\"", support::BuiltinFnName(b.method));
            },
            [this](const ForeignImportRef& f) -> std::string {
              const auto& owner = ResolveScope(f.hops);
              const auto& decl = owner.foreign_imports.Get(f.id);
              return std::format(
                  "ForeignImport[{}](hops={}) \"{}\"", f.id.value, f.hops.value,
                  decl.name);
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
              const std::string op_str =
                  a.compound_op.has_value()
                      ? std::format(" op={}", FormatBinaryOp(*a.compound_op))
                      : std::string{};
              return std::format(
                  "AssignExpr kind={}{} lhs=Expr[{}] rhs=Expr[{}]", kind_str,
                  op_str, a.lhs.value, a.rhs.value);
            },
            [](const IncDecExpr& inc) -> std::string {
              const auto* op_str = [&] {
                switch (inc.op) {
                  case IncDecOp::kPreInc:
                    return "PreInc";
                  case IncDecOp::kPostInc:
                    return "PostInc";
                  case IncDecOp::kPreDec:
                    return "PreDec";
                  case IncDecOp::kPostDec:
                    return "PostDec";
                }
                return "?";
              }();
              return std::format(
                  "IncDecExpr op={} target=Expr[{}]", op_str, inc.target.value);
            },
            [this](const CallExpr& c) -> std::string {
              std::string args;
              for (std::size_t i = 0; i < c.arguments.size(); ++i) {
                if (i != 0) {
                  args += ", ";
                }
                if (c.arguments[i].has_value()) {
                  args += std::format("Expr[{}]", c.arguments[i]->value);
                } else {
                  args += "<elided>";
                }
              }
              std::string with_text;
              if (c.with_clause.has_value()) {
                with_text = std::format(
                    " with={{element=\"{}\", expr=Expr[{}]}}",
                    c.with_clause->element_name, c.with_clause->expr.value);
              }
              return std::format(
                  "CallExpr callee={} args=[{}]{}",
                  FormatSubroutineRef(c.callee), args, with_text);
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
                            "left=Expr[{}] right=Expr[{}]",
                            sel.base_value.value, b.left_bound.value,
                            b.right_bound.value);
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
            [](const MemberAccessExpr& sel) -> std::string {
              return std::format(
                  "MemberAccessExpr base=Expr[{}] field={}",
                  sel.base_value.value, sel.field_index);
            },
            [](const ConcatExpr& c) -> std::string {
              std::string operands;
              for (std::size_t i = 0; i < c.operands.size(); ++i) {
                if (i != 0) {
                  operands += ", ";
                }
                operands += std::format("Expr[{}]", c.operands[i].value);
              }
              return std::format("ConcatExpr operands=[{}]", operands);
            },
            [](const ReplicationExpr& r) -> std::string {
              return std::format(
                  "ReplicationExpr count=Expr[{}] concat=Expr[{}]",
                  r.count.value, r.concat.value);
            },
            [](const AssignmentPatternExpr& a) -> std::string {
              std::string elements;
              for (std::size_t i = 0; i < a.elements.size(); ++i) {
                if (i != 0) {
                  elements += ", ";
                }
                elements += std::format("Expr[{}]", a.elements[i].value);
              }
              return std::format(
                  "AssignmentPatternExpr elements=[{}]", elements);
            },
            [](const AssignmentPatternReplicationExpr& a) -> std::string {
              std::string items;
              for (std::size_t i = 0; i < a.items.size(); ++i) {
                if (i != 0) {
                  items += ", ";
                }
                items += std::format("Expr[{}]", a.items[i].value);
              }
              return std::format(
                  "AssignmentPatternReplicationExpr count=Expr[{}] items=[{}]",
                  a.count.value, items);
            },
            [](const DynamicArrayNewExpr& n) -> std::string {
              if (n.initializer.has_value()) {
                return std::format(
                    "DynamicArrayNewExpr size=Expr[{}] initializer=Expr[{}]",
                    n.size.value, n.initializer->value);
              }
              return std::format(
                  "DynamicArrayNewExpr size=Expr[{}]", n.size.value);
            },
            [](const ClassNewExpr& n) -> std::string {
              std::string args;
              for (std::size_t i = 0; i < n.arguments.size(); ++i) {
                if (i != 0) args += ", ";
                args += std::format("Expr[{}]", n.arguments[i].value);
              }
              return std::format(
                  "ClassNewExpr class=Class[{}] args=[{}]", n.class_id.value,
                  args);
            },
            [](const AssociativeAssignmentPatternExpr& a) -> std::string {
              std::string entries;
              for (std::size_t i = 0; i < a.entries.size(); ++i) {
                if (i != 0) {
                  entries += ", ";
                }
                entries += std::format(
                    "Expr[{}]:Expr[{}]", a.entries[i].key.value,
                    a.entries[i].value.value);
              }
              if (a.default_value.has_value()) {
                return std::format(
                    "AssociativeAssignmentPatternExpr entries=[{}] "
                    "default=Expr[{}]",
                    entries, a.default_value->value);
              }
              return std::format(
                  "AssociativeAssignmentPatternExpr entries=[{}]", entries);
            },
        },
        e.data);
    return std::format("type=Type[{}] {}", e.type.value, formatted);
  }

  [[nodiscard]] auto FormatProcExpr(const ProceduralBody& p, ExprId id) const
      -> std::string {
    return FormatExpr(p.exprs.Get(id));
  }

  [[nodiscard]] auto FormatScopeExpr(const StructuralScope& s, ExprId id) const
      -> std::string {
    return FormatExpr(s.exprs.Get(id));
  }

  void DumpUnit(const ModuleUnit& u) {
    unit_ = &u;
    Line(std::format("ModuleUnit \"{}\"", u.name));
    Indent();

    Line("Types:");
    Indent();
    for (std::size_t i = 0; i < u.types.size(); ++i) {
      Line(
          std::format(
              "[{}] {}", i,
              FormatType(u.types.Get(TypeId{static_cast<std::uint32_t>(i)}))));
    }
    Dedent();

    if (u.classes.size() > 0) {
      Line("Classes:");
      Indent();
      for (std::size_t i = 0; i < u.classes.size(); ++i) {
        const ClassId id{static_cast<std::uint32_t>(i)};
        if (!u.classes.IsDefined(id)) {
          Line(std::format("[{}] <declared>", i));
          continue;
        }
        DumpClass(i, u.classes.Get(id));
      }
      Dedent();
    }

    Line("Root:");
    Indent();
    DumpScope(u.root_scope);
    Dedent();

    Dedent();
  }

  void DumpClass(std::size_t index, const ClassDecl& c) {
    Line(std::format("[{}] class \"{}\"", index, c.name));
    Indent();
    for (const auto& field : c.fields) {
      if (field.initializer.has_value()) {
        Line(
            std::format(
                "{}:Type[{}] = Expr[{}]", field.name, field.type.value,
                field.initializer->value));
      } else {
        Line(std::format("{}:Type[{}]", field.name, field.type.value));
      }
    }
    for (std::size_t i = 0; i < c.methods.size(); ++i) {
      DumpSubroutine("Method", i, c.methods[i]);
    }
    DumpSubroutine("Constructor", 0, c.constructor);
    Dedent();
  }

  void DumpScope(const StructuralScope& s) {
    scope_stack_.push_back(&s);
    Line("Scope:");
    Indent();
    for (std::size_t i = 0; i < s.structural_data_objects.size(); ++i) {
      const auto& v = s.structural_data_objects.Get(
          StructuralDataObjectId{static_cast<std::uint32_t>(i)});
      std::string suffix;
      if (const auto* var = std::get_if<StructuralVariableDecl>(&v.kind)) {
        if (var->reference.has_value()) {
          suffix += *var->reference == ReferenceBinding::kConstRef
                        ? " const ref"
                        : " ref";
        }
        if (var->initializer.has_value()) {
          suffix += std::format(" init=Expr[{}]", var->initializer->value);
        }
      } else {
        const auto& net = std::get<StructuralNetDecl>(v.kind);
        suffix += net.net_type == NetType::kWire ? " net=wire" : " net=tri";
      }
      Line(
          std::format(
              "StructuralDataObject[{}] \"{}\" : Type[{}]{}", i, v.name,
              v.type.value, suffix));
    }
    for (std::size_t i = 0; i < s.structural_subroutines.size(); ++i) {
      DumpSubroutine(
          "StructuralSubroutine", i,
          s.structural_subroutines.Get(
              StructuralSubroutineId{static_cast<std::uint32_t>(i)}));
    }
    for (std::size_t i = 0; i < s.foreign_imports.size(); ++i) {
      DumpForeignImport(
          i, s.foreign_imports.Get(
                 ForeignImportId{static_cast<std::uint32_t>(i)}));
    }
    if (!s.exprs.empty()) {
      Line("Exprs:");
      Indent();
      for (std::size_t i = 0; i < s.exprs.size(); ++i) {
        Line(
            std::format(
                "Expr[{}] {}", i,
                FormatExpr(
                    s.exprs.Get(ExprId{static_cast<std::uint32_t>(i)}))));
      }
      Dedent();
    }
    for (const auto& p : s.processes) {
      DumpProcess(p);
    }
    for (const auto& ca : s.continuous_assigns) {
      DumpContinuousAssign(ca);
    }
    for (const auto& g : s.generates) {
      DumpGenerate(g);
    }
    for (std::size_t i = 0; i < s.instance_members.size(); ++i) {
      const auto& im = s.instance_members.Get(
          InstanceMemberId{static_cast<std::uint32_t>(i)});
      std::string array_suffix;
      for (const auto dim : im.array_dims) {
        array_suffix += std::format("[{}]", dim);
      }
      Line(
          std::format(
              "InstanceMember[{}] \"{}\"{} : {}", i, im.instance_name,
              array_suffix, im.target_unit));
    }
    for (std::size_t i = 0; i < s.port_connections.size(); ++i) {
      const auto& pc = s.port_connections[i];
      std::string_view direction = "ref";
      switch (pc.direction) {
        case PortDirection::kInput:
          direction = "input";
          break;
        case PortDirection::kOutput:
          direction = "output";
          break;
        case PortDirection::kRef:
          break;
      }
      const std::string endpoint = std::visit(
          Overloaded{
              [](const PortCellEndpoint& c) -> std::string {
                return std::format("cell=Expr[{}]", c.cell.value);
              },
              [](const RoutedPathRecipe& r) -> std::string {
                return std::format("alias_to:Type[{}]", r.type.value);
              }},
          pc.endpoint);
      Line(
          std::format(
              "PortConnection[{}] {} {} peer=Expr[{}]", i, direction, endpoint,
              pc.peer.value));
    }
    Dedent();
    scope_stack_.pop_back();
  }

  void DumpSubroutine(
      std::string_view label, std::size_t index, const SubroutineDecl& d) {
    Line(
        std::format(
            "{}[{}] {} \"{}\" : Type[{}]", label, index,
            d.kind == SubroutineKind::kTask ? "task" : "function", d.name,
            d.result_type.value));
    Indent();
    for (std::size_t i = 0; i < d.params.size(); ++i) {
      const auto& param = d.params[i];
      Line(
          std::format(
              "Param[{}] {} var=ProceduralVar[{}]", i,
              FormatParamDirection(param.direction), param.var.value));
    }
    if (d.result_var.has_value()) {
      Line(std::format("Result var=ProceduralVar[{}]", d.result_var->value));
    }
    DumpProceduralBody(d.body);
    Dedent();
  }

  void DumpForeignImport(std::size_t index, const ForeignImportDecl& fi) {
    Line(
        std::format(
            R"(ForeignImport[{}] function "{}" c_name="{}"{} ret={})", index,
            fi.name, fi.foreign_name, fi.is_pure ? " pure" : "",
            support::DpiScalarAbiName(fi.ret_abi)));
    Indent();
    for (std::size_t i = 0; i < fi.params.size(); ++i) {
      Line(
          std::format(
              "Param[{}] {} {} : Type[{}]", i,
              support::DpiDirectionName(fi.params[i].direction),
              support::DpiCarrierName(fi.params[i].carrier),
              fi.params[i].sv_type.value));
    }
    Dedent();
  }

  [[nodiscard]] static auto FormatParamDirection(ParamDirection dir)
      -> std::string_view {
    switch (dir) {
      case ParamDirection::kInput:
        return "input";
      case ParamDirection::kOutput:
        return "output";
      case ParamDirection::kInOut:
        return "inout";
      case ParamDirection::kRef:
        return "ref";
      case ParamDirection::kConstRef:
        return "const ref";
    }
    throw InternalError("FormatParamDirection: unknown hir::ParamDirection");
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
    if (!p.implicit_sensitivity_list.empty()) {
      Line("ImplicitSensitivityList:");
      Indent();
      for (const auto& r : p.implicit_sensitivity_list) {
        Line(
            std::format(
                "{} bits={}", FormatReferenceRoute(r.ref),
                FormatFootprint(r.footprint)));
      }
      Dedent();
    }
    DumpProceduralBody(p.body);
    Dedent();
  }

  void DumpProceduralBody(const ProceduralBody& body) {
    if (!body.procedural_vars.empty()) {
      Line("ProceduralVars:");
      Indent();
      for (std::size_t i = 0; i < body.procedural_vars.size(); ++i) {
        const auto& lv = body.procedural_vars.Get(
            ProceduralVarId{static_cast<std::uint32_t>(i)});
        Line(
            std::format(
                "ProceduralVar[{}] \"{}\" : Type[{}]{}{}", i, lv.name,
                lv.type.value,
                lv.lifetime == VariableLifetime::kStatic ? " static" : "",
                lv.lifetime_extended ? " lifetime-extended" : ""));
      }
      Dedent();
    }
    if (!body.exprs.empty()) {
      Line("Exprs:");
      Indent();
      for (std::size_t i = 0; i < body.exprs.size(); ++i) {
        Line(
            std::format(
                "Expr[{}] {}", i,
                FormatExpr(
                    body.exprs.Get(ExprId{static_cast<std::uint32_t>(i)}))));
      }
      Dedent();
    }
    DumpStmt(body, body.root_stmt);
  }

  void DumpContinuousAssign(const ContinuousAssign& ca) {
    Line(
        std::format(
            "ContinuousAssign lhs=Expr[{}] rhs=Expr[{}]", ca.lhs.value,
            ca.rhs.value));
    Indent();
    if (!ca.sensitivity_list.empty()) {
      Line("SensitivityList:");
      Indent();
      for (const auto& r : ca.sensitivity_list) {
        Line(
            std::format(
                "{} bits={}", FormatReferenceRoute(r.ref),
                FormatFootprint(r.footprint)));
      }
      Dedent();
    }
    Dedent();
  }

  void DumpVarDeclStmtNode(
      const ProceduralBody& p, StmtId id, const VarDeclStmt& v) {
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

  void DumpExprStmtNode(const ProceduralBody& p, StmtId id, const ExprStmt& e) {
    Line(
        std::format("Stmt[{}] ExprStmt expr=Expr[{}]", id.value, e.expr.value));
    Indent();
    Line(std::format("Expr[{}] {}", e.expr.value, FormatProcExpr(p, e.expr)));
    Dedent();
  }

  void DumpBlockStmtNode(
      const ProceduralBody& p, StmtId id, const BlockStmt& b) {
    Line(
        std::format(
            "Stmt[{}] BlockStmt (count={})", id.value, b.statements.size()));
    Indent();
    for (const auto child : b.statements) {
      DumpStmt(p, child);
    }
    Dedent();
  }

  void DumpForkStmtNode(const ProceduralBody& p, StmtId id, const ForkStmt& f) {
    Line(
        std::format(
            "Stmt[{}] ForkStmt {} (locals={}, branches={})", id.value,
            ForkJoinModeLabel(f.mode), f.locals.size(), f.branches.size()));
    Indent();
    for (const auto local : f.locals) {
      DumpStmt(p, local);
    }
    for (const auto branch : f.branches) {
      DumpStmt(p, branch);
    }
    Dedent();
  }

  void DumpIfStmtNode(const ProceduralBody& p, StmtId id, const IfStmt& i) {
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

  void DumpCaseStmtNode(const ProceduralBody& p, StmtId id, const CaseStmt& c) {
    std::string_view kind_tag;
    switch (c.condition_kind) {
      case CaseCondition::kNormal:
        kind_tag = "";
        break;
      case CaseCondition::kWildcardJustZ:
        kind_tag = "z";
        break;
      case CaseCondition::kWildcardXOrZ:
        kind_tag = "x";
        break;
    }
    Line(
        std::format(
            "Stmt[{}] CaseStmt{}{} cond=Expr[{}] (items={})", id.value,
            kind_tag, FormatUniquePriorityCheck(c.check), c.condition.value,
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

  void DumpCaseInsideStmtNode(
      const ProceduralBody& p, StmtId id, const CaseInsideStmt& c) {
    Line(
        std::format(
            "Stmt[{}] CaseInsideStmt{} cond=Expr[{}] (items={})", id.value,
            FormatUniquePriorityCheck(c.check), c.condition.value,
            c.items.size()));
    Indent();
    Line(
        std::format(
            "Expr[{}] {}", c.condition.value, FormatProcExpr(p, c.condition)));
    for (std::size_t k = 0; k < c.items.size(); ++k) {
      const auto& item = c.items[k];
      Line(std::format("item[{}] range_list (count={})", k, item.items.size()));
      Indent();
      for (const auto& inside_item : item.items) {
        std::visit(
            Overloaded{
                [&](const ExprId& val_id) {
                  Line(
                      std::format(
                          "Expr[{}] {}", val_id.value,
                          FormatProcExpr(p, val_id)));
                },
                [&](const InsideRangePair& r) {
                  Line(
                      std::format(
                          "[Expr[{}]:Expr[{}]] {} : {}", r.lo.value, r.hi.value,
                          FormatProcExpr(p, r.lo), FormatProcExpr(p, r.hi)));
                },
            },
            inside_item);
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

  void DumpForStmtNode(const ProceduralBody& p, StmtId id, const ForStmt& f) {
    Line(
        std::format(
            "Stmt[{}] ForStmt (init={}, step={}{})", id.value, f.init.size(),
            f.step.size(),
            f.break_label.has_value()
                ? std::format(", break_label={}", f.break_label->value)
                : ""));
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

  void DumpWhileStmtNode(
      const ProceduralBody& p, StmtId id, const WhileStmt& w) {
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

  void DumpRepeatStmtNode(
      const ProceduralBody& p, StmtId id, const RepeatStmt& r) {
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

  void DumpDoWhileStmtNode(
      const ProceduralBody& p, StmtId id, const DoWhileStmt& d) {
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

  void DumpForeverStmtNode(
      const ProceduralBody& p, StmtId id, const ForeverStmt& f) {
    Line(std::format("Stmt[{}] ForeverStmt", id.value));
    Indent();
    Line("body:");
    Indent();
    DumpStmt(p, f.body);
    Dedent();
    Dedent();
  }

  void DumpTimedStmtNode(
      const ProceduralBody& p, StmtId id, const TimedStmt& t) {
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

  void DumpStmt(const ProceduralBody& p, StmtId id) {
    const auto& s = p.stmts.Get(id);
    std::visit(
        Overloaded{
            [&](const EmptyStmt&) {
              Line(std::format("Stmt[{}] EmptyStmt", id.value));
            },
            [&](const VarDeclStmt& v) { DumpVarDeclStmtNode(p, id, v); },
            [&](const ExprStmt& e) { DumpExprStmtNode(p, id, e); },
            [&](const BlockStmt& b) { DumpBlockStmtNode(p, id, b); },
            [&](const ForkStmt& f) { DumpForkStmtNode(p, id, f); },
            [&](const IfStmt& i) { DumpIfStmtNode(p, id, i); },
            [&](const CaseStmt& c) { DumpCaseStmtNode(p, id, c); },
            [&](const CaseInsideStmt& c) { DumpCaseInsideStmtNode(p, id, c); },
            [&](const ForStmt& f) { DumpForStmtNode(p, id, f); },
            [&](const WhileStmt& w) { DumpWhileStmtNode(p, id, w); },
            [&](const RepeatStmt& r) { DumpRepeatStmtNode(p, id, r); },
            [&](const DoWhileStmt& d) { DumpDoWhileStmtNode(p, id, d); },
            [&](const ForeverStmt& f) { DumpForeverStmtNode(p, id, f); },
            [&](const BreakStmt& b) {
              Line(
                  std::format(
                      "Stmt[{}] BreakStmt{}", id.value,
                      b.target.has_value()
                          ? std::format(" -> label {}", b.target->value)
                          : ""));
            },
            [&](const ContinueStmt&) {
              Line(std::format("Stmt[{}] ContinueStmt", id.value));
            },
            [&](const ReturnStmt& r) {
              if (r.value.has_value()) {
                Line(
                    std::format(
                        "Stmt[{}] ReturnStmt value=Expr[{}]", id.value,
                        r.value->value));
                Indent();
                Line(
                    std::format(
                        "Expr[{}] {}", r.value->value,
                        FormatProcExpr(p, *r.value)));
                Dedent();
              } else {
                Line(std::format("Stmt[{}] ReturnStmt", id.value));
              }
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
                    "{{{} bits={}}}", FormatReferenceRoute(r.ref),
                    FormatFootprint(r.footprint));
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
            [&](const WaitForkStmt&) {
              Line(std::format("Stmt[{}] WaitForkStmt", id.value));
            },
            [&](const DisableForkStmt&) {
              Line(std::format("Stmt[{}] DisableForkStmt", id.value));
            },
        },
        s.data);
  }

  void DumpGenerate(const Generate& g) {
    Line(std::format("Generate items={}", g.data.items.size()));
    Indent();
    for (const auto& item : g.data.items) {
      const std::string idx =
          item.index.has_value() ? std::format("[{}]", *item.index) : "[-]";
      Line(std::format("{}:", idx));
      Indent();
      DumpScope(g.child_scopes.Get(item.scope));
      Dedent();
    }
    Dedent();
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
