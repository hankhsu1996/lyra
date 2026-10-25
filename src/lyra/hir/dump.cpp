#include "lyra/hir/dump.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <map>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/case_keyword.hpp"
#include "lyra/hir/class_declaration.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/continuous_assign.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/owned_child_ref.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/hir/procedural_scope.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/published_scope_ref.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/hir/structural_data_object.hpp"
#include "lyra/hir/structural_hops.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/hir/subroutine_prototype.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/unary_op.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/support/def_path.hpp"
#include "lyra/support/strength_level.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::hir {

namespace {

// A range of elements as the source wrote it: two ends, or a start and a width
// counted up or down.
auto RangeBoundsText(const RangeBounds& bounds) -> std::string {
  return std::visit(
      Overloaded{
          [](const RangeConstantBounds& b) {
            return std::format(
                "kind=Constant left=Expr[{}] right=Expr[{}]",
                b.left_bound.value, b.right_bound.value);
          },
          [](const RangeIndexedUpBounds& b) {
            return std::format(
                "kind=IndexedUp base_index=Expr[{}] width=Expr[{}]",
                b.base_index.value, b.width.value);
          },
          [](const RangeIndexedDownBounds& b) {
            return std::format(
                "kind=IndexedDown base_index=Expr[{}] width=Expr[{}]",
                b.base_index.value, b.width.value);
          }},
      bounds);
}

auto StrengthLevelLabel(support::StrengthLevel level) -> std::string_view {
  switch (level) {
    case support::StrengthLevel::kHighImpedance:
      return "highz";
    case support::StrengthLevel::kSmall:
      return "small";
    case support::StrengthLevel::kMedium:
      return "medium";
    case support::StrengthLevel::kWeak:
      return "weak";
    case support::StrengthLevel::kLarge:
      return "large";
    case support::StrengthLevel::kPull:
      return "pull";
    case support::StrengthLevel::kStrong:
      return "strong";
    case support::StrengthLevel::kSupply:
      return "supply";
  }
  throw InternalError("StrengthLevelLabel: unknown strength level");
}

auto NetTypeLabel(NetType net_type) -> std::string_view {
  switch (net_type) {
    case NetType::kWire:
      return "wire";
    case NetType::kTri:
      return "tri";
    case NetType::kWand:
      return "wand";
    case NetType::kTriand:
      return "triand";
    case NetType::kWor:
      return "wor";
    case NetType::kTrior:
      return "trior";
    case NetType::kTri0:
      return "tri0";
    case NetType::kTri1:
      return "tri1";
    case NetType::kSupply0:
      return "supply0";
    case NetType::kSupply1:
      return "supply1";
    case NetType::kUwire:
      return "uwire";
    case NetType::kTrireg:
      return "trireg";
  }
  throw InternalError("NetTypeLabel: unknown NetType");
}

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

auto ProceduralScopeKindLabel(ProceduralScopeKind kind) -> std::string_view {
  switch (kind) {
    case ProceduralScopeKind::kProcessRoot:
      return "process-root";
    case ProceduralScopeKind::kSubroutineRoot:
      return "subroutine-root";
    case ProceduralScopeKind::kBlock:
      return "block";
    case ProceduralScopeKind::kFork:
      return "fork";
  }
  throw InternalError(
      "ProceduralScopeKindLabel: unknown hir::ProceduralScopeKind");
}

auto ReferenceBindingLabel(ReferenceBinding binding) -> std::string_view {
  switch (binding) {
    case ReferenceBinding::kRef:
      return " ref";
    case ReferenceBinding::kConstRef:
      return " const ref";
    case ReferenceBinding::kInput:
      return " input";
  }
  throw InternalError("ReferenceBindingLabel: unknown hir::ReferenceBinding");
}

auto FormatArguments(std::span<const ExprId> arguments) -> std::string {
  std::string out;
  for (std::size_t i = 0; i < arguments.size(); ++i) {
    if (i != 0) out += ", ";
    out += std::format("Expr[{}]", arguments[i].value);
  }
  return std::format("arguments({})", out);
}

auto FormatAssignments(std::span<const ParameterValueAssignment> assignments)
    -> std::string {
  std::string out;
  for (std::size_t i = 0; i < assignments.size(); ++i) {
    if (i != 0) out += ", ";
    out += assignments[i].value.has_value()
               ? std::format("Expr[{}]", assignments[i].value->value)
               : std::string{"default"};
  }
  return std::format("arguments({})", out);
}

// The identifier a declaration answers to, quoted, or nothing at all where the
// source gave it none. Printing the absence as an empty string rather than a
// stand-in is the point: what a reader learns from this dump is which
// declarations the design wrote, and a stand-in spelled out of ordinary
// identifier characters is one a design may also write.
auto FormatName(const std::optional<std::string>& name) -> std::string {
  return name.has_value() ? std::format(" \"{}\"", *name) : std::string{};
}

auto FormatPublishedStorage(const PublishedStorage& storage) -> std::string {
  return std::visit(
      Overloaded{
          [](const VariableStorage&) { return std::string{}; },
          [](const NetStorage&) { return std::string{" net"}; },
          [](const ReferenceStorage& reference) {
            return std::string{ReferenceBindingLabel(reference.binding)};
          },
          [](const BorrowedObjectStorage&) {
            return std::string{" borrowed"};
          }},
      storage);
}

auto FormatStructuralDataObject(const StructuralDataObjectDecl& decl)
    -> std::string {
  return std::visit(
      Overloaded{
          [](const StructuralVariableDecl& var) {
            return var.initializer.has_value()
                       ? std::format(" init=Expr[{}]", var.initializer->value)
                       : std::string{};
          },
          [](const StructuralNetDecl& net) {
            return net.charge_strength.has_value()
                       ? std::format(
                             " net={} charge={}", NetTypeLabel(net.net_type),
                             StrengthLevelLabel(*net.charge_strength))
                       : std::format(" net={}", NetTypeLabel(net.net_type));
          },
          [](const StructuralReferenceDecl& reference) {
            return std::string{ReferenceBindingLabel(reference.binding)};
          },
          [](const StructuralGenvarDecl&) { return std::string{" genvar"}; },
          [](const StructuralConstructionValueDecl&) {
            return std::string{" given at construction"};
          },
          [](const StructuralParameterDecl& parameter) {
            return parameter.default_value.has_value()
                       ? std::format(
                             " given at construction default=Expr[{}]",
                             parameter.default_value->value)
                       : std::string{" given at construction"};
          },
          [](const StructuralLocalparamDecl& parameter) {
            return std::format(
                " settled=Expr[{}]", parameter.initializer.value);
          }},
      decl.kind);
}

auto FormatClass(ClassId id) -> std::string {
  return std::format("Class[{}]", id.value);
}

auto FormatCovergroup(CovergroupId id) -> std::string {
  return std::format("Covergroup[{}]", id.value);
}

auto CovergroupMethodLabel(CovergroupMethod method) -> std::string_view {
  switch (method) {
    case CovergroupMethod::kSample:
      return "sample";
    case CovergroupMethod::kGetCoverage:
      return "get_coverage";
    case CovergroupMethod::kGetInstCoverage:
      return "get_inst_coverage";
    case CovergroupMethod::kSetInstName:
      return "set_inst_name";
    case CovergroupMethod::kStart:
      return "start";
    case CovergroupMethod::kStop:
      return "stop";
  }
  throw InternalError("CovergroupMethodLabel: unknown covergroup method");
}

// Where a covergroup member is reached: through an instance, or through the
// type where no instance is named, and on one of its items where one is.
auto FormatClassPropertyTarget(const ClassPropertyTarget& target)
    -> std::string {
  return std::format("Class[{}].{}", target.owner.value, target.field.value);
}

auto FormatStaticPropertyTarget(const StaticPropertyTarget& target)
    -> std::string {
  return std::visit(
      Overloaded{
          [](const InstanceStaticPropertyTarget& t) -> std::string {
            return std::format(
                "Class[{}].{} of instance hops:{}", t.property.owner.value,
                t.property.prop.value, t.hops.value);
          },
          [](const ClassStaticPropertyTarget& t) -> std::string {
            return std::format("Class[{}].{}", t.owner.value, t.prop.value);
          }},
      target);
}

auto FormatClassMethod(const ClassMethodTarget& t) -> std::string {
  return std::format("Class[{}].{}", t.owner.value, t.method.value);
}

auto SubroutineKindName(SubroutineKind kind) -> std::string_view {
  switch (kind) {
    case SubroutineKind::kTask:
      return "task";
    case SubroutineKind::kFunction:
      return "function";
    case SubroutineKind::kConstraintBlock:
      return "constraint";
    case SubroutineKind::kStaticConstraintBlock:
      return "static constraint";
  }
  throw InternalError("SubroutineKindName: unknown subroutine kind");
}

// A method's part in dispatch, as its class declares it, written after its
// name.
auto FormatMethodDispatch(const MethodDispatch& dispatch) -> std::string {
  const auto pure = [](bool is_pure) {
    return is_pure ? std::string{" pure"} : std::string{};
  };
  return std::visit(
      Overloaded{
          [](const TypeAssociated&) { return std::string{" static"}; },
          [](const NotVirtual&) { return std::string{}; },
          [&](const IntroducesVirtual& introduces) {
            return " virtual" + pure(introduces.is_pure);
          },
          [&](const OverridesVirtual& overrides) {
            return std::format(
                " virtual{} overrides=Class[{}].{}", pure(overrides.is_pure),
                overrides.introducer.value, overrides.name);
          }},
      dispatch);
}

auto FormatRandomizationHook(RandomizationHook hook) -> std::string_view {
  switch (hook) {
    case RandomizationHook::kPreRandomize:
      return "pre_randomize";
    case RandomizationHook::kPostRandomize:
      return "post_randomize";
  }
  throw InternalError("FormatRandomizationHook: unknown hook");
}

auto FormatInstanceMethodCallee(const InstanceMethodCallee& callee)
    -> std::string {
  return std::visit(
      Overloaded{
          [](const ClassMethodTarget& declared) {
            return FormatClassMethod(declared);
          },
          [](const BuiltinHookCallee& hook) {
            return std::format(
                "BuiltinHook \"{}\" class={}",
                FormatRandomizationHook(hook.hook), FormatClass(hook.cls));
          }},
      callee);
}

class HirDumper {
 public:
  auto Dump(const CompilationUnit& unit) -> std::string {
    DumpUnit(unit);
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
    }
    throw InternalError("HirDumper::FormatBitAtom: unknown BitAtom");
  }

  static auto FormatSignedness(Signedness s) -> std::string_view {
    return s == Signedness::kSigned ? "signed" : "unsigned";
  }

  // A packed aggregate's members in declaration order, which is the only
  // thing that positions them: where each lands in the vector is projected
  // downstream, so there is nothing positional to print here.
  static auto FormatPackedAggregateFields(
      const std::vector<PackedAggregateField>& fields) -> std::string {
    std::string out;
    for (std::size_t i = 0; i < fields.size(); ++i) {
      if (i > 0) out += ", ";
      out += std::format("{}:Type[{}]", fields[i].name, fields[i].type.value);
    }
    return out;
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

  static auto FormatCaseKeyword(CaseKeyword keyword) -> std::string_view {
    switch (keyword) {
      case CaseKeyword::kCase:
        return "case";
      case CaseKeyword::kCasez:
        return "casez";
      case CaseKeyword::kCasex:
        return "casex";
    }
    throw InternalError("HirDumper::FormatCaseKeyword: unknown case keyword");
  }

  static auto FormatPackedArray(const PackedArrayType& p) -> std::string {
    return std::format(
        "PackedArray(dim=[{}:{}], elem=Type[{}], signed={})", p.dim.left,
        p.dim.right, p.element_type.value, FormatSignedness(p.signedness));
  }

  static auto FormatType(const Type& t) -> std::string {
    return t.Visit(
        Overloaded{
            [](const ScalarBitType& s) -> std::string {
              return std::format("ScalarBit(atom={})", FormatBitAtom(s.atom));
            },
            [](const PackedArrayType& p) -> std::string {
              return FormatPackedArray(p);
            },
            [](const PackedStructType& s) -> std::string {
              return std::format(
                  "PackedStruct(signed={}, fields=[{}])",
                  FormatSignedness(s.signedness),
                  FormatPackedAggregateFields(s.fields));
            },
            [](const PackedUnionType& u) -> std::string {
              return std::format(
                  "PackedUnion(signed={}, tagged={}, fields=[{}])",
                  FormatSignedness(u.signedness), u.tagged,
                  FormatPackedAggregateFields(u.fields));
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
                  "Enum({}::{}, base=Type[{}], members=[{}])",
                  e.declaration.unit_name,
                  support::DisplayOf(e.declaration.path), e.base_type.value,
                  members);
            },
            [](const UnpackedStructType& s) -> std::string {
              std::string fields;
              for (std::size_t i = 0; i < s.fields.size(); ++i) {
                if (i > 0) fields += ", ";
                fields += std::format(
                    "{}:Type[{}]", s.fields[i].name, s.fields[i].type.value);
              }
              return std::format(
                  "UnpackedStruct({}::{}, fields=[{}])",
                  s.declaration.unit_name,
                  support::DisplayOf(s.declaration.path), fields);
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
                  "ClassHandleType(class={})", FormatClass(c.class_id));
            },
            [](const ImportedClassHandleType& c) -> std::string {
              return std::format(
                  "ImportedClassHandleType(class={})",
                  support::ImportedRuntimeClassName(c.klass));
            },
            [](const WeakHandleType& w) -> std::string {
              return std::format(
                  "WeakHandleType(handle=Type[{}])", w.handle.value);
            },
            [](const CovergroupHandleType& c) -> std::string {
              return std::format(
                  "CovergroupHandleType(covergroup={})",
                  FormatCovergroup(c.covergroup));
            },
            [](const UnitObjectType& u) -> std::string {
              return std::format(
                  "UnitObjectType(scope_class={})", u.scope_class.value);
            },
            [](const UnitObjectsType& u) -> std::string {
              std::string ranges;
              for (const UnpackedRange& range : u.ranges) {
                ranges += std::format("[{}:{}]", range.left, range.right);
              }
              std::string kinds;
              for (const UnitObjectType& kind : u.kinds) {
                kinds += std::format(
                    "{}ScopeClass[{}]", kinds.empty() ? "" : " | ",
                    kind.scope_class.value);
              }
              // Which kind each position takes says nothing where there is one.
              std::string taken;
              if (u.kinds.size() > 1) {
                for (const std::uint32_t at : u.taken) {
                  taken +=
                      std::format("{}{}", taken.empty() ? " taken(" : ",", at);
                }
                taken += ')';
              }
              return std::format(
                  "UnitObjectsType{}({}){}", ranges, kinds, taken);
            },
            [](const VirtualInterfaceType& v) -> std::string {
              return std::format("VirtualInterfaceType(unit={})", v.unit_name);
            },
            [](const NullType&) -> std::string { return "NullType"; },
            [](const DynamicType&) -> std::string { return "DynamicType"; },
            [](const VoidType&) -> std::string { return "VoidType"; },
        });
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
                  "IntegerLiteral({})", FormatIntegralConstant(lit.value));
            },
            [](const StringLiteral& lit) -> std::string {
              return std::format("StringLiteral(\"{}\")", lit.value);
            },
            [](const RealLiteral& lit) -> std::string {
              return std::format("RealLiteral({})", lit.value.Value());
            },
            [](const NullLiteral&) -> std::string { return "NullLiteral"; },
            [](const ThisHandle&) -> std::string { return "ThisHandle"; },
            [](const RandomizedObject&) -> std::string {
              return "RandomizedObject";
            },
            [](const QueueLastIndex&) -> std::string {
              return "QueueLastIndex";
            },
            [](const ProceduralVarRef& r) -> std::string {
              return std::format("ProceduralVar[{}]", r.var.value);
            },
            [](const ClassPropertyRef& r) -> std::string {
              return std::format(
                  "ClassProperty[{}]", FormatClassPropertyTarget(r.target));
            },
            [](const StaticPropertyRef& r) -> std::string {
              return std::format(
                  "StaticProperty[{}]", FormatStaticPropertyTarget(r.target));
            },
            [](const RoutedValueRef& r) -> std::string {
              return std::format("RoutedValueRef[{}]", r.id.value);
            },
            [](const RoutedObjectRef& r) -> std::string {
              return std::format("RoutedObjectRef[{}]", r.id.value);
            },
            [](const IterationBindingRef& r) -> std::string {
              const char* role = r.role == IterationBindingRole::kElement
                                     ? "Element"
                                     : "Index";
              return std::format("Iteration[{}].{}", r.clause.value, role);
            },
            [](const PatternVarRef& r) -> std::string {
              return std::format("PatternVar[{}]", r.pattern.value);
            },
            [](const NamespaceValueRef& r) -> std::string {
              return std::format(
                  "NamespaceValue({}::{})", r.unit_name, r.variable_name);
            },
        },
        p);
  }

  static auto FormatSealedCell(const ValueTarget& cell) -> std::string {
    return std::visit(
        Overloaded{
            [](const RoutedValueRef& r) -> std::string {
              return std::format("routed_ref={}", r.id.value);
            },
            [](const NamespaceValueRef& r) -> std::string {
              return std::format("var={}::{}", r.unit_name, r.variable_name);
            },
            [](const StaticPropertyRef& r) -> std::string {
              return std::format(
                  "var=StaticProperty[{}]",
                  FormatStaticPropertyTarget(r.target));
            },
        },
        cell);
  }

  static auto FormatWatchedStorage(const WatchedStorage& storage)
      -> std::string {
    return std::visit(
        Overloaded{
            [](const ValueTarget& cell) -> std::string {
              return FormatSealedCell(cell);
            },
            [](const ProceduralVarRef& r) -> std::string {
              return std::format("var=ProceduralVar[{}]", r.var.value);
            }},
        storage);
  }

  static auto FormatWatchedPart(const WatchedPart& part) -> std::string {
    return std::visit(
        Overloaded{
            [](const WatchedWhole&) -> std::string { return "whole"; },
            [](const WatchedSelect& s) -> std::string {
              return std::format("Expr[{}]", s.prefix.value);
            }},
        part);
  }

  static auto FormatSensitivityEntry(const SensitivityEntry& entry)
      -> std::string {
    return std::format(
        "{} part={}", FormatWatchedStorage(entry.cell),
        FormatWatchedPart(entry.part));
  }

  static auto FormatSensitivityList(std::span<const SensitivityEntry> entries)
      -> std::string {
    std::string out = "[";
    for (std::size_t i = 0; i < entries.size(); ++i) {
      if (i != 0) out += ", ";
      out += std::format("{{{}}}", FormatSensitivityEntry(entries[i]));
    }
    out += "]";
    return out;
  }

  static auto FormatCells(std::span<const SensitivityEntry> cells)
      -> std::string {
    return std::format("cells={}", FormatSensitivityList(cells));
  }

  static auto FormatReads(const Reads& reads) -> std::string {
    std::string out = std::format(
        "reads={} writes={} calls=[", FormatSensitivityList(reads.cells),
        FormatSensitivityList(reads.writes));
    for (std::size_t i = 0; i < reads.calls.size(); ++i) {
      if (i != 0) out += ", ";
      out += std::format("Expr[{}]", reads.calls[i].value);
    }
    out += "]";
    if (reads.unreportable.has_value()) {
      out += std::format(" unreportable=\"{}\"", *reads.unreportable);
    }
    return out;
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

  static auto FormatCondition(const std::optional<ExprId>& condition)
      -> std::string {
    return condition.has_value()
               ? std::format(" iff=Expr[{}]", condition->value)
               : std::string{};
  }

  static auto FormatDelayControl(const DelayControl& d) -> std::string {
    return std::format("DelayControl duration=Expr[{}]", d.duration.value);
  }

  static auto FormatEventEntry(const EventEntry& entry) -> std::string {
    const std::string occurrence = std::visit(
        Overloaded{
            [](const ValueChange& c) {
              return std::format(
                  "signal=Expr[{}] edge={} {}", c.signal.value,
                  FormatEventEdge(c.edge), FormatCells(c.cells));
            },
            [](const NamedEventTrigger& t) {
              return std::format("event=Expr[{}]", t.event.value);
            }},
        entry.occurrence);
    return std::format(
        "{{{}{}}}", occurrence, FormatCondition(entry.condition));
  }

  static auto FormatEventControl(const EventControl& e) -> std::string {
    std::string out = "EventControl entries=[";
    for (std::size_t i = 0; i < e.entries.size(); ++i) {
      if (i != 0) out += ", ";
      out += FormatEventEntry(e.entries[i]);
    }
    out += "]";
    return out;
  }

  static auto FormatTimingControlHeader(const TimingControl& tc)
      -> std::string {
    return std::visit(
        Overloaded{
            [](const DelayControl& d) { return FormatDelayControl(d); },
            [](const EventControl& e) { return FormatEventControl(e); },
            [](const ImplicitEventControl& ie) -> std::string {
              return std::format(
                  "ImplicitEventControl sensitivity={}",
                  FormatSensitivityList(ie.sensitivity_list));
            },
        },
        tc);
  }

  static auto FormatDelayOrEventControl(const DelayOrEventControl& control)
      -> std::string {
    return std::visit(
        Overloaded{
            [](const DelayControl& d) { return FormatDelayControl(d); },
            [](const EventControl& e) { return FormatEventControl(e); },
            [](const RepeatedEventControl& r) -> std::string {
              return std::format(
                  "RepeatedEventControl count=Expr[{}] event={}", r.count.value,
                  FormatEventControl(r.event));
            },
        },
        control);
  }

  static auto FormatTakeoverLevel(support::TakeoverLevel level)
      -> std::string_view {
    switch (level) {
      case support::TakeoverLevel::kAssign:
        return "assign";
      case support::TakeoverLevel::kForce:
        return "force";
    }
    throw InternalError("FormatTakeoverLevel: unknown takeover level");
  }

  static auto FormatEffectTiming(const EffectTiming& timing) -> std::string {
    return std::visit(
        Overloaded{
            [](const ImmediateEffect&) -> std::string { return "immediate"; },
            [](const NonBlockingEffect& nb) -> std::string {
              return nb.control.has_value()
                         ? std::format(
                               "nonblocking control={}",
                               FormatDelayOrEventControl(*nb.control))
                         : std::string{"nonblocking"};
            }},
        timing);
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

  static auto FormatRunTimeCheck(RunTimeCheck check) -> std::string_view {
    switch (check) {
      case RunTimeCheck::kNone:
        return "none";
      case RunTimeCheck::kValueIsAMemberOfTheEnumeration:
        return "value-is-a-member";
      case RunTimeCheck::kObjectIsOfTheDestinationClass:
        return "object-is-of-the-destination-class";
      case RunTimeCheck::kValueIsOfAnEquivalentType:
        return "value-is-of-an-equivalent-type";
    }
    throw InternalError("HirDumper::FormatRunTimeCheck: unknown check");
  }

  static auto FormatAssignmentValidity(const AssignmentValidity& validity)
      -> std::string {
    return std::visit(
        Overloaded{
            [](const NoAssignmentAllowed&) -> std::string {
              return "no-assignment-allowed";
            },
            [](const AssignmentAllowed& allowed) -> std::string {
              return std::format(
                  "allowed check={}", FormatRunTimeCheck(allowed.check));
            }},
        validity);
  }

  static auto FormatInvalidAssignmentHandling(InvalidAssignmentHandling h)
      -> std::string_view {
    switch (h) {
      case InvalidAssignmentHandling::kAnswered:
        return "answered";
      case InvalidAssignmentHandling::kReported:
        return "reported";
    }
    throw InternalError(
        "HirDumper::FormatInvalidAssignmentHandling: unknown handling");
  }

  static auto FormatEnumMethod(EnumMethod m) -> std::string_view {
    switch (m) {
      case EnumMethod::kFirst:
        return "first";
      case EnumMethod::kLast:
        return "last";
      case EnumMethod::kNum:
        return "num";
      case EnumMethod::kName:
        return "name";
      case EnumMethod::kNext:
        return "next";
      case EnumMethod::kPrev:
        return "prev";
    }
    throw InternalError("HirDumper::FormatEnumMethod: unknown method");
  }

  static auto FormatArrayIteratorMethod(ArrayIteratorMethod m)
      -> std::string_view {
    switch (m) {
      case ArrayIteratorMethod::kSort:
        return "sort";
      case ArrayIteratorMethod::kRsort:
        return "rsort";
      case ArrayIteratorMethod::kSum:
        return "sum";
      case ArrayIteratorMethod::kProduct:
        return "product";
      case ArrayIteratorMethod::kAnd:
        return "and";
      case ArrayIteratorMethod::kOr:
        return "or";
      case ArrayIteratorMethod::kXor:
        return "xor";
      case ArrayIteratorMethod::kFind:
        return "find";
      case ArrayIteratorMethod::kFindIndex:
        return "find_index";
      case ArrayIteratorMethod::kFindFirst:
        return "find_first";
      case ArrayIteratorMethod::kFindFirstIndex:
        return "find_first_index";
      case ArrayIteratorMethod::kFindLast:
        return "find_last";
      case ArrayIteratorMethod::kFindLastIndex:
        return "find_last_index";
      case ArrayIteratorMethod::kMin:
        return "min";
      case ArrayIteratorMethod::kMax:
        return "max";
      case ArrayIteratorMethod::kUnique:
        return "unique";
      case ArrayIteratorMethod::kUniqueIndex:
        return "unique_index";
      case ArrayIteratorMethod::kMap:
        return "map";
    }
    throw InternalError("HirDumper::FormatArrayIteratorMethod: unknown method");
  }

  [[nodiscard]] auto FormatSubroutineRef(const SubroutineRef& callee) const
      -> std::string {
    return std::visit(
        Overloaded{
            [this](const StructuralSubroutineRef& u) -> std::string {
              const StructuralScope* owner = &ResolveScope(u.hops);
              std::string descent;
              for (const OwnedChildStep& step : u.descent) {
                descent += std::format(
                    " . {}{}", FormatOwnedChild(step.names),
                    FormatIndices(step.selects));
                owner = &BlockScopeAt(*owner, step);
              }
              const auto& decl =
                  owner->structural_subroutines.Get(u.subroutine);
              return std::format(
                  "StructuralSubroutine[{}](self^{}{}) \"{}\"",
                  u.subroutine.value, u.hops.value, descent, decl.name);
            },
            [](const MethodCallRef& m) -> std::string {
              const std::string recv = std::visit(
                  Overloaded{
                      [](const HandleReceiver& h) {
                        return std::format("Expr[{}]", h.expr.value);
                      },
                      [](const SelfReceiver&) { return std::string{"<self>"}; },
                      [](const SuperReceiver&) {
                        return std::string{"<super>"};
                      }},
                  m.receiver);
              return std::format(
                  "Method target={} recv={}",
                  FormatInstanceMethodCallee(m.callee), recv);
            },
            [](const StaticMethodCallRef& s) -> std::string {
              return std::format(
                  "StaticMethod target={} {}", FormatClassMethod(s.callee),
                  FormatDeclaringInstance(s.declaring_instance));
            },
            [](const SystemSubroutineRef& s) -> std::string {
              const auto& desc = support::LookupSystemSubroutine(s.id);
              return std::format(
                  "SystemSubroutine[{}] \"{}\"", s.id.value, desc.name);
            },
            [](const BuiltinMethodRef& b) -> std::string {
              return std::format(
                  "BuiltinFn \"{}\"{}", support::RuntimeEntryOf(b.method).name,
                  b.receiver.has_value()
                      ? std::format(" recv=Expr[{}]", b.receiver->value)
                      : std::string{});
            },
            [](const EnumMethodRef& e) -> std::string {
              return std::format(
                  "EnumMethod \"{}\"", FormatEnumMethod(e.method));
            },
            [](const ArrayIteratorMethodRef& a) -> std::string {
              return std::format(
                  "ArrayIteratorMethod \"{}\"{}",
                  FormatArrayIteratorMethod(a.method),
                  a.with_clause.has_value()
                      ? std::format(
                            " with={{element=\"{}\", expr=Expr[{}]}}",
                            a.with_clause->element_name,
                            a.with_clause->expr.value)
                      : std::string{});
            },
            [](const PastValueRef& p) -> std::string {
              return std::format("PastValue history={}", p.history.value);
            },
            [](const ValueChangeRef& v) -> std::string {
              return std::format(
                  "ValueChange history={} reading={}", v.history.value,
                  support::ValueChangeReadingName(v.reading));
            },
            [this](const ForeignImportRef& f) -> std::string {
              const auto& decl = unit_->foreign_imports.Get(f.id);
              return std::format(
                  "ForeignImport[{}]({}) \"{}\"", f.id.value,
                  f.declaring_scope.has_value()
                      ? std::format(
                            "declaring_scope=hops:{}", f.declaring_scope->value)
                      : "declaring_scope=none",
                  decl.name);
            },
            [](const NamespaceSubroutineRef& e) -> std::string {
              return std::format(
                  "NamespaceSubroutine {} \"{}::{}\"",
                  SubroutineKindName(e.interface.kind), e.unit_name,
                  e.subroutine_name);
            },
            [this](const PublishedScopeMethodRef& e) -> std::string {
              const ScopeClassSignature& published =
                  unit_->scope_classes.Get(e.scope_class);
              const SubroutinePrototype& callable =
                  published.callables.Get(e.callable);
              const std::string receiver = std::visit(
                  Overloaded{
                      [](const RoutedObjectRef& r) -> std::string {
                        return std::format("RoutedObjectRef[{}]", r.id.value);
                      },
                      [](const InterfaceInstanceAccessExpr& r) -> std::string {
                        return std::format(
                            "held=Expr[{}] steps={}", r.handle.value,
                            r.steps.size());
                      }},
                  e.receiver);
              return std::format(
                  "PublishedScopeMethod {} \"{}::{}\" recv={}",
                  SubroutineKindName(callable.interface.kind),
                  support::DisplayOf(published.class_path), callable.name,
                  receiver);
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

  // Records, for every class the unit declares, the structural nesting its
  // bodies resolve outward names against -- the chain ending at the scope that
  // declares it (LRM 23.9). Classes are printed before the scope tree, so the
  // walk that would have supplied the chain has not run when one is dumped.
  //
  // A class identity is the declaring unit's, so the record is that unit's
  // too: one dumper prints several units and their identities share a range.
  void RecordClassScopeChains(const StructuralScope& root) {
    class_scope_chains_.clear();
    RecordClassScopeChainsIn(root);
  }

  void RecordClassScopeChainsIn(const StructuralScope& scope) {
    scope_stack_.push_back(&scope);
    for (const ClassId id : scope.replicated_classes) {
      class_scope_chains_.emplace(id, scope_stack_);
    }
    for (const CovergroupId id : scope.replicated_covergroups) {
      covergroup_scope_chains_.emplace(id, scope_stack_);
    }
    for (const auto& g : scope.generates) {
      for (const auto& block : g.blocks) {
        RecordClassScopeChainsIn(block.scope);
      }
    }
    scope_stack_.pop_back();
  }

  static auto FormatInsideExprNode(const InsideExpr& in) -> std::string {
    std::string items;
    for (std::size_t i = 0; i < in.items.size(); ++i) {
      if (i != 0) items += ", ";
      items += std::format("Expr[{}]", in.items[i].value);
    }
    return std::format(
        "InsideExpr lhs=Expr[{}] items=[{}]", in.lhs.value, items);
  }

  [[nodiscard]] static auto FormatPattern(const Pattern& p) -> std::string {
    std::string formatted = std::visit(
        Overloaded{
            [](const WildcardPattern&) -> std::string {
              return "WildcardPattern";
            },
            [](const ConstantPattern& c) -> std::string {
              return std::format(
                  "ConstantPattern value=Expr[{}]", c.value.value);
            },
            [](const VariablePattern& v) -> std::string {
              return std::format("VariablePattern \"{}\"", v.name);
            },
            [](const TaggedPattern& t) -> std::string {
              if (!t.value_pattern.has_value()) {
                return std::format(
                    "TaggedPattern member={}", t.member_index.value);
              }
              return std::format(
                  "TaggedPattern member={} value=Pattern[{}]",
                  t.member_index.value, t.value_pattern->value);
            },
            [](const StructurePattern& s) -> std::string {
              std::string fields;
              for (std::size_t i = 0; i < s.field_patterns.size(); ++i) {
                if (i != 0) fields += ", ";
                fields += std::format(
                    "{}=Pattern[{}]", s.field_patterns[i].first,
                    s.field_patterns[i].second.value);
              }
              return std::format("StructurePattern fields=[{}]", fields);
            },
        },
        p.data);
    return std::format("{} : Type[{}]", formatted, p.subject_type.value);
  }

  static auto FormatExprIds(std::span<const ExprId> ids) -> std::string {
    std::string out;
    for (const ExprId id : ids) {
      if (!out.empty()) out += ", ";
      out += std::format("Expr[{}]", id.value);
    }
    return out;
  }

  static auto FormatModeTarget(const auto& target) -> std::string {
    return std::visit(
        Overloaded{
            [](const RandomVariableMode& m) -> std::string {
              return std::format("variable=Expr[{}]", m.variable.value);
            },
            [](const ObjectModes& m) -> std::string {
              return std::format("object=Expr[{}]", m.object.value);
            },
            [](const ConstraintBlockMode& m) -> std::string {
              return std::format(
                  "object=Expr[{}] block={}", m.object.value, m.name);
            },
            [](const StaticConstraintBlockMode& m) -> std::string {
              return std::format(
                  "static_block={}::{}", FormatClass(m.introducer), m.block);
            }},
        target);
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
                  "ConditionalExpr clauses={} then=Expr[{}] else=Expr[{}]",
                  c.conditions.size(), c.then_value.value, c.else_value.value);
            },
            [](const AssignExpr& a) -> std::string {
              const std::string op_str =
                  a.compound.has_value() ? std::format(
                                               " op={} at=Type[{}]",
                                               FormatBinaryOp(a.compound->op),
                                               a.compound->applied_at.value)
                                         : std::string{};
              return std::format(
                  "AssignExpr timing={}{} lhs=Expr[{}] rhs=Expr[{}]",
                  FormatEffectTiming(a.timing), op_str, a.lhs.value,
                  a.rhs.value);
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
              return std::format(
                  "CallExpr callee={} args=[{}]", FormatSubroutineRef(c.callee),
                  args);
            },
            [](const ConversionExpr& cv) -> std::string {
              return std::format(
                  "ConversionExpr kind={} operand=Expr[{}]",
                  FormatConversionKind(cv.kind), cv.operand.value);
            },
            [](const ConstantExpr& c) -> std::string {
              return std::format("ConstantExpr value=Expr[{}]", c.value.value);
            },
            [](const ValueRangeExpr& r) -> std::string {
              const auto bound = [](const std::optional<ExprId>& b) {
                return b.has_value() ? std::format("Expr[{}]", b->value)
                                     : std::string{"$"};
              };
              return std::format(
                  "ValueRangeExpr lo={} hi={}", bound(r.lo), bound(r.hi));
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
              return std::format(
                  "RangeSelectExpr base=Expr[{}] {}", sel.base_value.value,
                  RangeBoundsText(sel.bounds));
            },
            [](const MemberAccessExpr& sel) -> std::string {
              return std::format(
                  "MemberAccessExpr base=Expr[{}] field={}",
                  sel.base_value.value, sel.field_index.value);
            },
            [](const ClassPropertyAccessExpr& sel) -> std::string {
              return std::format(
                  "ClassPropertyAccessExpr base=Expr[{}] target={}",
                  sel.base_value.value, FormatClassPropertyTarget(sel.target));
            },
            [this](const InterfaceInstanceAccessExpr& sel) -> std::string {
              return std::format(
                  "InterfaceInstanceAccessExpr handle=Expr[{}] interface={} "
                  "steps={}",
                  sel.handle.value,
                  unit_->scope_classes.KeyOf(sel.scope_class).unit_name,
                  sel.steps.size());
            },
            [this](const InterfaceMemberAccessExpr& sel) -> std::string {
              return std::format(
                  "InterfaceMemberAccessExpr handle=Expr[{}] interface={} "
                  "steps={} ScopeClass[{}].member[{}]",
                  sel.instance.handle.value,
                  unit_->scope_classes.KeyOf(sel.instance.scope_class)
                      .unit_name,
                  sel.instance.steps.size(), sel.scope_class.value,
                  sel.member.value);
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
            [](const StreamingConcatExpr& s) -> std::string {
              std::string operands;
              for (std::size_t i = 0; i < s.operands.size(); ++i) {
                if (i != 0) {
                  operands += ", ";
                }
                const StreamOperand& operand = s.operands[i];
                operands += std::format("Expr[{}]", operand.value.value);
                if (operand.with.has_value()) {
                  operands += std::visit(
                      Overloaded{
                          [](ExprId element) {
                            return std::format(
                                " with [Expr[{}]]", element.value);
                          },
                          [](const RangeBounds& bounds) {
                            return std::format(
                                " with [{}]", RangeBoundsText(bounds));
                          }},
                      *operand.with);
                }
              }
              return std::format(
                  "StreamingConcatExpr block_bits={} operands=[{}]",
                  s.block_bits, operands);
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
            [](const AssignmentPatternKeyedExpr& k) -> std::string {
              std::string entries;
              for (std::size_t i = 0; i < k.entries.size(); ++i) {
                if (i != 0) {
                  entries += ", ";
                }
                entries += std::format(
                    "Expr[{}]: Expr[{}]", k.entries[i].index.value,
                    k.entries[i].value.value);
              }
              const std::string fill =
                  k.default_value.has_value()
                      ? std::format("Expr[{}]", k.default_value->value)
                      : "none";
              return std::format(
                  "AssignmentPatternKeyedExpr entries=[{}] default={}", entries,
                  fill);
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
                  "ClassNewExpr class={} args=[{}] {}", FormatClass(n.class_id),
                  args, FormatDeclaringInstance(n.declaring_instance));
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
            [](const TaggedUnionExpr& t) -> std::string {
              if (t.payload.has_value()) {
                return std::format(
                    "TaggedUnionExpr member={} payload=Expr[{}]",
                    t.member_index.value, t.payload->value);
              }
              return std::format(
                  "TaggedUnionExpr member={} void", t.member_index.value);
            },
            [](const DynamicCastExpr& c) -> std::string {
              return std::format(
                  "DynamicCastExpr destination=Expr[{}] source=Expr[{}] "
                  "validity={} on_invalid={}",
                  c.destination.value, c.source.value,
                  FormatAssignmentValidity(c.validity),
                  FormatInvalidAssignmentHandling(c.on_invalid));
            },
            [](const CovergroupNewExpr& n) -> std::string {
              std::string args;
              for (std::size_t i = 0; i < n.arguments.size(); ++i) {
                if (i != 0) args += ", ";
                args += std::format("Expr[{}]", n.arguments[i].value);
              }
              const std::string embedding =
                  n.embedding.has_value()
                      ? std::format(" embedding=Expr[{}]", n.embedding->value)
                      : std::string{};
              return std::format(
                  "CovergroupNewExpr covergroup={} args=[{}]{} {}",
                  FormatCovergroup(n.covergroup), args, embedding,
                  FormatDeclaringInstance(n.declaring_instance));
            },
            [](const CovergroupMethodCallExpr& c) -> std::string {
              std::string args;
              for (std::size_t i = 0; i < c.arguments.size(); ++i) {
                if (i != 0) args += ", ";
                args += c.arguments[i].has_value()
                            ? std::format("Expr[{}]", c.arguments[i]->value)
                            : std::string{"default"};
              }
              return std::format(
                  "CovergroupMethodCallExpr covergroup={} {} method={} "
                  "args=[{}]",
                  FormatCovergroup(c.covergroup),
                  FormatCovergroupReach(c.receiver, c.item),
                  CovergroupMethodLabel(c.method), args);
            },
            [](const CovergroupOptionExpr& o) -> std::string {
              return std::format(
                  "CovergroupOptionExpr covergroup={} {} {}",
                  FormatCovergroup(o.covergroup),
                  FormatCovergroupReach(o.receiver, o.item), o.field);
            },
            [](const RandomizeExpr& r) -> std::string {
              const std::string with =
                  r.with.has_value()
                      ? std::format(" with=Constraint[{}]", r.with->value)
                      : std::string{};
              return std::visit(
                  Overloaded{
                      [&](const RandomizedClassObject& o) -> std::string {
                        const std::string chosen = std::visit(
                            Overloaded{
                                [](const DeclaredRandomVariables&)
                                    -> std::string { return "declared"; },
                                [](const ListedRandomVariables& l)
                                    -> std::string {
                                  return std::format(
                                      "listed=[{}]",
                                      FormatExprIds(l.variables));
                                }},
                            o.variables);
                        return std::format(
                            "RandomizeExpr object=Expr[{}] {}{}",
                            o.object.value, chosen, with);
                      },
                      [&](const RandomizedScopeVariables& v) -> std::string {
                        return std::format(
                            "RandomizeExpr scope listed=[{}]{}",
                            FormatExprIds(v.variables), with);
                      }},
                  r.target);
            },
            [](const RandModeSetExpr& m) -> std::string {
              return std::format(
                  "RandModeSetExpr {} on=Expr[{}]", FormatModeTarget(m.target),
                  m.on.value);
            },
            [](const RandModeGetExpr& m) -> std::string {
              return std::format(
                  "RandModeGetExpr variable=Expr[{}]", m.target.variable.value);
            },
            [](const ConstraintModeSetExpr& m) -> std::string {
              return std::format(
                  "ConstraintModeSetExpr {} on=Expr[{}]",
                  FormatModeTarget(m.target), m.on.value);
            },
            [](const ConstraintModeGetExpr& m) -> std::string {
              return std::format(
                  "ConstraintModeGetExpr {}", FormatModeTarget(m.target));
            },
        },
        e.data);
    return std::format("type=Type[{}] {}", e.type.value, formatted);
  }

  [[nodiscard]] auto FormatProcExpr(const ProceduralBody& p, ExprId id) const
      -> std::string {
    return FormatExpr(p.exprs.Get(id));
  }

  void DumpUnit(const CompilationUnit& u) {
    unit_ = &u;
    Line(std::format("CompilationUnit \"{}\"", u.name));
    Indent();

    Line("Types:");
    Indent();
    for (const TypeId id : u.types.Ids()) {
      Line(std::format("[{}] {}", id.value, FormatType(u.types.Get(id))));
    }
    Dedent();

    for (const ForeignImportId id : u.foreign_imports.Ids()) {
      DumpForeignImport(id.value, u.foreign_imports.Get(id));
    }

    RecordClassScopeChains(u.root_scope);
    if (u.classes.size() > 0) {
      Line("Classes:");
      Indent();
      for (const ClassId id : u.classes.Ids()) {
        if (!u.classes.IsOwn(id)) {
          Line(
              std::format(
                  "[{}] of {} \"{}\"", id.value, u.classes.KeyOf(id).unit_name,
                  support::DisplayOf(u.classes.PathOf(id))));
          continue;
        }
        if (!u.classes.HasBodies(id)) {
          Line(
              std::format(
                  "[{}] <declared> \"{}\"", id.value,
                  support::DisplayOf(u.classes.PathOf(id))));
          continue;
        }
        DumpClass(
            id, support::DisplayOf(u.classes.PathOf(id)),
            u.classes.Declaration(id), u.classes.Get(id));
      }
      Dedent();
    }

    if (u.covergroups.size() > 0) {
      Line("Covergroups:");
      Indent();
      for (const CovergroupId id : u.covergroups.Ids()) {
        if (!u.covergroups.IsOwn(id)) {
          Line(
              std::format(
                  "[{}] of {} \"{}\"", id.value,
                  u.covergroups.KeyOf(id).unit_name,
                  support::DisplayOf(u.covergroups.PathOf(id))));
          continue;
        }
        DumpCovergroup(
            id, support::DisplayOf(u.covergroups.PathOf(id)),
            u.covergroups.Get(id));
      }
      Dedent();
    }

    if (!u.scope_classes.empty()) {
      Line("ScopeClasses:");
      Indent();
      for (const ScopeClassId id : u.scope_classes.Ids()) {
        const ScopeClassKey& key = u.scope_classes.KeyOf(id);
        if (!u.scope_classes.IsRecorded(id)) {
          Line(
              std::format(
                  "[{}] {}::{} <named>", id.value, key.unit_name,
                  support::DisplayOf(key.class_path)));
          continue;
        }
        const ScopeClassSignature& record = u.scope_classes.Get(id);
        Line(
            std::format(
                "[{}] {}::{}", id.value, key.unit_name,
                support::DisplayOf(key.class_path)));
        Indent();
        for (const PublishedMemberId member_id : record.members.Ids()) {
          const PublishedMember& member = record.members.Get(member_id);
          Line(
              std::format(
                  "[{}] \"{}\" : Type[{}]{}", member_id.value, member.name,
                  member.type.value, FormatPublishedStorage(member.storage)));
        }
        Dedent();
      }
      Dedent();
    }

    Line("Root:");
    Indent();
    DumpScope(u.root_scope);
    Dedent();

    Dedent();
  }

  static auto FormatBinElement(const BinElement& element) -> std::string {
    const auto end = [](const std::optional<ExprId>& e) -> std::string {
      return e.has_value() ? std::format("Expr[{}]", e->value) : "$";
    };
    return std::visit(
        Overloaded{
            [](const BinValue& v) -> std::string {
              return std::format("Expr[{}]", v.value.value);
            },
            [&](const BinRange& r) -> std::string {
              return std::format("[{}:{}]", end(r.low), end(r.high));
            },
            [](const BinTolerance& r) -> std::string {
              return std::format(
                  "[Expr[{}] {} Expr[{}]]", r.center.value,
                  r.in_percent ? "+%-" : "+/-", r.tolerance.value);
            }},
        element);
  }

  static auto FormatBinElements(const std::vector<BinElement>& elements)
      -> std::string {
    std::string text;
    for (const BinElement& element : elements) {
      if (!text.empty()) text += ", ";
      text += FormatBinElement(element);
    }
    return text;
  }

  static auto BinKindLabel(BinKind kind) -> std::string_view {
    switch (kind) {
      case BinKind::kBins:
        return "bins";
      case BinKind::kIgnoreBins:
        return "ignore_bins";
      case BinKind::kIllegalBins:
        return "illegal_bins";
    }
    throw InternalError("BinKindLabel: unknown kind of bins");
  }

  static auto FormatGuard(const std::optional<ExprId>& iff) -> std::string {
    return iff.has_value() ? std::format(" iff=Expr[{}]", iff->value) : "";
  }

  void DumpOptions(const std::vector<OptionSetting>& options) {
    for (const OptionSetting& option : options) {
      Line(
          std::format(
              "{}.{} = Expr[{}]", option.type_option ? "type_option" : "option",
              option.field, option.value.value));
    }
  }

  void DumpBinConstruct(const BinConstruct& bin) {
    const std::string form = std::visit(
        Overloaded{
            [](const ValueBins& v) -> std::string {
              const std::string array = std::visit(
                  Overloaded{
                      [](const OneBin&) -> std::string { return ""; },
                      [](const BinPerValue&) -> std::string { return "[]"; },
                      [](const FixedBins& f) -> std::string {
                        return std::format("[Expr[{}]]", f.count.value);
                      }},
                  v.array);
              std::string text = std::format(
                  "values{}{} {{{}}}", array, v.wildcard ? " wildcard" : "",
                  FormatBinElements(v.elements));
              if (v.set.has_value()) {
                text += std::format(" set=Expr[{}]", v.set->value);
              }
              if (v.filter.has_value()) {
                text += std::format(
                    " with item=ProceduralVar[{}] Expr[{}]",
                    v.filter->item.value, v.filter->keeps.value);
              }
              return text;
            },
            [](const TransitionBins& tr) -> std::string {
              std::string text = std::format(
                  "transitions{}{}", tr.per_sequence ? "[]" : "",
                  tr.wildcard ? " wildcard" : "");
              for (const auto& sequence : tr.sequences) {
                text += " (";
                for (std::size_t s = 0; s < sequence.size(); ++s) {
                  if (s != 0) text += " => ";
                  text += FormatBinElements(sequence[s].values);
                }
                text += ")";
              }
              return text;
            },
            [](const DefaultBins& d) -> std::string {
              return d.per_value ? "default[]" : "default";
            },
            [](const DefaultSequenceBins&) -> std::string {
              return "default sequence";
            }},
        bin.form);
    Line(
        std::format(
            "{} \"{}\" {}{}", BinKindLabel(bin.kind), bin.name, form,
            FormatGuard(bin.iff)));
  }

  static auto FormatSelect(const CrossSelect& select) -> std::string {
    return std::visit(
        Overloaded{
            [](const SelectAll&) -> std::string { return "all"; },
            [](const SelectBinsOf& b) -> std::string {
              std::string text = std::format(
                  "binsof({}{})", b.point,
                  b.bin.has_value() ? "." + *b.bin : std::string{});
              if (!b.intersect.empty()) {
                text += std::format(
                    " intersect {{{}}}", FormatBinElements(b.intersect));
              }
              return text;
            },
            [](const SelectNot& n) -> std::string {
              return std::format("!Select[{}]", n.operand.value);
            },
            [](const SelectAnd& a) -> std::string {
              return std::format(
                  "Select[{}] && Select[{}]", a.left.value, a.right.value);
            },
            [](const SelectOr& o) -> std::string {
              return std::format(
                  "Select[{}] || Select[{}]", o.left.value, o.right.value);
            },
            [](const SelectWith& w) -> std::string {
              return std::format(
                  "Select[{}] with Expr[{}]", w.candidates.value,
                  w.filter.value);
            },
            [](const SelectSet& s) -> std::string {
              return std::format("set Expr[{}]", s.tuples.value);
            }},
        select);
  }

  void DumpCovergroup(
      CovergroupId id, std::string_view name, const CovergroupDecl& c) {
    const auto chain = covergroup_scope_chains_.find(id);
    if (chain == covergroup_scope_chains_.end()) {
      throw InternalError(
          "HirDumper::DumpCovergroup: every covergroup a unit declares is "
          "named by one of its scopes");
    }
    const std::vector<const StructuralScope*> outer =
        std::exchange(scope_stack_, chain->second);
    Line(std::format("[{}] covergroup \"{}\"", id.value, name));
    Indent();
    const auto vars = [](const std::vector<SubroutineParam>& listed) {
      std::string text;
      for (const SubroutineParam& param : listed) {
        if (!text.empty()) text += ", ";
        text += std::format(
            "{} ProceduralVar[{}]", FormatParamDirection(param.direction),
            param.var.value);
      }
      return text;
    };
    Line(std::format("Formals: [{}]", vars(c.formals)));
    if (!c.sample_formals.empty()) {
      Line(std::format("SampleFormals: [{}]", vars(c.sample_formals)));
    }
    if (c.embedding.has_value()) {
      Line(std::format("Embedding: ProceduralVar[{}]", c.embedding->value));
    }
    if (c.base.has_value()) {
      Line(std::format("Extends: {}", FormatCovergroup(*c.base)));
    }
    std::visit(
        Overloaded{
            [&](const SampledByCall&) { Line("Sampled: by call"); },
            [&](const SampledOnEvent& e) {
              Line(
                  std::format(
                      "Sampled: on event of {} entries",
                      e.event.entries.size()));
            },
            [&](const SampledAtBlocks& b) {
              Line(std::format("Sampled: at {} block events", b.events.size()));
            }},
        c.sampling);
    DumpOptions(c.options);
    for (const Coverpoint& point : c.coverpoints) {
      Line(
          std::format(
              "coverpoint \"{}\" : Expr[{}]{}", point.name,
              point.expression.value, FormatGuard(point.iff)));
      Indent();
      DumpOptions(point.options);
      for (const BinConstruct& bin : point.bins) DumpBinConstruct(bin);
      Dedent();
    }
    for (const Cross& cross : c.crosses) {
      std::string targets;
      for (const std::string& target : cross.targets) {
        if (!targets.empty()) targets += ", ";
        targets += target;
      }
      Line(
          std::format(
              "cross \"{}\" [{}]{}", cross.name, targets,
              FormatGuard(cross.iff)));
      Indent();
      DumpOptions(cross.options);
      for (const CrossSelectId id : cross.selects.Ids()) {
        Line(
            std::format(
                "Select[{}] {}", id.value,
                FormatSelect(cross.selects.Get(id))));
      }
      for (const CrossBin& bin : cross.bins) {
        Line(
            std::format(
                "{} \"{}\" Select[{}]{}", BinKindLabel(bin.kind), bin.name,
                bin.select.value, FormatGuard(bin.iff)));
      }
      Dedent();
    }
    DumpProceduralBodyContents(c.body, c.procedural_scopes);
    Dedent();
    scope_stack_ = outer;
  }

  void DumpClass(
      ClassId id, std::string_view name, const ClassDeclaration& declared,
      const ClassDecl& c) {
    // A class body's outward references count hops from the scope that
    // declares the class, so the chain that scope sits on is what resolves
    // them -- the same reading a process of that scope gets from the walk.
    const auto chain = class_scope_chains_.find(id);
    if (chain == class_scope_chains_.end()) {
      throw InternalError(
          "HirDumper::DumpClass: every class a unit declares is named by one "
          "of its scopes");
    }
    const std::vector<const StructuralScope*> outer =
        std::exchange(scope_stack_, chain->second);
    const std::string kind =
        declared.is_interface_class ? "interface class" : "class";
    Line(std::format("[{}] {} \"{}\"", id.value, kind, name));
    Indent();
    if (declared.takes_declaring_instance) {
      Line("Takes: the instance it belongs to");
    }
    if (declared.base.has_value()) {
      Line(std::format("Extends: {}", FormatClass(*declared.base)));
    }
    for (const auto& impl : declared.implements) {
      Line(std::format("Implements: {}", FormatClass(impl)));
    }
    for (const ConformingBehavior& answered : c.conforming) {
      Line(
          std::format(
              "Conforms: {} <- {}",
              FormatClassMethod(answered.interface_behavior),
              answered.answered_by.has_value()
                  ? FormatClassMethod(*answered.answered_by)
                  : std::string{"nothing"}));
    }
    for (const DeclaredProperty& field : declared.properties) {
      Line(std::format("{}:Type[{}]", field.name, field.type.value));
    }
    for (const DeclaredProperty& field : declared.static_properties) {
      Line(std::format("static {}:Type[{}]", field.name, field.type.value));
    }
    for (const auto& init : c.field_inits) {
      Line(
          std::format(
              "FieldInit target=Field[{}] value=Expr[{}]", init.target.value,
              init.value.value));
    }
    for (const MethodId id : c.methods.Ids()) {
      DumpSubroutine(
          "Method", id.value, c.methods.Get(id), c.procedural_scopes,
          FormatMethodDispatch(declared.methods.Get(id).dispatch));
    }
    for (std::size_t i = 0; i < c.interface_forwards.size(); ++i) {
      const InterfaceForward& forward = c.interface_forwards[i];
      DumpSubroutine(
          "InterfaceForward", i, forward.method, c.procedural_scopes,
          std::format(" overrides={}", FormatClassMethod(forward.overrides)));
    }
    DumpSubroutine("Constructor", 0, c.constructor, c.procedural_scopes, "");
    if (declared.base.has_value()) {
      std::string args;
      for (std::size_t i = 0; i < c.base_call.arguments.size(); ++i) {
        if (i != 0) args += ", ";
        args += std::format("Expr[{}]", c.base_call.arguments[i].value);
      }
      Line(
          std::format(
              "BaseCall: {} ({})",
              FormatDeclaringInstance(c.base_call.declaring_instance), args));
    }
    Dedent();
    scope_stack_ = outer;
  }

  static auto FormatDeclaringInstance(
      const std::optional<DeclaringInstanceReach>& reach) -> std::string {
    if (!reach.has_value()) return "declaring_instance=none";
    return std::visit(
        Overloaded{
            [](StructuralHops hops) {
              return std::format("declaring_instance=hops:{}", hops.value);
            },
            [](const RoutedObjectRef& routed) {
              return std::format(
                  "declaring_instance=RoutedObjectRef[{}]", routed.id.value);
            }},
        *reach);
  }

  static auto FormatCovergroupReach(
      const CovergroupReceiver& receiver,
      const std::optional<std::string>& item) -> std::string {
    std::string reach = std::visit(
        Overloaded{
            [](const CovergroupInstanceReceiver& r) {
              return std::format("instance=Expr[{}]", r.instance.value);
            },
            [](const CovergroupTypeReceiver& r) {
              return std::format(
                  "type {}", FormatDeclaringInstance(r.declaring_instance));
            }},
        receiver);
    if (item.has_value()) reach += std::format(" item={}", *item);
    return reach;
  }

  static auto FormatIndices(const std::vector<std::uint32_t>& indices)
      -> std::string {
    std::string out;
    for (const auto index : indices) {
      out += std::format("[{}]", index);
    }
    return out;
  }

  static auto FormatPublication(const ScopePublication& published)
      -> std::string {
    std::string positions;
    const auto add = [&](std::string position) {
      if (!positions.empty()) positions += ", ";
      positions += position;
    };
    add(std::format(
        "as \"{}\"", support::DisplayOf(published.signature.class_path)));
    for (const PublishedDecl& decl : published.members) {
      add(std::visit(
          Overloaded{
              [](const StructuralDataObjectId& id) {
                return std::format("StructuralDataObject[{}]", id.value);
              },
              [](const PublishedStatic& local) {
                return std::format(
                    "Static[{}] of {}", local.var.value,
                    std::visit(
                        Overloaded{
                            [](ProcessId id) {
                              return std::format("Process[{}]", id.value);
                            },
                            [](StructuralSubroutineId id) {
                              return std::format(
                                  "StructuralSubroutine[{}]", id.value);
                            }},
                        local.body));
              },
              [](const ClassStaticPropertyTarget& property) {
                return std::format(
                    "StaticProperty[{}] of Class[{}]", property.prop.value,
                    property.owner.value);
              },
              [](const CovergroupBlockEvent& event) {
                return std::format(
                    "BlockEvent of Covergroup[{}]", event.covergroup.value);
              },
              [](const InstanceMemberId& id) {
                return std::format("InstanceMember[{}]", id.value);
              },
              [](const InterfacePortId& id) {
                return std::format("InterfacePort[{}]", id.value);
              }},
          decl));
    }
    for (const GenerateId id : published.loops) {
      add(std::format("Generate[{}]", id.value));
    }
    for (const GenerateId id : published.choices) {
      add(std::format("Generate[{}]", id.value));
    }
    for (const ProceduralScopeId id : published.disable_targets) {
      add(std::format("DisableTarget[ProceduralScope[{}]]", id.value));
    }
    for (const StructuralSubroutineId id : published.callables) {
      add(std::format("Method[StructuralSubroutine[{}]]", id.value));
    }
    return positions;
  }

  static auto FormatOwnedChild(const OwnedChildRef& names) -> std::string {
    return std::visit(
        Overloaded{
            [](const InstanceMemberId& id) {
              return std::format("InstanceMember[{}]", id.value);
            },
            [](const GenerateLoopRef& loop) {
              return std::format(
                  "Generate[{}].index{}", loop.generate.value, loop.index);
            },
            [](const GenerateBlockRef& block) {
              return std::format(
                  "Generate[{}].alt{}", block.generate.value,
                  block.alternative);
            },
        },
        names);
  }

  static auto FormatPublishedScopeRef(const PublishedScopeRef& names)
      -> std::string {
    return std::visit(
        Overloaded{
            [](const PublishedMemberRef& member) {
              return std::format(
                  "ScopeClass[{}].member[{}]", member.scope_class.value,
                  member.member.value);
            },
            [](const PublishedGenerateLoopRef& loop) {
              return std::format(
                  "ScopeClass[{}].loop[{}].index{} as "
                  "ScopeClass[{}]",
                  loop.scope_class.value, loop.loop.value, loop.index,
                  loop.result_class.value);
            },
            [](const PublishedGenerateBlockRef& block) {
              return std::format(
                  "ScopeClass[{}].choice[{}] as "
                  "ScopeClass[{}]",
                  block.scope_class.value, block.choice.value,
                  block.result_class.value);
            },
        },
        names);
  }

  // The scope a descent element names a block of `owner` lands in. A
  // subroutine's descent stays inside its own unit, so it names no instance.
  static auto BlockScopeAt(
      const StructuralScope& owner, const OwnedChildStep& step)
      -> const StructuralScope& {
    return std::visit(
        Overloaded{
            [](const InstanceMemberId&) -> const StructuralScope& {
              throw InternalError(
                  "HirDumper::BlockScopeAt: a descent names an instance of "
                  "another unit");
            },
            [&](const GenerateLoopRef& loop) -> const StructuralScope& {
              const Generate& gen = owner.generates.Get(loop.generate);
              return gen.blocks.Get(LoopBlockScopeOf(gen, loop.index)).scope;
            },
            [&](const GenerateBlockRef& block) -> const StructuralScope& {
              const Generate& gen = owner.generates.Get(block.generate);
              return gen.blocks.Get(ChosenBlockScopeOf(gen, block.alternative))
                  .scope;
            },
        },
        step.names);
  }

  static auto FormatSelectionChoice(
      const BlocksChoose& chosen, SelectionChoiceId at) -> std::string;

  static auto FormatSelectionBranch(
      const BlocksChoose& chosen, const SelectionBranch& branch)
      -> std::string {
    return std::visit(
        Overloaded{
            [](const NothingStands&) { return std::string{"-"}; },
            [](const AlternativeStands& stands) {
              return std::format("alt{}", stands.position);
            },
            [&](SelectionChoiceId nested) {
              return FormatSelectionChoice(chosen, nested);
            },
        },
        branch);
  }

  static auto FormatRouteWalk(
      const RouteBase& base, const std::vector<PathStep>& steps)
      -> std::string {
    std::string out = std::visit(
        Overloaded{
            [](const InUnitBase& b) {
              return std::format("self^{}", b.hops.value);
            },
            [](const EnclosingScopeBase& b) {
              return std::format(
                  "enclosing ScopeClass[{}]", b.scope_class.value);
            }},
        base);
    for (const PathStep& step : steps) {
      const std::string names = std::visit(
          Overloaded{
              [](const OwnedChildRef& owned) {
                return FormatOwnedChild(owned);
              },
              [](const InterfacePortId& port) {
                return std::format("InterfacePort[{}]", port.value);
              },
              [](const PublishedScopeRef& published) {
                return FormatPublishedScopeRef(published);
              }},
          step.names);
      out += std::format(" . {}{}", names, FormatIndices(step.selects));
    }
    return out;
  }

  static auto FormatLeaf(const DataLeaf& leaf) -> std::string {
    const std::string at = std::visit(
        Overloaded{
            [](const StructuralDataObjectLeaf& l) {
              return std::format(" . StructuralDataObject[{}]", l.object.value);
            },
            [](const ProceduralStaticLeaf& l) {
              const std::string body = std::visit(
                  Overloaded{
                      [](const ProcessId& id) {
                        return std::format("Process[{}]", id.value);
                      },
                      [](const StructuralSubroutineId& id) {
                        return std::format(
                            "StructuralSubroutine[{}]", id.value);
                      }},
                  l.body);
              return std::format(" . {}.ProceduralVar[{}]", body, l.var.value);
            },
            [](const PublishedMemberLeaf& l) {
              return std::format(
                  " . ScopeClass[{}].member[{}]", l.scope_class.value,
                  l.member.value);
            }},
        leaf);
    const DataCell cell = CellOf(leaf);
    return std::format(
        "{} : Type[{}]{}", at, cell.type.value,
        FormatPublishedStorage(cell.storage));
  }

  static auto FormatLeaf(const ScopeLeaf& leaf) -> std::string {
    return std::format(" : Type[{}] object", leaf.type.value);
  }

  static auto FormatLeaf(const DisableLeaf& leaf) -> std::string {
    return std::visit(
        Overloaded{
            [](const DisableTargetLeaf& l) {
              return std::format(
                  " . ProceduralScope[{}] : disable target", l.scope.value);
            },
            [](const PublishedDisableTargetLeaf& l) {
              return std::format(
                  " . ScopeClass[{}].disable_target[{}]", l.scope_class.value,
                  l.target.value);
            }},
        leaf);
  }

  template <typename Leaf>
  static auto FormatRoute(const Route<Leaf>& r) -> std::string {
    return FormatRouteWalk(r.base, r.steps) + FormatLeaf(r.leaf);
  }

  template <typename Walk, typename Id>
  void DumpTable(std::string_view label, const base::Arena<Walk, Id>& table) {
    for (const Id id : table.Ids()) {
      Line(
          std::format(
              "{}[{}] {}", label, id.value, FormatRoute(table.Get(id))));
    }
  }

  void DumpScope(const StructuralScope& s) {
    scope_stack_.push_back(&s);
    Line("Scope:");
    Indent();
    for (const StructuralDataObjectId id : s.structural_data_objects.Ids()) {
      const auto& v = s.structural_data_objects.Get(id);
      Line(
          std::format(
              "StructuralDataObject[{}] \"{}\" : Type[{}]{}", id.value, v.name,
              v.type.value, FormatStructuralDataObject(v)));
    }
    for (const InterfacePortId id : s.interface_ports.Ids()) {
      const auto& port = s.interface_ports.Get(id);
      std::string array_suffix;
      for (const UnpackedRange& dim : port.array_dims) {
        array_suffix += std::format("[{}:{}]", dim.left, dim.right);
      }
      Line(
          std::format(
              "InterfacePort[{}] \"{}\"{}", id.value, port.name, array_suffix));
    }
    Line(std::format("Published: {}", FormatPublication(s.published)));
    for (const StructuralSubroutineId id : s.structural_subroutines.Ids()) {
      DumpSubroutine(
          "StructuralSubroutine", id.value, s.structural_subroutines.Get(id),
          s.procedural_scopes, "");
    }
    for (const ForeignExportDecl& exported : s.foreign_exports) {
      Line(
          std::format(
              R"(ForeignExport StructuralSubroutine[{}] c_name="{}" ret={})",
              exported.subroutine.value, exported.foreign_name,
              support::DpiScalarAbiName(exported.ret_abi)));
      Indent();
      for (std::size_t i = 0; i < exported.params.size(); ++i) {
        Line(
            std::format(
                "Param[{}] {} {} : Type[{}]", i,
                support::DpiDirectionName(exported.params[i].direction),
                support::DpiCarrierName(exported.params[i].carrier),
                exported.params[i].sv_type.value));
      }
      Dedent();
    }
    if (!s.exprs.empty()) {
      Line("Exprs:");
      Indent();
      for (const ExprId id : s.exprs.Ids()) {
        Line(std::format("Expr[{}] {}", id.value, FormatExpr(s.exprs.Get(id))));
      }
      Dedent();
    }
    DumpPatterns(s.patterns);
    for (const SampledHistoryId id : s.sampled_histories.Ids()) {
      const auto& h = s.sampled_histories.Get(id);
      Line(
          std::format(
              "SampledHistory[{}] subject=Expr[{}] depth=Expr[{}]", id.value,
              h.subject.value, h.depth.value));
      Indent();
      Line(std::format("clock: {}", FormatEventControl(h.clock)));
      Dedent();
    }
    for (const ConcurrentAssertionId id : s.concurrent_assertions.Ids()) {
      DumpConcurrentAssertion(
          id, s.concurrent_assertions.Get(id), s.procedural_scopes);
    }
    for (const auto& p : s.processes) {
      DumpProcess(p, s.procedural_scopes);
    }
    for (const auto& ca : s.continuous_assigns) {
      DumpContinuousAssign(ca);
    }
    for (const auto& g : s.generates) {
      DumpGenerate(g);
    }
    for (const InstanceMemberId id : s.instance_members.Ids()) {
      const auto& im = s.instance_members.Get(id);
      std::string array_suffix;
      for (const UnpackedRange& dim : im.array_dims) {
        array_suffix += std::format("[{}:{}]", dim.left, dim.right);
      }
      std::string built_as;
      for (const InstanceAlternative& alternative : im.alternatives) {
        built_as += std::format(
            "{}ScopeClass[{}] {}", built_as.empty() ? "" : " | ",
            alternative.scope_class.value,
            FormatAssignments(alternative.assignments));
      }
      std::string taken;
      if (im.alternatives.size() > 1) {
        for (const std::uint32_t alternative : im.taken) {
          taken +=
              std::format("{}{}", taken.empty() ? " taken(" : ",", alternative);
        }
        taken += ')';
      }
      Line(
          std::format(
              "InstanceMember[{}] \"{}\"{} : {}{}", id.value, im.instance_name,
              array_suffix, built_as, taken));
    }
    DumpTable("RoutedValueRef", s.routes.values);
    DumpTable("RoutedObjectRef", s.routes.objects);
    DumpTable("RoutedDisableTargetRef", s.routes.disable_targets);
    for (const PortConnectionId id : s.port_connections.Ids()) {
      const auto& pc = s.port_connections.Get(id);
      const std::string body = std::visit(
          Overloaded{
              [](const DataPortConnection& d) -> std::string {
                std::string_view direction;
                switch (d.direction) {
                  case PortDirection::kInput:
                    direction = "input";
                    break;
                  case PortDirection::kOutput:
                    direction = "output";
                    break;
                  case PortDirection::kInOut:
                    direction = "inout";
                    break;
                  case PortDirection::kRef:
                    direction = "ref";
                    break;
                  case PortDirection::kConstRef:
                    direction = "const ref";
                    break;
                }
                const std::string endpoint = std::visit(
                    Overloaded{
                        [](const PortCellEndpoint& c) -> std::string {
                          return std::format("cell=Expr[{}]", c.cell.value);
                        },
                        [](const ValueRoute& r) -> std::string {
                          return std::format("alias_to:{}", FormatRoute(r));
                        }},
                    d.endpoint);
                if (!d.peer.has_value()) {
                  return std::format("{} {} unconnected", direction, endpoint);
                }
                return std::format(
                    "{} {} peer=Expr[{}]", direction, endpoint, d.peer->value);
              },
              [](const InterfacePortConnection& i) -> std::string {
                std::string peers;
                for (const ObjectRoute& peer : i.peers) {
                  if (!peers.empty()) peers += ", ";
                  peers += FormatRoute(peer);
                }
                return std::format(
                    "interface bind_to:{} peers:[{}]", FormatRoute(i.endpoint),
                    peers);
              }},
          pc.kind);
      Line(std::format("PortConnection[{}] {}", id.value, body));
    }
    for (const NetJoin& join : s.net_joins) {
      std::string sides;
      for (const NetSide& side : join.sides) {
        if (!sides.empty()) sides += " = ";
        std::string operands;
        for (const NetPositions& operand : side) {
          if (!operands.empty()) operands += ", ";
          operands += std::format(
              "Expr[{}][{}+:{}]", operand.part.value, operand.offset,
              operand.width);
        }
        sides += std::format("{{{}}}", operands);
      }
      Line(std::format("NetJoin {}", sides));
    }
    Dedent();
    scope_stack_.pop_back();
  }

  void DumpSubroutine(
      std::string_view label, std::size_t index, const SubroutineDecl& d,
      const base::Registry<ProceduralScopeDecl, ProceduralScopeId>& scopes,
      std::string_view dispatch) {
    std::string flags{dispatch};
    if (!d.root_stmt.has_value()) flags += " prototype";
    Line(
        std::format(
            "{}[{}] {} \"{}\" : Type[{}]{}", label, index,
            SubroutineKindName(d.kind), d.name, d.result_type.value, flags));
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
    Line(FormatReads(d.reads));
    // A pure virtual method is a prototype with no implementation (LRM 8.21):
    // its formals and the scope owning them are real declarations, but there
    // is no statement tree behind the signature to walk.
    if (d.root_stmt.has_value()) {
      DumpProceduralBody(d.body, *d.root_stmt, scopes);
    } else {
      DumpProceduralDeclarations(d.body, scopes);
    }
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

  void DumpProcess(
      const Process& p,
      const base::Registry<ProceduralScopeDecl, ProceduralScopeId>& scopes) {
    std::visit(
        Overloaded{
            [&](const InitialProcess&) { Line("Process (Initial)"); },
            [&](const FinalProcess&) { Line("Process (Final)"); },
            [&](const AlwaysProcess&) { Line("Process (Always)"); },
            [&](const AlwaysFfProcess&) { Line("Process (AlwaysFf)"); },
            [&](const AlwaysCombProcess& comb) {
              Line(
                  std::format(
                      "Process (AlwaysComb) implicit {}",
                      FormatReads(comb.implicit_reads)));
            },
            [&](const AlwaysLatchProcess& latch) {
              Line(
                  std::format(
                      "Process (AlwaysLatch) implicit {}",
                      FormatReads(latch.implicit_reads)));
            }},
        p.kind);
    Indent();
    DumpProceduralBody(p.body, p.root_stmt, scopes);
    Dedent();
  }

  // A pattern tree is reached by id from the clause or case item that names its
  // root, so the arena is dumped whole rather than inline at the reference.
  void DumpPatterns(const base::Arena<Pattern, PatternId>& patterns) {
    if (patterns.empty()) return;
    Line("Patterns:");
    Indent();
    for (const PatternId id : patterns.Ids()) {
      Line(
          std::format(
              "Pattern[{}] {}", id.value, FormatPattern(patterns.Get(id))));
    }
    Dedent();
  }

  // One arena holds the scopes of every body in a structural scope or class,
  // while the scopes of a single body form a tree under that body's root.
  // Descending from the root is what separates one body's scopes from its
  // peers', so the tree is walked instead of the arena being listed flat.
  void DumpProceduralScope(
      const base::Registry<ProceduralScopeDecl, ProceduralScopeId>& scopes,
      ProceduralScopeId id) {
    const auto& s = scopes.Get(id);
    std::string declares;
    for (std::size_t i = 0; i < s.declarations.size(); ++i) {
      declares += i == 0 ? " declares=(" : ", ";
      declares += std::format("ProceduralVar[{}]", s.declarations[i].value);
    }
    if (!s.declarations.empty()) declares += ")";
    Line(
        std::format(
            "ProceduralScope[{}] {}{}{}", id.value,
            ProceduralScopeKindLabel(s.kind), FormatName(s.source_name),
            declares));
    Indent();
    for (const ProceduralScopeId child : s.child_scopes) {
      DumpProceduralScope(scopes, child);
    }
    Dedent();
  }

  void DumpProceduralDeclarations(
      const ProceduralBody& body,
      const base::Registry<ProceduralScopeDecl, ProceduralScopeId>& scopes) {
    if (body.procedural_vars.size() != 0) {
      Line("ProceduralVars:");
      Indent();
      for (const ProceduralVarId id : body.procedural_vars.Ids()) {
        const auto& lv = body.procedural_vars.Get(id);
        Line(
            std::format(
                "ProceduralVar[{}]{} : Type[{}]{}{}{}", id.value,
                FormatName(lv.name), lv.type.value,
                lv.lifetime == VariableLifetime::kStatic ? " static" : "",
                lv.lifetime_extended ? " lifetime-extended" : "",
                lv.init.has_value()
                    ? std::format(" init=Expr[{}]", lv.init->value)
                    : ""));
      }
      Dedent();
    }
    Line("ProceduralScopes:");
    Indent();
    DumpProceduralScope(scopes, body.root_scope);
    Dedent();
  }

  // Everything a body holds except where execution enters it, which the two
  // callers answer differently: a procedure runs its root statement, and a
  // concurrent assertion's action body is entered at whichever arm an outcome
  // selects.
  void DumpProceduralBodyContents(
      const ProceduralBody& body,
      const base::Registry<ProceduralScopeDecl, ProceduralScopeId>& scopes) {
    DumpProceduralDeclarations(body, scopes);
    if (!body.exprs.empty()) {
      Line("Exprs:");
      Indent();
      for (const ExprId id : body.exprs.Ids()) {
        Line(
            std::format(
                "Expr[{}] {}", id.value, FormatExpr(body.exprs.Get(id))));
      }
      Dedent();
    }
    DumpPatterns(body.patterns);
  }

  void DumpProceduralBody(
      const ProceduralBody& body, StmtId root_stmt,
      const base::Registry<ProceduralScopeDecl, ProceduralScopeId>& scopes) {
    DumpProceduralBodyContents(body, scopes);
    DumpStmt(body, root_stmt);
  }

  void DumpConcurrentAssertion(
      ConcurrentAssertionId id, const ConcurrentAssertionDecl& decl,
      const base::Registry<ProceduralScopeDecl, ProceduralScopeId>& scopes) {
    Line(
        std::format(
            "ConcurrentAssertion[{}] standing=ProceduralScope[{}]", id.value,
            decl.standing_scope.value));
    Indent();
    DumpProceduralBodyContents(decl.action, scopes);
    std::visit(
        Overloaded{
            [&](const ConcurrentAssertStmt& a) {
              Line(std::format("{} property", DirectiveName(a.directive)));
              Indent();
              DumpPropertySpec(decl.action, a.spec);
              DumpActionBlock(decl.action, "pass", a.pass_stmt);
              DumpActionBlock(decl.action, "fail", a.fail_stmt);
              Dedent();
            },
            [&](const ConcurrentCoverStmt& c) {
              Line("cover property");
              Indent();
              DumpPropertySpec(decl.action, c.spec);
              DumpActionBlock(decl.action, "pass", c.pass_stmt);
              Dedent();
            }},
        decl.assertion);
    Dedent();
  }

  void DumpContinuousAssign(const ContinuousAssign& ca) {
    std::string delay;
    if (ca.delay.has_value()) {
      delay = std::visit(
          Overloaded{
              [](const SingleDelay& d) {
                return std::format(" delay=Expr[{}]", d.amount.value);
              },
              [](const RiseFallDelay& d) {
                return std::format(
                    " delay=(rise Expr[{}], fall Expr[{}])", d.rise.value,
                    d.fall.value);
              },
              [](const RiseFallTurnOffDelay& d) {
                return std::format(
                    " delay=(rise Expr[{}], fall Expr[{}], turn-off Expr[{}])",
                    d.rise.value, d.fall.value, d.turn_off.value);
              }},
          *ca.delay);
    }
    Line(
        std::format(
            "ContinuousAssign lhs=Expr[{}] rhs=Expr[{}] strength={}{}",
            ca.lhs.value, ca.rhs.value, StrengthLevelLabel(ca.strength),
            delay));
    Indent();
    if (!ca.sensitivity_list.empty()) {
      Line("SensitivityList:");
      Indent();
      for (const auto& r : ca.sensitivity_list) {
        Line(FormatSensitivityEntry(r));
      }
      Dedent();
    }
    Dedent();
  }

  void DumpVarDeclStmtNode(StmtId id, const VarDeclStmt& v) {
    Line(
        std::format(
            "Stmt[{}] VarDeclStmt var=ProceduralVar[{}]", id.value,
            v.var.value));
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
            "Stmt[{}] BlockStmt (count={}) scope=ProceduralScope[{}]", id.value,
            b.statements.size(), b.scope.value));
    Indent();
    for (const auto child : b.statements) {
      DumpStmt(p, child);
    }
    Dedent();
  }

  void DumpForkStmtNode(const ProceduralBody& p, StmtId id, const ForkStmt& f) {
    Line(
        std::format(
            "Stmt[{}] ForkStmt {} (locals={}, branches={}) "
            "scope=ProceduralScope[{}]",
            id.value, ForkJoinModeLabel(f.mode), f.locals.size(),
            f.branches.size(), f.scope.value));
    Indent();
    for (const auto local : f.locals) {
      DumpStmt(p, local);
    }
    for (const auto branch : f.branches) {
      DumpStmt(p, branch);
    }
    Dedent();
  }

  void DumpConditionClauses(
      const ProceduralBody& p, const std::vector<ConditionClause>& clauses) {
    for (const auto& clause : clauses) {
      Line(
          std::format(
              "Expr[{}] {}", clause.expr.value,
              FormatProcExpr(p, clause.expr)));
      if (clause.pattern.has_value()) {
        Line(std::format("matches Pattern[{}]", clause.pattern->value));
      }
    }
  }

  void DumpIfStmtNode(const ProceduralBody& p, StmtId id, const IfStmt& i) {
    Line(
        std::format(
            "Stmt[{}] IfStmt{} clauses={}", id.value,
            FormatUniquePriorityCheck(i.check), i.conditions.size()));
    Indent();
    DumpConditionClauses(p, i.conditions);
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

  void DumpActionBlock(
      const ProceduralBody& p, std::string_view arm,
      std::optional<StmtId> stmt) {
    if (!stmt.has_value()) {
      return;
    }
    Line(std::format("{}:", arm));
    Indent();
    DumpStmt(p, *stmt);
    Dedent();
  }

  static auto DirectiveName(AssertionDirective directive) -> std::string_view {
    switch (directive) {
      case AssertionDirective::kAssert:
        return "assert";
      case AssertionDirective::kAssume:
        return "assume";
    }
    throw InternalError("DirectiveName: unknown hir::AssertionDirective");
  }

  static auto ConnectiveName(PropertyConnective connective)
      -> std::string_view {
    switch (connective) {
      case PropertyConnective::kAnd:
        return "And";
      case PropertyConnective::kOr:
        return "Or";
      case PropertyConnective::kImplies:
        return "Implies";
      case PropertyConnective::kIff:
        return "Iff";
    }
    throw InternalError("ConnectiveName: unknown hir::PropertyConnective");
  }

  static auto TimingName(AssertionTiming timing) -> std::string_view {
    switch (timing) {
      case AssertionTiming::kSimple:
        return "simple";
      case AssertionTiming::kObserved:
        return "deferred #0";
      case AssertionTiming::kFinal:
        return "deferred final";
    }
    throw InternalError("TimingName: unknown hir::AssertionTiming");
  }

  void DumpAssertStmtNode(
      const ProceduralBody& p, StmtId id, const AssertStmt& a) {
    const std::string_view directive = DirectiveName(a.directive);
    Line(
        std::format(
            "Stmt[{}] AssertStmt {} [{}] cond=Expr[{}]", id.value, directive,
            TimingName(a.timing), a.condition.value));
    Indent();
    Line(
        std::format(
            "Expr[{}] {}", a.condition.value, FormatProcExpr(p, a.condition)));
    DumpActionBlock(p, "pass", a.pass_stmt);
    DumpActionBlock(p, "fail", a.fail_stmt);
    Dedent();
  }

  void DumpCoverStmtNode(
      const ProceduralBody& p, StmtId id, const CoverStmt& c) {
    Line(
        std::format(
            "Stmt[{}] CoverStmt [{}] cond=Expr[{}]", id.value,
            TimingName(c.timing), c.condition.value));
    Indent();
    Line(
        std::format(
            "Expr[{}] {}", c.condition.value, FormatProcExpr(p, c.condition)));
    DumpActionBlock(p, "pass", c.pass_stmt);
    Dedent();
  }

  static auto FormatTickRange(const TickRange& range) -> std::string {
    if (range.max == range.min) {
      return std::format("[{}]", range.min);
    }
    return std::format("[{}:{}]", range.min, range.max);
  }

  void DumpSequenceExpr(const ProceduralBody& p, SequenceExprId id) {
    std::visit(
        Overloaded{
            [&](const SequenceBoolean& b) {
              Line(
                  std::format(
                      "Sequence[{}] Boolean Expr[{}] {}", id.value,
                      b.condition.value, FormatProcExpr(p, b.condition)));
            },
            [&](const SequenceDelay& d) {
              Line(
                  std::format(
                      "Sequence[{}] Delay {}", id.value,
                      FormatTickRange(d.delay)));
              Indent();
              DumpSequenceExpr(p, d.head);
              DumpSequenceExpr(p, d.tail);
              Dedent();
            },
            [&](const SequenceRepetition& r) {
              Line(
                  std::format(
                      "Sequence[{}] Repetition {}", id.value,
                      FormatTickRange(r.count)));
              Indent();
              DumpSequenceExpr(p, r.body);
              Dedent();
            }},
        p.sequence_exprs.Get(id).data);
  }

  void DumpPropertyExpr(const ProceduralBody& p, PropertyExprId id) {
    std::visit(
        Overloaded{
            [&](const PropertySequence& s) {
              Line(
                  std::format(
                      "Property[{}] Sequence ({})", id.value,
                      s.strength == SequenceStrength::kStrong ? "strong"
                                                              : "weak"));
              Indent();
              DumpSequenceExpr(p, s.sequence);
              Dedent();
            },
            [&](const PropertyImplication& i) {
              Line(
                  std::format(
                      "Property[{}] Implication ({})", id.value,
                      i.start == ConsequentStart::kSameTick ? "|->" : "|=>"));
              Indent();
              DumpSequenceExpr(p, i.antecedent);
              DumpPropertyExpr(p, i.consequent);
              Dedent();
            },
            [&](const PropertyFollowedBy& f) {
              Line(
                  std::format(
                      "Property[{}] FollowedBy ({})", id.value,
                      f.start == ConsequentStart::kSameTick ? "#-#" : "#=#"));
              Indent();
              DumpSequenceExpr(p, f.antecedent);
              DumpPropertyExpr(p, f.consequent);
              Dedent();
            },
            [&](const PropertyNot& n) {
              Line(std::format("Property[{}] Not", id.value));
              Indent();
              DumpPropertyExpr(p, n.operand);
              Dedent();
            },
            [&](const PropertyBinary& b) {
              Line(
                  std::format(
                      "Property[{}] {}", id.value,
                      ConnectiveName(b.connective)));
              Indent();
              DumpPropertyExpr(p, b.lhs);
              DumpPropertyExpr(p, b.rhs);
              Dedent();
            },
            [&](const PropertyConditional& c) {
              Line(
                  std::format(
                      "Property[{}] If Expr[{}] {}", id.value,
                      c.condition.value, FormatProcExpr(p, c.condition)));
              Indent();
              Line("then");
              Indent();
              DumpPropertyExpr(p, c.then_property);
              Dedent();
              if (c.else_property.has_value()) {
                Line("else");
                Indent();
                DumpPropertyExpr(p, *c.else_property);
                Dedent();
              }
              Dedent();
            },
            [&](const PropertyCase& c) {
              Line(
                  std::format(
                      "Property[{}] Case Expr[{}] {}", id.value,
                      c.subject.value, FormatProcExpr(p, c.subject)));
              Indent();
              for (const PropertyCaseItem& item : c.items) {
                std::string labels;
                for (const ExprId label : item.labels) {
                  if (!labels.empty()) {
                    labels += ", ";
                  }
                  labels += FormatProcExpr(p, label);
                }
                Line(std::format("item {}", labels));
                Indent();
                DumpPropertyExpr(p, item.property);
                Dedent();
              }
              if (c.default_property.has_value()) {
                Line("default");
                Indent();
                DumpPropertyExpr(p, *c.default_property);
                Dedent();
              }
              Dedent();
            }},
        p.property_exprs.Get(id).data);
  }

  void DumpPropertySpec(const ProceduralBody& p, const PropertySpec& spec) {
    Line(std::format("clock={}", FormatEventControl(spec.clock)));
    if (spec.disable.has_value()) {
      Line(
          std::format(
              "disable iff=Expr[{}] {} watching={}",
              spec.disable->condition.value,
              FormatProcExpr(p, spec.disable->condition),
              spec.disable->sensitivity.size()));
    }
    DumpPropertyExpr(p, spec.body);
  }

  void DumpConcurrentAssertStmtNode(
      const ProceduralBody& p, StmtId id, const ConcurrentAssertStmt& a) {
    const std::string_view directive = DirectiveName(a.directive);
    Line(
        std::format(
            "Stmt[{}] ConcurrentAssertStmt {} property", id.value, directive));
    Indent();
    DumpPropertySpec(p, a.spec);
    DumpActionBlock(p, "pass", a.pass_stmt);
    DumpActionBlock(p, "fail", a.fail_stmt);
    Dedent();
  }

  void DumpConcurrentCoverStmtNode(
      const ProceduralBody& p, StmtId id, const ConcurrentCoverStmt& c) {
    Line(std::format("Stmt[{}] ConcurrentCoverStmt property", id.value));
    Indent();
    DumpPropertySpec(p, c.spec);
    DumpActionBlock(p, "pass", c.pass_stmt);
    Dedent();
  }

  void DumpExpectStmtNode(
      const ProceduralBody& p, StmtId id, const ExpectStmt& e) {
    Line(std::format("Stmt[{}] ExpectStmt property", id.value));
    Indent();
    DumpPropertySpec(p, e.spec);
    DumpActionBlock(p, "pass", e.pass_stmt);
    DumpActionBlock(p, "fail", e.fail_stmt);
    Dedent();
  }

  void DumpCaseStmtNode(const ProceduralBody& p, StmtId id, const CaseStmt& c) {
    const std::string_view label_test = std::visit(
        Overloaded{
            [](CaseKeyword keyword) { return FormatCaseKeyword(keyword); },
            [](CaseInside) -> std::string_view { return "case inside"; }},
        c.label_test);
    Line(
        std::format(
            "Stmt[{}] CaseStmt {}{} cond=Expr[{}] (items={})", id.value,
            label_test, FormatUniquePriorityCheck(c.check), c.condition.value,
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

  void DumpForStmtNode(const ProceduralBody& p, StmtId id, const ForStmt& f) {
    Line(
        std::format(
            "Stmt[{}] ForStmt (init={}, step={})", id.value, f.init.size(),
            f.step.size()));
    Indent();
    for (std::size_t k = 0; k < f.init.size(); ++k) {
      Line(
          std::format(
              "init[{}]: Expr[{}] {}", k, f.init[k].value,
              FormatProcExpr(p, f.init[k])));
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

  void DumpForeachStmtNode(
      const ProceduralBody& p, StmtId id, const ForeachStmt& f) {
    std::string loop_vars;
    for (const std::optional<ProceduralVarId>& var : f.loop_vars) {
      if (!loop_vars.empty()) loop_vars += ", ";
      loop_vars += var.has_value()
                       ? std::format("ProceduralVar[{}]", var->value)
                       : std::string{"-"};
    }
    Line(
        std::format(
            "Stmt[{}] ForeachStmt array=Expr[{}] loop_vars=[{}]", id.value,
            f.array.value, loop_vars));
    Indent();
    Line(std::format("Expr[{}] {}", f.array.value, FormatProcExpr(p, f.array)));
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
            [&](const VarDeclStmt& v) { DumpVarDeclStmtNode(id, v); },
            [&](const ExprStmt& e) { DumpExprStmtNode(p, id, e); },
            [&](const BlockStmt& b) { DumpBlockStmtNode(p, id, b); },
            [&](const ForkStmt& f) { DumpForkStmtNode(p, id, f); },
            [&](const IfStmt& i) { DumpIfStmtNode(p, id, i); },
            [&](const CaseStmt& c) { DumpCaseStmtNode(p, id, c); },
            [&](const PatternCaseStmt& pc) {
              Line(
                  std::format(
                      "Stmt[{}] PatternCaseStmt {}", id.value,
                      FormatCaseKeyword(pc.keyword)));
            },
            [&](const AssertStmt& a) { DumpAssertStmtNode(p, id, a); },
            [&](const CoverStmt& c) { DumpCoverStmtNode(p, id, c); },
            [&](const ConcurrentAssertStmt& a) {
              DumpConcurrentAssertStmtNode(p, id, a);
            },
            [&](const ConcurrentCoverStmt& c) {
              DumpConcurrentCoverStmtNode(p, id, c);
            },
            [&](const ExpectStmt& e) { DumpExpectStmtNode(p, id, e); },
            [&](const ForStmt& f) { DumpForStmtNode(p, id, f); },
            [&](const ForeachStmt& f) { DumpForeachStmtNode(p, id, f); },
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
                      "Stmt[{}] EventTriggerStmt event=Expr[{}] timing={}",
                      id.value, et.event.value, FormatEffectTiming(et.timing)));
            },
            [&](const WaitStmt& w) {
              Line(
                  std::format(
                      "Stmt[{}] WaitStmt cond=Expr[{}] {}", id.value,
                      w.cond.value, FormatCells(w.cells)));
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
            [&](const DisableStmt& d) {
              const std::string target = std::visit(
                  Overloaded{
                      [](const DirectDisableTarget& t) {
                        return std::format(
                            "ProceduralScope[{}]", t.scope.value);
                      },
                      [](const RoutedDisableTarget& t) {
                        return std::format(
                            "RoutedDisableTargetRef[{}]", t.target.id.value);
                      }},
                  d.target);
              Line(
                  std::format(
                      "Stmt[{}] DisableStmt target={}", id.value, target));
            },
            [&](const ProceduralContinuousAssignStmt& pca) {
              Line(
                  std::format(
                      "Stmt[{}] ProceduralContinuousAssignStmt level={} "
                      "target=Expr[{}] source=Expr[{}] sensitivity={}",
                      id.value, FormatTakeoverLevel(pca.level),
                      pca.target.value, pca.source.value,
                      FormatSensitivityList(pca.sensitivity_list)));
            },
            [&](const ProceduralContinuousEndStmt& pce) {
              Line(
                  std::format(
                      "Stmt[{}] ProceduralContinuousEndStmt level={} "
                      "target=Expr[{}]",
                      id.value, FormatTakeoverLevel(pce.level),
                      pce.target.value));
            },
            [&](const ConstraintStmt& c) {
              Line(std::format("Stmt[{}] ConstraintStmt", id.value));
              Indent();
              DumpConstraint(p, c.constraints);
              Dedent();
            },
        },
        s.data);
  }

  void DumpConstraint(const ProceduralBody& p, ConstraintId id) {
    const Constraint& c = p.constraints.Get(id);
    const auto expr = [&](ExprId e) {
      Line(std::format("Expr[{}] {}", e.value, FormatProcExpr(p, e)));
    };
    std::visit(
        Overloaded{
            [&](const ConstraintList& l) {
              Line(std::format("Constraint[{}] List", id.value));
              Indent();
              for (const ConstraintId item : l.items) {
                DumpConstraint(p, item);
              }
              Dedent();
            },
            [&](const ExpressionConstraint& e) {
              Line(
                  std::format(
                      "Constraint[{}] Expression{}", id.value,
                      e.is_soft ? " soft" : ""));
              Indent();
              expr(e.condition);
              Dedent();
            },
            [&](const DistConstraint& d) {
              Line(
                  std::format(
                      "Constraint[{}] Dist{}", id.value,
                      d.is_soft ? " soft" : ""));
              Indent();
              expr(d.subject);
              for (const DistItem& item : d.items) {
                Line(
                    std::format(
                        "item low=Expr[{}]{}{}", item.low.value,
                        item.high.has_value()
                            ? std::format(" high=Expr[{}]", item.high->value)
                            : std::string{},
                        item.weight.has_value()
                            ? std::format(
                                  " weight{}=Expr[{}]",
                                  item.weight_per_range ? "/" : "=",
                                  item.weight->value)
                            : std::string{}));
              }
              if (d.default_weight.has_value()) {
                Line(
                    std::format(
                        "default weight/=Expr[{}]", d.default_weight->value));
              }
              Dedent();
            },
            [&](const ImplicationConstraint& i) {
              Line(std::format("Constraint[{}] Implication", id.value));
              Indent();
              expr(i.predicate);
              DumpConstraint(p, i.body);
              Dedent();
            },
            [&](const ConditionalConstraint& c) {
              Line(std::format("Constraint[{}] Conditional", id.value));
              Indent();
              expr(c.predicate);
              DumpConstraint(p, c.then_body);
              if (c.else_body.has_value()) {
                Line("else:");
                DumpConstraint(p, *c.else_body);
              }
              Dedent();
            },
            [&](const UniquenessConstraint& u) {
              Line(std::format("Constraint[{}] Unique", id.value));
              Indent();
              for (const ExprId item : u.items) {
                expr(item);
              }
              Dedent();
            },
            [&](const ForeachConstraint& f) {
              std::string loop_vars;
              for (const std::optional<ProceduralVarId>& var : f.loop_vars) {
                if (!loop_vars.empty()) loop_vars += ", ";
                loop_vars += var.has_value()
                                 ? std::format("ProceduralVar[{}]", var->value)
                                 : std::string{"-"};
              }
              Line(
                  std::format(
                      "Constraint[{}] Foreach loop_vars=[{}]", id.value,
                      loop_vars));
              Indent();
              expr(f.array);
              DumpConstraint(p, f.body);
              Dedent();
            },
            [&](const SolveBeforeConstraint& sb) {
              Line(
                  std::format(
                      "Constraint[{}] SolveBefore solve=[{}] after=[{}]",
                      id.value, FormatExprIds(sb.solve),
                      FormatExprIds(sb.after)));
            },
            [&](const DisableSoftConstraint& d) {
              Line(
                  std::format(
                      "Constraint[{}] DisableSoft target=Expr[{}]", id.value,
                      d.target.value));
            }},
        c.data);
  }

  static auto FormatLoopBodies(std::span<const LoopBody> bodies)
      -> std::string {
    std::string out;
    for (const LoopBody& built : bodies) {
      std::string indices;
      for (const std::int64_t index : built.indices) {
        indices += std::format("{}{}", indices.empty() ? "" : ",", index);
      }
      out += std::format(
          " body{}{}@[{}]", built.body.value,
          built.body_arguments.has_value()
              ? std::format("#{:016x}", *built.body_arguments)
              : std::string{},
          indices);
    }
    return out;
  }

  void DumpGenerate(const Generate& g) {
    const std::string form = std::visit(
        Overloaded{
            [](const SingleBlock&) { return std::string{"single"}; },
            [](const BlocksRepeat& r) {
              return std::format(
                  "repeated var=StructuralDataObject[{}] initial=Expr[{}] "
                  "condition=Expr[{}] step=Expr[{}] {}{}",
                  r.variable.value, r.initial.value, r.condition.value,
                  r.step.value, FormatArguments(r.arguments),
                  FormatLoopBodies(r.bodies));
            },
            [](const BlocksChoose& c) {
              std::string chosen =
                  std::format("chosen {}", FormatSelectionChoice(c, c.root));
              for (std::size_t at = 0; at < c.alternatives.size(); ++at) {
                chosen += c.alternatives[at].has_value()
                              ? std::format(
                                    " alt{}:{}", at, c.alternatives[at]->value)
                              : std::format(" alt{}:-", at);
              }
              return chosen;
            },
        },
        g.counting);
    Line(std::format("Generate blocks={} {}", g.blocks.size(), form));
    Indent();
    for (const StructuralScopeId id : g.blocks.Ids()) {
      Line(std::format("body{}:", id.value));
      Indent();
      DumpScope(g.blocks.Get(id).scope);
      Dedent();
    }
    Dedent();
  }

  std::string out_;
  int indent_ = 0;
  std::vector<const StructuralScope*> scope_stack_;
  std::map<ClassId, std::vector<const StructuralScope*>> class_scope_chains_;
  std::map<CovergroupId, std::vector<const StructuralScope*>>
      covergroup_scope_chains_;
  const CompilationUnit* unit_ = nullptr;
};

auto HirDumper::FormatSelectionChoice(
    const BlocksChoose& chosen, SelectionChoiceId at) -> std::string {
  const auto labels = [](const std::vector<ExprId>& of) {
    std::string out;
    for (const ExprId label : of) {
      if (!out.empty()) out += ",";
      out += std::format("Expr[{}]", label.value);
    }
    return out;
  };
  return std::visit(
      Overloaded{
          [&](const ChoiceOnCondition& on) {
            return std::format(
                "(if Expr[{}] {} else {})", on.condition.value,
                FormatSelectionBranch(chosen, on.holds),
                FormatSelectionBranch(chosen, on.fails));
          },
          [&](const ChoiceOnLabel& on) {
            std::string out = std::format("(case Expr[{}]", on.selector.value);
            for (const LabeledItem& item : on.items) {
              out += std::format(
                  " {}=>{}", labels(item.labels),
                  FormatSelectionBranch(chosen, item.stands));
            }
            out += std::format(
                " default=>{})", FormatSelectionBranch(chosen, on.otherwise));
            return out;
          },
      },
      chosen.choices.Get(at));
}

}  // namespace

auto DumpHir(const CompilationUnit& unit) -> std::string {
  HirDumper dumper;
  return dumper.Dump(unit);
}

}  // namespace lyra::hir
