#include "lyra/lowering/hir_to_mir/default_value.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <format>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/real_bits.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/type_definition.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/container_construction.hpp"
#include "lyra/lowering/hir_to_mir/expression/references.hpp"
#include "lyra/lowering/hir_to_mir/real_literal.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/library_decl.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::lowering::hir_to_mir {

// LRM Table 6-7: a 4-state integral type defaults to all `x`, a 2-state one to
// all zero.
auto DefaultIntegralConstant(const mir::IntegralType& integral)
    -> mir::IntegralConstant {
  mir::IntegralConstant held = mir::BlankIntegralConstant(integral);
  value::FillDefault(
      value::IntegralView{
          .value = held.value_words,
          .unknown = held.state_words,
          .width = integral.bit_width});
  return held;
}

namespace {

// Materialize a folded member-default constant (LRM 7.2.2) as a MIR value of
// its type. A scalar leaf becomes a literal (a string constructs the runtime
// String); an unpacked struct is its recursively materialized components, and
// an unpacked array is the construction that takes them, with the type
// deciding which of the two a component list is.
auto MaterializeConstant(
    const UnitLowerer& unit_lowerer, mir::Builder block, hir::TypeId hir_type,
    const hir::ConstantValue& value) -> mir::Expr {
  const mir::TypeId mir_type = unit_lowerer.TranslateType(hir_type);
  return std::visit(
      Overloaded{
          [&](const hir::IntegralConstant& c) -> mir::Expr {
            return block.Get(
                mir::BuildIntegralLiteral(
                    unit_lowerer.Unit(), block, mir_type,
                    LowerHirIntegralConstant(c)));
          },
          [&](hir::RealBits real) -> mir::Expr {
            return block.Get(BuildRealLiteral(
                unit_lowerer.Unit(), block, mir_type, real.Value()));
          },
          [&](const std::string& text) -> mir::Expr {
            const mir::ExprId literal = block.Add(
                mir::Expr{
                    .data = mir::StringLiteral{.value = text},
                    .type = mir_type});
            return mir::Expr{
                .data =
                    mir::CallExpr{
                        .callee = mir::Construct{}, .arguments = {literal}},
                .type = mir_type};
          },
          [&](const std::vector<hir::ConstantValue>& components) -> mir::Expr {
            const auto& hir_ty = unit_lowerer.Hir().types.Get(hir_type);
            if (const auto* declared = hir_ty.As<hir::UnpackedStructType>()) {
              const hir::UnpackedStructDefinition& structure =
                  unit_lowerer.Hir().DefinitionOf(*declared);
              std::vector<mir::ExprId> component_ids;
              component_ids.reserve(components.size());
              for (std::size_t i = 0; i < components.size(); ++i) {
                component_ids.push_back(block.Add(MaterializeConstant(
                    unit_lowerer, block, structure.fields[i].type,
                    components[i])));
              }
              return mir::Expr{
                  .data = mir::CompositeExpr{.parts = std::move(component_ids)},
                  .type = mir_type};
            }
            if (const auto* ua = hir_ty.As<hir::UnpackedArrayType>()) {
              std::vector<mir::ExprId> element_ids;
              element_ids.reserve(components.size());
              for (const auto& component : components) {
                element_ids.push_back(block.Add(MaterializeConstant(
                    unit_lowerer, block, ua->element_type, component)));
              }
              return BuildArrayConstructionCall(
                  unit_lowerer.Unit(), block, mir_type, std::move(element_ids));
            }
            throw InternalError(
                "MaterializeConstant: aggregate value for a non-aggregate "
                "type");
          },
      },
      value.data);
}

}  // namespace

auto DefaultValueOf(
    const mir::CompilationUnit& unit, mir::Builder block, mir::TypeId type)
    -> std::expected<mir::Expr, std::string_view> {
  using Answer = std::expected<mir::Expr, std::string_view>;
  const auto& ty = unit.types.Get(type);
  const auto component_wise =
      [&](std::span<const mir::TypeId> components) -> Answer {
    std::vector<mir::ExprId> parts;
    parts.reserve(components.size());
    for (const mir::TypeId component : components) {
      Answer part = DefaultValueOf(unit, block, component);
      if (!part) {
        return part;
      }
      parts.push_back(block.Add(*std::move(part)));
    }
    return mir::Expr{
        .data = mir::CompositeExpr{.parts = std::move(parts)}, .type = type};
  };
  // LRM Table 6-7 and Table 7-1 answer for the types a variable, a member, or
  // an element is declared to hold, and that is the whole of what is ever asked
  // here. The rest of the vocabulary is what a lowering builds to carry one of
  // those or to reach it, so a value of it starts out wherever the step that
  // built it put it, and being asked what one starts out as means a producer
  // handed over a type no declaration names.
  const auto holds_no_declared_value = [](std::string_view what) -> Answer {
    return std::unexpected(what);
  };
  return ty.Visit(
      Overloaded{
          [&](const mir::IntegralType& integral) -> Answer {
            return block.Get(
                mir::BuildIntegralLiteral(
                    unit, block, type, DefaultIntegralConstant(integral)));
          },
          // An enumeration defaults as the vector it is: LRM Table 6-7 reads
          // the default off the state domain, which the base carries and the
          // names do not affect.
          [&](const mir::EnumType& e) -> Answer {
            return block.Get(
                mir::BuildIntegralLiteral(
                    unit, block, type, DefaultIntegralConstant(e.base)));
          },
          [&](const mir::RealType&) -> Answer {
            return block.Get(BuildRealLiteral(unit, block, type, 0.0));
          },
          // LRM Table 7-1: a fixed unpacked array defaults to every element at
          // the element type's default. One element default fills the array,
          // so stating it stays O(1) in the array's element count.
          [&](const mir::UnpackedArrayType& ua) -> Answer {
            Answer element = DefaultValueOf(unit, block, ua.element_type);
            if (!element) {
              return element;
            }
            return BuildFixedArrayConstructionCall(
                unit, block, type, {block.Add(*std::move(element))});
          },
          // LRM Table 7-1: a product no declaration names defaults
          // component-wise -- each takes its own type's default, recursively.
          [&](const mir::TupleType& t) -> Answer {
            return component_wise(t.elements);
          },
          // A structure or a union states its own default: a structure's
          // members at their declared initial values (LRM 7.2.2), a union's
          // first member at its default (LRM Table 7-1).
          [&](const mir::RecordType& s) -> Answer {
            return mir::Expr{
                .data =
                    mir::CallExpr{
                        .callee =
                            mir::Direct{
                                .target =
                                    mir::RecordMethodTarget{
                                        .record = s.declaration,
                                        .answers = support::BuiltinFn::
                                            kDeclaredDefault}},
                        .arguments = {}},
                .type = type};
          },
          // What a declaration holds for an object whose class this unit does
          // not declare is a handle to it, and a handle starts out null; the
          // object type itself is what that handle points at, which a
          // construction brings into existence.
          [&](const mir::ObjectType& o) -> Answer {
            if (mir::IsDefinedElsewhere(unit, o.of)) {
              return holds_no_declared_value(
                  "an object of another unit's class");
            }
            return mir::Expr{
                .data =
                    mir::CallExpr{.callee = mir::Construct{}, .arguments = {}},
                .type = type};
          },
          [&](const mir::PointerType&) -> Answer {
            return mir::Expr{.data = mir::NullLiteral{}, .type = type};
          },
          // A reference is bound where it is declared and is never null.
          [&](const mir::ReferenceType&) -> Answer {
            return holds_no_declared_value("a reference");
          },
          // LRM 8.4 / Table 7-1: an uninitialized class handle defaults to
          // null, and so does an event read where an array has no entry. A
          // declared event names a new object of its own (Table 6-7), which a
          // declaration states on top of this.
          [&](const mir::ManagedRefType&) -> Answer {
            return mir::Expr{.data = mir::NullLiteral{}, .type = type};
          },
          // A type the library declares starts as what the entry its
          // declaration names builds out of nothing, and a type it names none
          // for holds nothing a declaration could ask for.
          [&](const mir::LibraryType& t) -> Answer {
            const support::LibraryDeclData& declaration =
                support::DeclarationOf(t.decl);
            if (!declaration.default_value.has_value()) {
              return holds_no_declared_value(declaration.name);
            }
            return mir::Expr{
                .data =
                    mir::CallExpr{
                        .callee = mir::BuiltinCalleeAnswering(
                            *declaration.default_value, std::nullopt, type),
                        .arguments = {}},
                .type = type};
          },
          // LRM 6.14: a chandle is always initialized to null, and so is a
          // virtual interface (LRM 25.9).
          [&](const mir::UnmanagedRefType&) -> Answer {
            return mir::Expr{.data = mir::NullLiteral{}, .type = type};
          },
          // A declaration standing for no object holds a sequence of nothing,
          // which its own constructor builds from a list of no elements.
          [&](const mir::VectorType&) -> Answer {
            return BuildSequenceConstructionCall(unit, block, type, {});
          },

          // The machine vocabulary a lowering builds to carry a value: a count,
          // a flag, a host string, a sequence of operands, the address of a
          // body. Each is built where it is used, out of what it is built from.
          [&](const mir::MachineIntType&) -> Answer {
            return holds_no_declared_value("a machine integer");
          },
          [&](const mir::MachineFloatType&) -> Answer {
            return holds_no_declared_value("a machine floating-point number");
          },
          [&](const mir::MachineBoolType&) -> Answer {
            return holds_no_declared_value("a machine boolean");
          },
          [&](const mir::MachineCStringType&) -> Answer {
            return holds_no_declared_value("a machine string");
          },
          [&](const mir::MachineArrayType&) -> Answer {
            return holds_no_declared_value("a machine array of operands");
          },
          [&](const mir::MachineSpanType&) -> Answer {
            return holds_no_declared_value("a machine span of entries");
          },
          [&](const mir::MachineFunctionType&) -> Answer {
            return holds_no_declared_value("a machine function address");
          },

          // Storage the runtime installs: a cell that is observed, a net's
          // resolution, a driver's contribution, and a write open on one of
          // them. What a declaration holds is the value inside one of these,
          // which answers above.
          [&](const mir::ObservableType&) -> Answer {
            return holds_no_declared_value("an observable cell");
          },
          [&](const mir::ResolvedType&) -> Answer {
            return holds_no_declared_value("a net's resolution");
          },
          [&](const mir::DriverType&) -> Answer {
            return holds_no_declared_value("a driver's contribution");
          },
          [&](const mir::OpenWriteType&) -> Answer {
            return holds_no_declared_value("a write in progress");
          },
          [&](const mir::DesignationType&) -> Answer {
            return holds_no_declared_value("a part a write designates");
          },
          [&](const mir::BitsReferenceType&) -> Answer {
            return holds_no_declared_value("a reference to bits");
          },
          [&](const mir::ObjectWriteType&) -> Answer {
            return holds_no_declared_value("a write into an object");
          },

          // Code, and the storage a body reaches through rather than holds: a
          // suspending body's frame, a closure's, and a reference bound to a
          // cell that already exists.
          [&](const mir::CoroutineType&) -> Answer {
            return holds_no_declared_value("a suspending body");
          },
          [&](const mir::ClosureType&) -> Answer {
            return holds_no_declared_value("a closure");
          },
          [&](const mir::ErasedCallableType&) -> Answer {
            return holds_no_declared_value("a closure of no known type");
          },
          [&](const mir::ErasedValueType&) -> Answer {
            return holds_no_declared_value("a value of no known type");
          },
          [&](const mir::VariableRefType&) -> Answer {
            return holds_no_declared_value("a reference to a cell");
          },
          [&](const mir::ReferenceWrapperType&) -> Answer {
            return holds_no_declared_value("a reference to a cell");
          },

          // LRM 7.3.2's `void` member occupies a value slot and is carried as
          // the tuple of no components, which answers above. Everywhere else
          // `void` is the absence of a result, and an absence is not declared.
          [&](const mir::VoidType&) -> Answer {
            return holds_no_declared_value("the absence of a result");
          },
      });
}

auto BuildDefaultValueExpr(
    const mir::CompilationUnit& unit, mir::Builder block, mir::TypeId type)
    -> mir::Expr {
  std::expected<mir::Expr, std::string_view> built =
      DefaultValueOf(unit, block, type);
  if (!built) {
    throw InternalError(
        std::format(
            "BuildDefaultValueExpr: {} is not what a declaration holds, so "
            "there is no initial value to state for it",
            built.error()));
  }
  return *std::move(built);
}

auto BuildDeclaredStructValue(
    const UnitLowerer& unit_lowerer, mir::Builder block,
    const hir::UnpackedStructDefinition& source, mir::TypeId structure)
    -> mir::Expr {
  std::vector<mir::ExprId> components;
  components.reserve(source.fields.size());
  for (const auto& field : source.fields) {
    components.push_back(
        field.default_init.has_value()
            ? block.Add(MaterializeConstant(
                  unit_lowerer, block, field.type, *field.default_init))
            : block.Add(
                  BuildDefaultValueFromHir(unit_lowerer, block, field.type)));
  }
  return mir::Expr{
      .data = mir::CompositeExpr{.parts = std::move(components)},
      .type = structure};
}

auto BuildDefaultValueFromHir(
    const UnitLowerer& unit_lowerer, mir::Builder block, hir::TypeId hir_type)
    -> mir::Expr {
  return BuildDefaultValueExpr(
      unit_lowerer.Unit(), block, unit_lowerer.TranslateType(hir_type));
}

}  // namespace lyra::lowering::hir_to_mir
