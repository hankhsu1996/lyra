#include "lyra/mir/consumed_signatures.hpp"

#include <algorithm>
#include <gtest/gtest.h>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/mir/callable.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/record_decl.hpp"
#include "lyra/mir/record_id.hpp"
#include "lyra/mir/static_property_id.hpp"
#include "lyra/mir/static_variable_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/def_path.hpp"

namespace lyra::mir {
namespace {

auto PathOf(std::string name) -> support::DefPath {
  return support::DefPath{
      .data = {support::ClassStep{
          .name = std::move(name), .arguments = std::nullopt}}};
}

// A unit `U` declaring two classes package `P` defines, `Derived` extending
// `Base`, and holding one body of its namespace, `f`, which `read` writes into.
// `Derived` sits at identity 1 and `Base` at 0.
template <typename Read>
auto UnitReading(const Read& read) -> CompilationUnit {
  CompilationUnit unit;
  unit.name = "U";
  const ClassId base = unit.DeclareClass();
  const ClassId derived = unit.DeclareClass();
  Class base_class;
  base_class.defining_unit = "P";
  base_class.path = PathOf("Base");
  base_class.base = ClassRef{ManagedObjectRootRef{}};
  base_class.fields.Add(FieldDecl{.type = unit.builtins.int_type});
  unit.DefineClass(base, std::move(base_class));
  Class derived_class;
  derived_class.defining_unit = "P";
  derived_class.path = PathOf("Derived");
  derived_class.base = ClassRef{base};
  unit.DefineClass(derived, std::move(derived_class));
  CallableCode code = CallableCode::Defined();
  code.result_type = unit.builtins.void_type;
  read(unit, code.Body(), derived);
  unit.callables.Add(
      CallableDecl{
          .code = std::move(code),
          .foreign = std::nullopt,
          .virtual_dispatch = std::nullopt,
          .type_parameters = {},
          .defining_unit = std::nullopt});
  return unit;
}

auto HandleTo(CompilationUnit& unit, ClassId cls) -> TypeId {
  return unit.types.Intern(
      Type{ManagedRefType{
          .pointee = unit.types.Intern(Type{ObjectType{.of = cls}})}});
}

auto Classes(const std::vector<ConsumedSignature>& consumed)
    -> std::vector<std::string> {
  std::vector<std::string> names;
  for (const ConsumedSignature& read : consumed) {
    if (const auto* cls = std::get_if<ConsumedClass>(&read)) {
      names.push_back(
          cls->unit_name + "::" + support::DisplayOf(cls->class_path));
    }
  }
  return names;
}

// Holding a handle reads nothing of its class, so a body that only holds one
// depends on no unit; a class it reads is read with every class it extends,
// whose layout and dispatch positions it follows.
TEST(ConsumedSignaturesTest, AHandleReadsNothingAndAReadClassBringsItsLineage) {
  const CompilationUnit holding = UnitReading([](CompilationUnit& u,
                                                 Block& body, ClassId derived) {
    body.AppendStmt(
        ExprStmt{
            .expr = body.exprs.Add(
                Expr{.data = NullLiteral{}, .type = HandleTo(u, derived)})});
  });
  EXPECT_TRUE(ConsumedSignaturesOf(holding).empty());

  const CompilationUnit reaching =
      UnitReading([](CompilationUnit& u, Block& body, ClassId derived) {
        const ExprId object = body.exprs.Add(
            Expr{
                .data = NullLiteral{},
                .type = u.types.Intern(Type{ObjectType{.of = derived}})});
        body.AppendStmt(
            ExprStmt{
                .expr = body.Read(body.exprs.Add(MakeFieldAccessExpr(
                    object,
                    ClassFieldTarget{
                        .owner = ClassId{.value = 0}, .slot = FieldId{}},
                    u.builtins.int_type)))});
      });
  EXPECT_EQ(
      Classes(ConsumedSignaturesOf(reaching)),
      (std::vector<std::string>{"P::Derived", "P::Base"}));
}

// Each way a body reaches into a class reads it, through the node that
// reaches it and nothing else, with every class it extends.
TEST(ConsumedSignaturesTest, EveryReachIntoAClassReadsItWithItsLineage) {
  const auto reads_derived = [](const auto& reach) {
    const CompilationUnit unit =
        UnitReading([&](CompilationUnit& u, Block& body, ClassId derived) {
          body.AppendStmt(
              ExprStmt{.expr = body.exprs.Add(reach(u, body, derived))});
        });
    return Classes(ConsumedSignaturesOf(unit)) ==
           std::vector<std::string>{"P::Derived", "P::Base"};
  };
  EXPECT_TRUE(reads_derived([](CompilationUnit& u, Block& body, ClassId cls) {
    const ExprId object = body.exprs.Add(
        Expr{.data = NullLiteral{}, .type = u.builtins.int_type});
    return MakeFieldAccessExpr(
        object, ClassFieldTarget{.owner = cls, .slot = FieldId{}},
        u.builtins.int_type);
  })) << "a member accessed";
  EXPECT_TRUE(reads_derived([](CompilationUnit& u, Block& body, ClassId cls) {
    const ExprId receiver = body.exprs.Add(
        Expr{.data = NullLiteral{}, .type = u.builtins.int_type});
    return Expr{
        .data =
            CallExpr{
                .callee =
                    Virtual{
                        .receiver = receiver,
                        .slot =
                            VirtualSlot{
                                .owner_class = cls, .slot = CallableId{}}},
                .arguments = {}},
        .type = u.builtins.void_type};
  })) << "a behavior dispatched on";
  EXPECT_TRUE(reads_derived([](CompilationUnit& u, Block&, ClassId cls) {
    return MakeCallExpr(
        Direct{.target = ConstructorTarget{.cls = cls}}, {},
        u.builtins.void_type);
  })) << "a construction entered";
  EXPECT_TRUE(reads_derived([](CompilationUnit& u, Block&, ClassId cls) {
    return Expr{
        .data =
            ReferenceExpr{
                .target =
                    StaticPropertyRef{
                        .owner = cls, .prop = StaticPropertyId{}}},
        .type = u.builtins.int_type};
  })) << "a static property";
}

// What a class this unit defines holds is read where the class owns it, and
// a value of a record, a variable or an entry another unit declares is read
// where a body names it.
TEST(ConsumedSignaturesTest, WhatIsHeldOrNamedOfAnotherUnitIsRead) {
  CompilationUnit owning =
      UnitReading([](CompilationUnit&, Block&, ClassId) {});
  Class holder;
  holder.base = ClassRef{ManagedObjectRootRef{}};
  holder.fields.Add(
      FieldDecl{
          .type = owning.types.Intern(
              Type{PointerType{
                  .pointee = owning.types.Intern(
                      Type{ObjectType{.of = ClassId{.value = 1}}}),
                  .ownership = PointerOwnership::kUnique}})});
  const ClassId held_by = owning.DeclareClass();
  owning.DefineClass(held_by, std::move(holder));
  EXPECT_EQ(
      Classes(ConsumedSignaturesOf(owning)),
      (std::vector<std::string>{"P::Derived", "P::Base"}));

  const CompilationUnit naming = UnitReading([](CompilationUnit& u, Block& body,
                                                ClassId) {
    const RecordId record = u.records.Add(
        RecordDecl{
            .defining_unit = "R",
            .path = PathOf("S"),
            .kind = RecordKind::kStruct,
            .elements = {}});
    body.AppendStmt(
        ExprStmt{
            .expr = body.exprs.Add(
                Expr{
                    .data = NullLiteral{},
                    .type = u.types.Intern(
                        Type{RecordType{.declaration = record}})})});
    const StaticVariableId variable = u.static_variables.Add(
        StaticVariableDecl{.type = u.builtins.int_type, .defining_unit = "Q"});
    body.AppendStmt(
        ExprStmt{
            .expr = body.exprs.Add(
                Expr{
                    .data =
                        ReferenceExpr{
                            .target = StaticVariableRef{.variable = variable}},
                    .type = u.builtins.int_type})});
    body.AppendStmt(
        ExprStmt{
            .expr = body.exprs.Add(
                Expr{
                    .data =
                        ReferenceExpr{
                            .target =
                                ExternalObjectEntryRef{
                                    .unit_name = "M", .body_name = "M"}},
                    .type = u.builtins.void_type})});
  });
  EXPECT_EQ(
      ConsumedSignaturesOf(naming),
      (std::vector<ConsumedSignature>{
          ConsumedNamespace{.unit_name = "R"},
          ConsumedNamespace{.unit_name = "Q"},
          ConsumedObjectEntry{.unit_name = "M", .body_name = "M"}}));
}

// A conversion between two class handles reads both classes, whatever the
// body otherwise reaches of them (LRM 8.15, 8.16).
TEST(ConsumedSignaturesTest, AConversionBetweenClassesReadsBoth) {
  for (const support::BuiltinFn conversion :
       {support::BuiltinFn::kDerivedToBase, support::BuiltinFn::kDynamicCast}) {
    const CompilationUnit converting = UnitReading([conversion](
                                                       CompilationUnit& u,
                                                       Block& body,
                                                       ClassId derived) {
      const TypeId base_handle = HandleTo(u, ClassId{.value = 0});
      const ExprId handle = body.exprs.Add(
          Expr{.data = NullLiteral{}, .type = HandleTo(u, derived)});
      body.AppendStmt(
          ExprStmt{
              .expr = body.exprs.Add(MakeCallExpr(
                  Direct{.target = conversion, .type_arguments = {base_handle}},
                  {handle}, base_handle))});
    });
    const std::vector<std::string> read =
        Classes(ConsumedSignaturesOf(converting));
    EXPECT_EQ(read.size(), 2U);
    EXPECT_NE(std::ranges::find(read, "P::Derived"), read.end());
    EXPECT_NE(std::ranges::find(read, "P::Base"), read.end());
  }
}

// A subroutine of another unit's namespace is read where a body names it, and
// the unit's own namespace is no dependency of itself.
TEST(ConsumedSignaturesTest, ANamespaceMemberOfAnotherUnitIsReadWhereNamed) {
  const CompilationUnit calling = UnitReading([](CompilationUnit& u,
                                                 Block& body, ClassId) {
    const CallableId declared = u.callables.Add(
        CallableDecl{
            .code = CallableCode{},
            .foreign = std::nullopt,
            .virtual_dispatch = std::nullopt,
            .type_parameters = {},
            .defining_unit = "Q"});
    u.named_callables.push_back(NamedCallable{.name = "g", .body = declared});
    body.AppendStmt(
        ExprStmt{
            .expr = body.exprs.Add(MakeCallExpr(
                Direct{.target = UnitCallableTarget{.slot = declared}}, {},
                u.builtins.void_type))});
    body.AppendStmt(
        ExprStmt{
            .expr = body.exprs.Add(MakeCallExpr(
                Direct{
                    .target =
                        ExternalBringUpTarget{
                            .unit_name = u.name,
                            .entry = BringUpEntry::kInstallStorage}},
                {}, u.builtins.void_type))});
  });
  EXPECT_EQ(
      ConsumedSignaturesOf(calling),
      (std::vector<ConsumedSignature>{ConsumedNamespace{.unit_name = "Q"}}));
}

}  // namespace
}  // namespace lyra::mir
