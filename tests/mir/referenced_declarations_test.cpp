#include "lyra/mir/referenced_declarations.hpp"

#include <algorithm>
#include <gtest/gtest.h>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/diag/source_span.hpp"
#include "lyra/mir/builder.hpp"
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
#include "lyra/mir/type_builders.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/def_path.hpp"
#include "lyra/support/library_decl.hpp"

namespace lyra::mir {
namespace {

auto PathOf(std::string name) -> support::DefPath {
  return support::DefPath{
      .data = {support::ClassStep{
          .name = std::move(name), .arguments = std::nullopt}}};
}

// A unit `U` declaring two classes package `P` defines, `Derived` extending
// `Base`, and holding one body of its namespace, `f`, which `write_body` writes
// into. `Derived` sits at identity 1 and `Base` at 0.
template <typename WriteBody>
auto UnitWithBody(const WriteBody& write_body) -> CompilationUnit {
  CompilationUnit unit;
  unit.name = "U";
  const ClassId base = unit.DeclareClass();
  const ClassId derived = unit.DeclareClass();
  Class base_class;
  base_class.defining_unit = "P";
  base_class.identity = PathOf("Base");
  base_class.base = ClassRef{ClassObjectRootRef{}};
  base_class.fields.Add(FieldDecl{.type = unit.builtins.int_type});
  unit.DefineClass(base, std::move(base_class));
  Class derived_class;
  derived_class.defining_unit = "P";
  derived_class.identity = PathOf("Derived");
  derived_class.base = ClassRef{base};
  unit.DefineClass(derived, std::move(derived_class));
  CallableCode code = CallableCode::Defined(diag::SourceSpan{});
  code.result_type = unit.builtins.void_type;
  write_body(unit, Builder(code.Body(), diag::SourceSpan{}), derived);
  unit.callables.Add(
      CallableDecl{
          .code = std::move(code),
          .foreign = std::nullopt,
          .virtual_dispatch = std::nullopt,
          .defining_unit = std::nullopt});
  return unit;
}

auto HandleTo(CompilationUnit& unit, ClassId cls) -> TypeId {
  return unit.types.Intern(
      Type{ManagedRefType{
          .pointee = unit.types.Intern(Type{ObjectType{.of = cls}})}});
}

auto Classes(const std::vector<ReferencedDeclaration>& referenced)
    -> std::vector<std::string> {
  std::vector<std::string> names;
  for (const ReferencedDeclaration& named : referenced) {
    if (const auto* cls = std::get_if<ReferencedClass>(&named)) {
      names.push_back(
          cls->unit_name + "::" + support::DisplayOf(cls->class_path));
    }
  }
  return names;
}

// Holding a handle names nothing of its class, so a body that only holds one
// names no other unit; a class it names is named with every class it extends,
// whose layout and dispatch positions it follows.
TEST(
    ReferencedDeclarationsTest,
    AHandleNamesNothingAndANamedClassBringsItsLineage) {
  const CompilationUnit holding = UnitWithBody([](CompilationUnit& u,
                                                  Builder body,
                                                  ClassId derived) {
    body.AppendStmt(
        ExprStmt{
            .expr = body.Add(
                Expr{.data = NullLiteral{}, .type = HandleTo(u, derived)})});
  });
  EXPECT_TRUE(ReferencedDeclarationsOf(holding).empty());

  const CompilationUnit reaching =
      UnitWithBody([](CompilationUnit& u, Builder body, ClassId derived) {
        const ExprId object = body.Add(
            Expr{
                .data = NullLiteral{},
                .type = u.types.Intern(Type{ObjectType{.of = derived}})});
        body.AppendStmt(
            ExprStmt{
                .expr = body.Read(body.Add(MakeFieldAccessExpr(
                    object,
                    ClassFieldTarget{
                        .owner = ClassId{.value = 0}, .slot = FieldId{}},
                    u.builtins.int_type)))});
      });
  EXPECT_EQ(
      Classes(ReferencedDeclarationsOf(reaching)),
      (std::vector<std::string>{"P::Derived", "P::Base"}));
}

// Each way a body reaches into a class names it, through the node that
// reaches it and nothing else, with every class it extends.
TEST(ReferencedDeclarationsTest, EveryReachIntoAClassNamesItWithItsLineage) {
  const auto names_derived = [](const auto& reach) {
    const CompilationUnit unit =
        UnitWithBody([&](CompilationUnit& u, Builder body, ClassId derived) {
          body.AppendStmt(ExprStmt{.expr = body.Add(reach(u, body, derived))});
        });
    return Classes(ReferencedDeclarationsOf(unit)) ==
           std::vector<std::string>{"P::Derived", "P::Base"};
  };
  EXPECT_TRUE(names_derived([](CompilationUnit& u, Builder body, ClassId cls) {
    const ExprId object =
        body.Add(Expr{.data = NullLiteral{}, .type = u.builtins.int_type});
    return MakeFieldAccessExpr(
        object, ClassFieldTarget{.owner = cls, .slot = FieldId{}},
        u.builtins.int_type);
  })) << "a member accessed";
  EXPECT_TRUE(names_derived([](CompilationUnit& u, Builder body, ClassId cls) {
    const ExprId receiver =
        body.Add(Expr{.data = NullLiteral{}, .type = u.builtins.int_type});
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
  EXPECT_TRUE(names_derived([](CompilationUnit& u, Builder, ClassId cls) {
    return MakeCallExpr(
        Direct{.target = ConstructorTarget{.cls = cls}}, {},
        u.builtins.void_type);
  })) << "a construction entered";
  EXPECT_TRUE(names_derived([](CompilationUnit& u, Builder, ClassId cls) {
    return Expr{
        .data =
            ReferenceExpr{
                .target =
                    StaticPropertyRef{
                        .owner = cls, .prop = StaticPropertyId{}}},
        .type = u.builtins.int_type};
  })) << "a static property";
}

// What a class this unit defines holds is named where the class owns it, and
// a value of a record, a variable or an entry another unit declares is named
// where a body names it.
TEST(ReferencedDeclarationsTest, WhatIsHeldOrNamedOfAnotherUnitIsReferenced) {
  CompilationUnit owning =
      UnitWithBody([](CompilationUnit&, Builder, ClassId) {});
  Class holder;
  holder.base = ClassRef{ClassObjectRootRef{}};
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
      Classes(ReferencedDeclarationsOf(owning)),
      (std::vector<std::string>{"P::Derived", "P::Base"}));

  const CompilationUnit naming = UnitWithBody([](CompilationUnit& u,
                                                 Builder body, ClassId) {
    const RecordId record = u.records.Add(
        RecordDecl{
            .defining_unit = "R",
            .path = PathOf("S"),
            .kind = RecordKind::kStruct,
            .elements = {}});
    body.AppendStmt(
        ExprStmt{
            .expr = body.Add(
                Expr{
                    .data = NullLiteral{},
                    .type = u.types.Intern(
                        Type{RecordType{.declaration = record}})})});
    const StaticVariableId variable = u.static_variables.Add(
        StaticVariableDecl{.type = u.builtins.int_type, .defining_unit = "Q"});
    body.AppendStmt(
        ExprStmt{
            .expr = body.Add(
                Expr{
                    .data =
                        ReferenceExpr{
                            .target = StaticVariableRef{.variable = variable}},
                    .type = u.builtins.int_type})});
    body.AppendStmt(
        ExprStmt{
            .expr = body.Add(
                Expr{
                    .data =
                        ReferenceExpr{
                            .target =
                                ExternalObjectEntryRef{
                                    .unit_name = "M",
                                    .body_name = "M",
                                    .block = std::nullopt}},
                    .type = u.builtins.void_type})});
  });
  EXPECT_EQ(
      ReferencedDeclarationsOf(naming),
      (std::vector<ReferencedDeclaration>{
          ReferencedNamespace{.unit_name = "R"},
          ReferencedNamespace{.unit_name = "Q"},
          ReferencedObjectEntry{.unit_name = "M", .body_name = "M"}}));
}

// A handle seen as another class's is the same handle (LRM 8.15), and a `$cast`
// asks the object whether it is of the class it names (LRM 8.16) by that
// class's definition alone; neither names anything the classes declare.
TEST(ReferencedDeclarationsTest, AConversionBetweenClassesNamesNeither) {
  const CompilationUnit seen_as =
      UnitWithBody([](CompilationUnit& u, Builder body, ClassId derived) {
        const ExprId handle =
            body.Add(Expr{.data = NullLiteral{}, .type = HandleTo(u, derived)});
        body.AppendStmt(
            ExprStmt{
                .expr = body.Add(
                    Expr{
                        .data = CastExpr{.operand = handle},
                        .type = HandleTo(u, ClassId{.value = 0})})});
      });
  EXPECT_TRUE(Classes(ReferencedDeclarationsOf(seen_as)).empty());

  const CompilationUnit cast = UnitWithBody([](CompilationUnit& u, Builder body,
                                               ClassId derived) {
    const TypeId base_handle = HandleTo(u, ClassId{.value = 0});
    const ExprId handle =
        body.Add(Expr{.data = NullLiteral{}, .type = base_handle});
    const ExprId definition = body.Add(MakeAddressOfExpr(
        body.Add(
            Expr{
                .data = ReferenceExpr{.target = DefinitionRef{.of = derived}},
                .type = LibraryTypeOf(
                    u.types, support::LibraryDecl::kObjectDefinition)}),
        ClassDefinitionPointer(u.types)));
    body.AppendStmt(
        ExprStmt{
            .expr = body.Add(MakeCallExpr(
                Direct{.target = support::BuiltinFn::kDynamicCast},
                {handle, definition}, HandleTo(u, derived)))});
  });
  EXPECT_TRUE(Classes(ReferencedDeclarationsOf(cast)).empty());
}

// A subroutine of another unit's namespace is referenced where a body names it,
// and the unit's own namespace is never named as another's.
TEST(
    ReferencedDeclarationsTest,
    ANamespaceMemberOfAnotherUnitIsReferencedWhereNamed) {
  const CompilationUnit calling = UnitWithBody([](CompilationUnit& u,
                                                  Builder body, ClassId) {
    const CallableId declared = u.callables.Add(
        CallableDecl{
            .code = CallableCode{},
            .foreign = std::nullopt,
            .virtual_dispatch = std::nullopt,
            .defining_unit = "Q"});
    u.named_callables.push_back(NamedCallable{.name = "g", .body = declared});
    body.AppendStmt(
        ExprStmt{
            .expr = body.Add(MakeCallExpr(
                Direct{.target = UnitCallableTarget{.slot = declared}}, {},
                u.builtins.void_type))});
    body.AppendStmt(
        ExprStmt{
            .expr = body.Add(MakeCallExpr(
                Direct{
                    .target =
                        ExternalBringUpTarget{
                            .unit_name = u.name,
                            .entry = BringUpEntry::kInstallStorage}},
                {}, u.builtins.void_type))});
  });
  EXPECT_EQ(
      ReferencedDeclarationsOf(calling),
      (std::vector<ReferencedDeclaration>{
          ReferencedNamespace{.unit_name = "Q"}}));
}

}  // namespace
}  // namespace lyra::mir
