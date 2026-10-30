#pragma once

#include <cstdint>
#include <map>
#include <optional>
#include <span>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/registry.hpp"
#include "lyra/base/translation.hpp"
#include "lyra/lir/class_id.hpp"
#include "lyra/lir/closure_id.hpp"
#include "lyra/lir/enum_table.hpp"
#include "lyra/lir/enum_table_id.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/function_id.hpp"
#include "lyra/lir/integral_constant.hpp"
#include "lyra/lir/integral_constant_id.hpp"
#include "lyra/lir/linkage.hpp"
#include "lyra/lir/record_id.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/lir/value_step.hpp"
#include "lyra/support/class_name.hpp"
#include "lyra/support/def_path.hpp"
#include "lyra/support/value_operation.hpp"

namespace lyra::lir {

// A typed member of whatever declares it -- the storage a member place reaches
// by a member projection. Its position in the declaring list is its member
// identity there, and it carries no other, because nothing below here reaches a
// member by a name. Where it sits in the value is decided below this layer,
// when the declaration is laid out.
struct Member {
  TypeId type;
};

struct Constant;

// A machine integer, at the width of whatever it is placed in.
struct ConstantInt {
  std::int64_t value = 0;
};

// An address naming nothing.
struct ConstantNull {};

// The address of what ends an object of the class `cls`, of this unit or
// another: each member, then the storage the object was created in.
struct ConstantDestructor {
  TypeId cls;
};

// The address of the table an object of the class `cls`, of this unit, lays
// the bodies of its behaviors out in, laid out by the target from what the
// class states of its dispatch.
struct ConstantDispatchTable {
  TypeId cls;
};

// The same for the table it gives the interface class `iface`.
struct ConstantInterfaceTable {
  TypeId cls;
  TypeId iface;
};

// The address of a NUL-terminated string.
struct ConstantString {
  std::string text;
};

// The address of a function of this unit.
struct ConstantFunction {
  FunctionId function;
};

// The address of a function another unit emits, linked under `symbol`: an
// entry of that unit's that answers to no name, declared at `prototype`, the
// type that unit published it with.
struct ConstantExternalFunction {
  std::string symbol;
  TypeId prototype;
};

// The address of data linked under `symbol`: a constant of this unit, or a
// class's definition, which another unit may emit.
struct ConstantAddress {
  std::string symbol;
};

// A structure of the runtime library, its members given in the order the
// library declares them, a nested structure's members standing where it does.
struct ConstantRecord {
  RuntimeLibraryKind kind;
  std::vector<Constant> parts;
};

// Values of one type, one after another.
struct ConstantArray {
  std::vector<Constant> elements;
};

// A value fixed where the unit is compiled and built from nothing that runs --
// literals, addresses, and structures and arrays of them -- so a target emits
// it as data.
struct Constant {
  std::variant<
      ConstantInt, ConstantNull, ConstantDestructor, ConstantDispatchTable,
      ConstantInterfaceTable, ConstantString, ConstantFunction,
      ConstantExternalFunction, ConstantAddress, ConstantRecord, ConstantArray>
      value;
};

// Read-only data this unit defines, linked under `symbol` and holding
// `initializer` from before the program starts. Everything naming it -- another
// constant, or under external linkage another unit -- does so by that symbol,
// which is stated here and composed nowhere below.
struct GlobalConstant {
  std::string symbol;
  Linkage linkage = Linkage::kInternal;
  Constant initializer;
};

// The body a slot of a table holds: a function of this unit, or one another
// unit defines, reached by its symbol.
using TableBody = std::variant<ConstantFunction, ConstantExternalFunction>;

// One behavior an object of a class dispatches on (LRM 8.20) and the body
// answering it, none where the class leaves it abstract (LRM 8.21) and for a
// class another unit defines, whose table this unit lays out nothing of.
struct DispatchedBehavior {
  StatedDispatchRef behavior;
  std::optional<TableBody> answered_by;
};

// What an object of a class answers the methods one interface class declares
// itself with (LRM 8.26.5), in the order it declares them.
struct InterfaceTable {
  TypeId of;
  std::vector<std::optional<TableBody>> answered_by;
};

// One compiled class: its identity, the members it declares and what an object
// of it dispatches on -- what a target needs to hold a value of it, lay one out
// and lay out its tables. Which body each behavior runs is resolved above, and
// only where each lies in a table is the target's.
//
// A class states its own members and not its base's. What a value of it holds
// is read from the lineage, which is what keeps one declaration's meaning
// independent of what extends it. What its constructor links under and what its
// definition links under are two different symbols over the same parts, each
// composed by the one function that owns its category.
struct Class {
  // The unit that defines the class, where it is another unit's. Such a class
  // is declared here, as that unit published it: its members at the slots that
  // class gave them -- the ones another unit may name first, then its `local`
  // ones, which this unit never names but a class of it extending this one is
  // placed after. This unit emits nothing of it.
  std::optional<std::string> defining_unit;
  // The path another unit reaches this class by, or its place among the
  // classes the lowering built for its own use, which nothing outside this unit
  // names. Every symbol qualified by this class is composed from it, and the
  // two ranges never meet.
  support::ClassName identity;
  // How everything this class links under is linked: its definition, how its
  // objects are created and ended, and its bodies.
  Linkage linkage = Linkage::kExternal;
  // The type of what this class's members are placed after -- the class it
  // extends, of this unit or another, or a class of the runtime library -- and
  // absent for an interface class, which holds no storage and of which no value
  // is built.
  std::optional<TypeId> base;
  std::vector<Member> members;
  // Every behavior an object of the class dispatches on: those of the class it
  // extends first, in that class's order, then those it introduces, in the
  // order it declares them.
  std::vector<DispatchedBehavior> dispatch;
  // Each interface class an object of the class is also an object of, once
  // however many ways it is reached (LRM 8.26.6.3). Empty for a class another
  // unit defines.
  std::vector<InterfaceTable> interface_tables;
};

// One compiled closure: the captures it holds and the one body that reads them.
// Its captures are initialized where a value of it is built rather than by a
// body of its own, and nothing dispatches on it, so it shares the member
// vocabulary with a class and no part of its interface.
// It carries no name: the source declares no closure, so its position in its
// unit is the whole of its identity and what it links under is composed from
// that.
struct Closure {
  std::vector<Member> captures;
  FunctionId invoke{};
};

// The function of the unit a record answers one operation on its whole value
// with.
struct RecordMethod {
  support::ValueOperation answers;
  FunctionId function;
};

// Which aggregate a record declares: a structure (LRM 7.2), or a union (LRM
// 7.3), untagged or tagged.
enum class RecordKind : std::uint8_t {
  kStruct,
  kUnion,
  kTaggedUnion,
};

// A structure or a union a unit declares: which declaration of its unit it is,
// which is what another unit reaches it by, which aggregate it is, its
// members' types in position order, and the function it answers each
// operation on a whole value with.
//
// The runtime asks the same questions of values it holds, and it was compiled
// before the type existed, so a target hands it these methods along with the
// functions the unit states for each step of a value's life, and where the
// components lie, which the target lays out.
struct RecordDecl {
  // The unit that defines the record, where it is another unit's. Such a
  // record is declared here as this unit read it, and its methods are that
  // unit's, so it lists none and this unit emits nothing of it.
  std::optional<std::string> defining_unit;
  support::DefPath path;
  // How everything this record links under is linked: its type and its
  // methods.
  Linkage linkage = Linkage::kExternal;
  RecordKind kind;
  std::vector<TypeId> elements;
  std::vector<RecordMethod> methods;
};

// The function of the unit answering the operation `answers` at `type`: what a
// use in the program calls, and what code compiled before the type existed
// reaches through the type's table -- an array's operations on a whole value
// (LRM 7.4.2), an array's element default (LRM 7.4.5, 7.5.1).
struct TypeOperation {
  TypeId type;
  support::ValueOperation answers;
  FunctionId function;
};

// Storage this unit defines that no instance owns: one cell for the whole
// program, reached by its linkage symbol rather than through a receiver.
// `type` is the storage's own type, which is what tells whoever realizes it
// what to build; a reference to it is an operand typed as a pointer to that.
// The unit that declares the storage lists it, and only that unit does, so
// every referrer -- this one included -- names it and none defines it twice.
struct StaticStorage {
  std::string symbol;
  TypeId type;
};

// The LIR of one compilation unit: its own type graph, its classes and those of
// other units it compiled against, its closures, its records and those of
// other units it holds values of, the storage it shares program-wide, every
// function it compiles, and the class its object tree is rooted at, when it
// roots one -- a unit that declares only a namespace compiles functions and
// roots no objects. Self-contained -- it holds no reference to the MIR it was
// lowered from.
//
// Every body is a function here, whatever declared it, and its position is the
// identity a call names. A class reaches its own bodies the same way any other
// caller does.
struct CompilationUnit {
  // The unit's own name, carried from the program this one was lowered from.
  // Every identity below indexes a pool this unit owns and numbers from zero,
  // so a reader holding two units needs the pool named to know which one an id
  // belongs to.
  std::string name;
  // Which body of the unit this is, carried the same way: what only this body
  // holds is linked under it, and it is the artifact's name.
  std::string body_name;
  TypePool types;
  base::Registry<Class, ClassId> classes;
  base::Registry<Closure, ClosureId> closures;
  base::Registry<RecordDecl, RecordId> records;
  base::Registry<Function, FunctionId> functions;
  // The functions answering the operations the unit states per type, one per
  // type and operation.
  std::vector<TypeOperation> type_operations;
  // The function carrying out each step of a value's life the unit states for
  // a type, by the type and the step: what copying, moving, writing over and
  // ending a value of the type does, built from its parts. A step of a value
  // calls it, and code compiled before the type existed reaches it through
  // the type's table.
  std::map<std::pair<TypeId, ValueStep>, FunctionId> type_steps;
  std::vector<StaticStorage> static_storage;
  // The read-only data the unit defines. What the runtime library reads of each
  // class -- its record of the class, under the class's linkage and definition
  // symbol, and the constants that record points at, under internal linkage --
  // is here, except the tables of the bodies its objects dispatch through,
  // which the class states and a target lays out.
  std::vector<GlobalConstant> constants;
  // Each enumeration member table the unit holds, which an operand names.
  base::Translation<EnumTableId, EnumTableDecl> enum_tables;
  // Each constant integral value the unit holds: its type and its bits, which
  // is the whole of a value of an integral type, so a target states one as
  // data.
  base::Translation<IntegralConstantId, IntegralConstantDecl>
      integral_constants;
  std::optional<ClassId> root;
};

// The members' types of the record `record`, wherever it was declared, in
// declaration order.
[[nodiscard]] inline auto RecordElements(
    const CompilationUnit& unit, RecordId record) -> std::span<const TypeId> {
  return unit.records.Get(record).elements;
}

// Which aggregate the record `record` is, wherever it was declared.
[[nodiscard]] inline auto RecordKindOf(
    const CompilationUnit& unit, RecordId record) -> RecordKind {
  return unit.records.Get(record).kind;
}

// The record declaration `type` is, and nothing for any other type.
[[nodiscard]] inline auto RecordOf(const Type& type) -> const RecordId* {
  if (const auto* record = type.As<RecordType>()) {
    return &record->declaration;
  }
  return nullptr;
}

// Which aggregate `type` is, where it is a record, and nothing for any other
// type.
[[nodiscard]] inline auto RecordKindOf(const CompilationUnit& unit, TypeId type)
    -> std::optional<RecordKind> {
  const RecordId* record = RecordOf(unit.types.Get(type));
  if (record == nullptr) {
    return std::nullopt;
  }
  return RecordKindOf(unit, *record);
}

// The components of a product -- a tuple or a structure, which are reached the
// same way -- or nothing for a type that is no product.
[[nodiscard]] inline auto ProductElements(
    const CompilationUnit& unit, TypeId type)
    -> std::optional<std::span<const TypeId>> {
  const Type& t = unit.types.Get(type);
  if (const auto* tuple = t.As<TupleType>()) {
    return std::span<const TypeId>{tuple->elements};
  }
  const RecordId* record = RecordOf(t);
  if (record == nullptr) {
    return std::nullopt;
  }
  switch (RecordKindOf(unit, *record)) {
    case RecordKind::kStruct:
      return RecordElements(unit, *record);
    case RecordKind::kUnion:
    case RecordKind::kTaggedUnion:
      return std::nullopt;
  }
  throw InternalError("lir: unknown record kind");
}

// The members of the union or the tagged union `type` is, in declaration
// order, each lying at the start of the union's storage (LRM 7.3, 7.3.2), and
// nothing for any other type.
[[nodiscard]] inline auto OverlaidMembers(
    const CompilationUnit& unit, TypeId type)
    -> std::optional<std::span<const TypeId>> {
  const RecordId* record = RecordOf(unit.types.Get(type));
  if (record == nullptr) {
    return std::nullopt;
  }
  switch (RecordKindOf(unit, *record)) {
    case RecordKind::kStruct:
      return std::nullopt;
    case RecordKind::kUnion:
    case RecordKind::kTaggedUnion:
      return RecordElements(unit, *record);
  }
  throw InternalError("lir: unknown record kind");
}

// Whether `type` is a tagged union, whose member is chosen by its tag (LRM
// 7.3.2).
[[nodiscard]] inline auto IsTaggedUnion(
    const CompilationUnit& unit, TypeId type) -> bool {
  const std::optional<RecordKind> kind = RecordKindOf(unit, type);
  if (!kind.has_value()) {
    return false;
  }
  switch (*kind) {
    case RecordKind::kStruct:
    case RecordKind::kUnion:
      return false;
    case RecordKind::kTaggedUnion:
      return true;
  }
  throw InternalError("lir: unknown record kind");
}

// The parts a component step reaches in a value of `type`, by position: a
// structure's or a union's members, or a tuple's components; nothing for any
// other type.
[[nodiscard]] inline auto ComponentTypes(
    const CompilationUnit& unit, TypeId type)
    -> std::optional<std::span<const TypeId>> {
  const Type& t = unit.types.Get(type);
  if (const auto* tuple = t.As<TupleType>()) {
    return std::span<const TypeId>{tuple->elements};
  }
  if (const RecordId* record = RecordOf(t)) {
    return RecordElements(unit, *record);
  }
  return std::nullopt;
}

}  // namespace lyra::lir
