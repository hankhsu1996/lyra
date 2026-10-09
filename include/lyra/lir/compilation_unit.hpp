#pragma once

#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/base/registry.hpp"
#include "lyra/base/translation.hpp"
#include "lyra/lir/class_id.hpp"
#include "lyra/lir/closure_id.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/function_id.hpp"
#include "lyra/lir/integral_constant_id.hpp"
#include "lyra/lir/struct_id.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/type_id.hpp"
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

// One virtual method a class overrides (LRM 8.20): which virtual method it is,
// named by the class that first declared it and its position among that
// class's virtual methods, and the symbol of the function this class supplies
// for it.
struct Override {
  StatedDispatchRef behavior;
  std::string body;
};

// What one class contributes to its virtual table (LRM 8.20). `introduces` is
// the virtual methods this class is the first to declare, in declaration order,
// each with the symbol of its function -- or nothing for a pure virtual method
// (LRM 8.21), which has none. `overrides` is the virtual methods of a base
// class this one replaces.
//
// A class states only its own contribution. A target builds the class's whole
// table by taking the base class's table, putting each override in the slot it
// replaces, and appending what this class introduces.
struct ClassDispatch {
  std::vector<std::optional<std::string>> introduces;
  std::vector<Override> overrides;
};

// How a class satisfies one method of an interface class it implements (LRM
// 8.26.2). `interface_behavior` is the interface class's method; `answered_by`
// is the virtual method in the class's own virtual table that implements it.
// A call through an interface class handle runs whatever function the object's
// class has in that slot, so a class overriding the method further down
// answers the interface with its own function too. Absent where an abstract
// class leaves the method for a class extending it to implement.
struct ConformingBehavior {
  StatedDispatchRef interface_behavior;
  std::optional<StatedDispatchRef> answered_by;
};

struct Constant;

// A machine integer, at the width of whatever it is placed in.
struct ConstantInt {
  std::int64_t value = 0;
};

// An address naming nothing.
struct ConstantNull {};

// The address of a NUL-terminated string.
struct ConstantString {
  std::string text;
};

// The address of a function of this unit.
struct ConstantFunction {
  FunctionId function;
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
      ConstantInt, ConstantNull, ConstantString, ConstantFunction,
      ConstantAddress, ConstantRecord, ConstantArray>
      value;
};

// Who may name a symbol of data: only the unit defining it, or any unit of the
// program.
enum class Linkage : std::uint8_t { kInternal, kExternal };

// Read-only data this unit defines, linked under `symbol` and holding
// `initializer` from before the program starts. Everything naming it -- another
// constant, or under external linkage another unit -- does so by that symbol,
// which is stated here and composed nowhere below.
struct GlobalConstant {
  std::string symbol;
  Linkage linkage = Linkage::kInternal;
  Constant initializer;
};

// One compiled class: its path, the members it declares, what it adds to
// dispatch, and the interface classes it names -- what a target needs to hold a
// value of it and lay one out.
//
// A class states its own members and not its base's. What a value of it holds
// is read from the lineage, which is what keeps one declaration's meaning
// independent of what extends it. What its constructor links under and what its
// definition links under are two different symbols over the same parts, each
// composed by the one function that owns its category.
struct Class {
  // The path another unit reaches this class by, absent where nothing outside
  // this unit names it -- a class the lowering built for its own use. Every
  // symbol qualified by this class is composed from it where it is present and
  // from the class's own position where it is not, so the two ranges never
  // meet.
  std::optional<support::DefPath> path;
  // The type of what this class's members are placed after -- the class it
  // extends, of this unit or another, or a class of the runtime library -- and
  // absent for an interface class, which holds no storage and of which no value
  // is built.
  std::optional<TypeId> base;
  std::vector<Member> members;
  ClassDispatch dispatch;
  // The interface classes this one names: what it implements, or for an
  // interface class what it extends (LRM 8.26.2) -- the bases its description
  // lists beside the class it extends. A value of the class is also a value of
  // what those extend and of what its base is, and holds a part for each; which
  // parts and in what order is the target's to place.
  std::vector<TypeId> implements;
  // What answers each behavior of every interface class a value of this class
  // is also a value of, for a class that is not an interface class.
  std::vector<ConformingBehavior> conforming;
};

// A class of another unit this one reaches into, as that unit published it:
// which unit declares it and its path there, both resolved at link time,
// what it extends, and its members at the slots that class gave them -- the
// ones another unit may name first, then its `local` ones, which this unit
// never names but a class of it extending this one is placed after -- and what
// it adds to dispatch, which a class of this unit extending it builds its table
// from. What it inherited is not among them: a member of an ancestor is reached
// through the class that declares it.
//
// What a unit published of the object one of its scopes is -- an instance, or
// a generate block inside one -- is such a class too. Its members are what the
// scope published, in the order it published them, and it adds nothing to
// dispatch, because none of its methods dispatches.
//
// This unit compiles none of it, which is why it sits apart from the classes
// above.
struct ExternalClass {
  std::string unit_name;
  support::DefPath class_path;
  // What its members are placed after, as for a class of this unit.
  std::optional<TypeId> base;
  std::vector<Member> members;
  ClassDispatch dispatch;
  // The interface classes it names, as for a class of this unit.
  std::vector<TypeId> implements;
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

// The function of the unit a struct answers one operation on its whole value
// with.
struct StructMethod {
  support::ValueOperation answers;
  FunctionId function;
};

// A struct this unit declares (LRM 7.2): which declaration of the unit it is,
// which is what another unit reaches it by, its components' types in position
// order, and the function it answers each operation on a whole value with.
//
// The runtime asks the same questions of values it holds, and it was compiled
// before the type existed, so a target hands it these methods along with what
// it derives of the type itself: where the components lie, and how a value is
// copied, moved and ended.
struct Struct {
  support::DefPath path;
  std::vector<TypeId> elements;
  std::vector<StructMethod> methods;
};

// A struct another unit declares, as this unit reads it: the declaration it is
// and its components' types, which is what a value of it needs here.
struct ExternalStruct {
  TypeDeclarationRef declaration;
  std::vector<TypeId> elements;
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

// The LIR of one compilation unit: its own type graph, its classes, its
// closures, its structs and those of other units it holds values of, the
// classes of other units it compiled against, the storage it shares
// program-wide, every function it compiles, and the class its object tree is
// rooted at, when it roots one -- a unit that declares only a namespace
// compiles functions and roots no objects. Self-contained -- it holds no
// reference to the MIR it was lowered from.
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
  TypePool types;
  base::Registry<Class, ClassId> classes;
  base::Registry<Closure, ClosureId> closures;
  base::Registry<Struct, StructId> structs;
  // Every struct of another unit this one holds a value of, found by the
  // declaration that names it.
  std::vector<ExternalStruct> external_structs;
  // One entry per class of another unit this one names, found by the pair that
  // names the class -- the pair every reference to one carries, so a reference
  // and the record its slot is counted out of cannot come apart.
  std::vector<ExternalClass> external_classes;
  base::Registry<Function, FunctionId> functions;
  std::vector<StaticStorage> static_storage;
  // The read-only data the unit defines. What the runtime library reads of each
  // class -- its record of the class, under external linkage and the class's
  // definition symbol, and the tables that record points at, under internal
  // linkage -- is here, a class holding none of it.
  std::vector<GlobalConstant> constants;
  // The nullary function building each value the unit holds, one answer per
  // entry of the pool that holds it. Building a value is an instruction
  // sequence like any other, so a description and a constant are both code
  // here, and the entry naming one is what reaches its function.
  base::Translation<TypeDescriptorId, FunctionId> type_descriptor_initializers;
  base::Translation<IntegralConstantId, FunctionId>
      integral_constant_initializers;
  std::optional<ClassId> root;
};

// The record kept of the class `class_path` of unit `unit_name`, or nothing
// where this unit holds no published record of it -- a class no signature the
// design compiles carries.
[[nodiscard]] inline auto FindExternalClass(
    const CompilationUnit& unit, std::string_view unit_name,
    const support::DefPath& class_path) -> const ExternalClass* {
  for (const ExternalClass& record : unit.external_classes) {
    if (record.unit_name == unit_name && record.class_path == class_path) {
      return &record;
    }
  }
  return nullptr;
}

// What this unit read of the struct another unit declares as `declaration`.
[[nodiscard]] inline auto ExternalStructOf(
    const CompilationUnit& unit, const TypeDeclarationRef& declaration)
    -> const ExternalStruct& {
  for (const ExternalStruct& external : unit.external_structs) {
    if (external.declaration == declaration) {
      return external;
    }
  }
  throw InternalError(
      "lir: a struct of another unit is named that this unit never read");
}

// The components' types of the struct `type` is, wherever it was declared.
[[nodiscard]] inline auto StructElements(
    const CompilationUnit& unit, const StructType& type)
    -> std::span<const TypeId> {
  return std::visit(
      Overloaded{
          [&](StructId id) -> std::span<const TypeId> {
            return unit.structs.Get(id).elements;
          },
          [&](const TypeDeclarationRef& ref) -> std::span<const TypeId> {
            return ExternalStructOf(unit, ref).elements;
          }},
      type.declaration);
}

// The declaration any unit names the struct `type` is by.
[[nodiscard]] inline auto StructDeclarationOf(
    const CompilationUnit& unit, const StructType& type) -> TypeDeclarationRef {
  return std::visit(
      Overloaded{
          [&](StructId id) -> TypeDeclarationRef {
            return TypeDeclarationRef{
                .unit_name = unit.name, .path = unit.structs.Get(id).path};
          },
          [](const TypeDeclarationRef& ref) -> TypeDeclarationRef {
            return ref;
          }},
      type.declaration);
}

// The components of a product -- a tuple or a struct, which are reached the
// same way -- or nothing for a type that is no product.
[[nodiscard]] inline auto ProductElements(
    const CompilationUnit& unit, TypeId type)
    -> std::optional<std::span<const TypeId>> {
  const Type& t = unit.types.Get(type);
  if (const auto* tuple = t.As<TupleType>()) {
    return std::span<const TypeId>{tuple->elements};
  }
  if (const auto* structure = t.As<StructType>()) {
    return StructElements(unit, *structure);
  }
  return std::nullopt;
}

}  // namespace lyra::lir
