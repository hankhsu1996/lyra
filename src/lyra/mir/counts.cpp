#include "lyra/mir/counts.hpp"

#include <cstdint>
#include <vector>

#include "lyra/mir/callable.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/closure_decl.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/statistics.hpp"

namespace lyra::mir {

namespace {

// What the classes of a unit hold between them.
struct HeldByClasses {
  std::uint64_t fields = 0;
  std::uint64_t constructors = 0;
  std::uint64_t callables = 0;
  std::uint64_t coroutines = 0;
  std::uint64_t static_properties = 0;
  std::uint64_t constants = 0;
};

auto IsCoroutine(const CompilationUnit& unit, const CallableCode& code)
    -> bool {
  return unit.types.Get(code.result_type).Is<CoroutineType>();
}

// Every member of a class is taken apart by name, so a member added to one
// does not compile here until it is said what kind of thing it is, or that it
// is none.
void Add(const CompilationUnit& unit, const Class& cls, HeldByClasses& held) {
  const auto& [path, base, implements, conforming, is_final, is_interface_class, self_pointer_type, time_resolution, fields, named_fields, constructor, contained, callables, static_properties, named_static_properties, named_callables, constants, object_definition_initializer] =
      cls;
  held.fields += fields.size();
  held.constructors += constructor.has_value() ? 1 : 0;
  held.callables += callables.size();
  for (const CallableDecl& callable : callables) {
    held.coroutines += IsCoroutine(unit, callable.code) ? 1 : 0;
  }
  held.static_properties += static_properties.size();
  held.constants += constants.size();
}

}  // namespace

auto CountsOf(const CompilationUnit& unit) -> std::vector<support::Count> {
  // As for a class: every member of the unit is named here.
  const auto& [name, body_name, source_name, types, integral_constants, enum_tables, builtins, classes, content, external_classes, callables, named_callables, type_operations, static_variables, named_static_variables, foreign_scope_entries, records, record_methods, external_records, closures, consumed_signatures] =
      unit;

  HeldByClasses in_classes;
  for (const Class& cls : classes) {
    Add(unit, cls, in_classes);
  }

  std::uint64_t coroutines = in_classes.coroutines;
  for (const CallableDecl& callable : callables) {
    coroutines += IsCoroutine(unit, callable.code) ? 1 : 0;
  }
  std::uint64_t methods_of_records = 0;
  for (const RecordMethods& methods : record_methods) {
    methods_of_records += methods.size();
  }
  std::uint64_t captures = 0;
  for (const ClosureDecl& closure : closures) {
    captures += closure.fields.size();
    coroutines += IsCoroutine(unit, closure.invoke) ? 1 : 0;
  }

  return {
      {.kind = "classes", .held = classes.size()},
      {.kind = "fields", .held = in_classes.fields},
      {.kind = "constructors", .held = in_classes.constructors},
      {.kind = "callables", .held = callables.size() + in_classes.callables},
      {.kind = "coroutines", .held = coroutines},
      {.kind = "closures", .held = closures.size()},
      {.kind = "captures", .held = captures},
      {.kind = "records", .held = records.size()},
      {.kind = "record methods", .held = methods_of_records},
      {.kind = "type operations", .held = type_operations.size()},
      {.kind = "static variables", .held = static_variables.size()},
      {.kind = "static properties", .held = in_classes.static_properties},
      {.kind = "class constants", .held = in_classes.constants},
      {.kind = "integral constants", .held = integral_constants.size()},
      {.kind = "enum tables", .held = enum_tables.size()},
      {.kind = "types", .held = types.size()},
      {.kind = "foreign scope entries", .held = foreign_scope_entries.size()},
      {.kind = "external classes", .held = external_classes.size()},
      {.kind = "external records", .held = external_records.size()},
      {.kind = "consumed signatures", .held = consumed_signatures.size()},
  };
}

}  // namespace lyra::mir
