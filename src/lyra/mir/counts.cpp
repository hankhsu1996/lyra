#include "lyra/mir/counts.hpp"

#include <algorithm>
#include <cstdint>
#include <span>
#include <utility>
#include <vector>

#include "lyra/mir/callable.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/closure_decl.hpp"
#include "lyra/mir/code_source.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/operand_use.hpp"
#include "lyra/mir/stmt.hpp"
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
  const auto& [path, base, implements, conforming, is_final, is_interface_class, self_pointer_type, time_resolution, execution, dumped_as, fields, named_fields, constructor, contained, callables, static_properties, named_static_properties, named_callables, constants, object_definition_initializer] =
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

// How many levels the tallest expression of `block` stands on, an expression
// one level above the tallest of its operands. Walked without recursion, as the
// blocks are.
auto TallestExpression(const Block& block) -> std::uint64_t {
  std::vector<std::uint64_t> height(block.exprs.size(), 0);
  const auto height_of = [&](ExprId id) -> std::uint64_t& {
    return height.at(id.value);
  };
  std::uint64_t tallest = 0;
  for (const ExprId root : block.exprs.Ids()) {
    // An expression is visited twice: once to reach its operands, and once
    // they are measured, to measure it.
    std::vector<std::pair<ExprId, bool>> unvisited{{root, false}};
    while (!unvisited.empty()) {
      const auto [id, operands_measured] = unvisited.back();
      unvisited.pop_back();
      if (height_of(id) != 0) {
        continue;
      }
      const ExprData& data = block.exprs.Get(id).data;
      if (!operands_measured) {
        unvisited.emplace_back(id, true);
        ForEachOperand(data, [&](const ExprId& operand, OperandUse) {
          if (height_of(operand) == 0) {
            unvisited.emplace_back(operand, false);
          }
        });
        continue;
      }
      std::uint64_t measured = 1;
      ForEachOperand(data, [&](const ExprId& operand, OperandUse) {
        measured = std::max(measured, height_of(operand) + 1);
      });
      height_of(id) = measured;
      tallest = std::max(tallest, measured);
    }
  }
  return tallest;
}

// How deep `body` nests, the body itself one level: each block one level
// inside the block holding it, and an expression as many levels inside its
// block as it stands on. Walked without recursion, so a body nested deeper
// than a stack allows is still measured.
auto NestingDepth(const Block& body) -> std::uint64_t {
  std::uint64_t deepest = 0;
  std::vector<std::pair<const Block*, std::uint64_t>> unvisited{{&body, 1}};
  while (!unvisited.empty()) {
    const auto [block, depth] = unvisited.back();
    unvisited.pop_back();
    deepest = std::max(deepest, depth + TallestExpression(*block));
    for (const Block& child : block->child_scopes) {
      unvisited.emplace_back(&child, depth + 1);
    }
  }
  return deepest;
}

}  // namespace

auto CountsOf(const CompilationUnit& unit) -> std::vector<support::Count> {
  // As for a class: every member of the unit is named here. The layouts kept
  // on it are answers to questions asked of it, which no unit states, so they
  // are not counted.
  const auto& [name, body_name, source_name, types, integral_constants, enum_tables, builtins, classes, content, external_classes, callables, named_callables, type_operations, static_variables, named_static_variables, foreign_scope_entries, records, record_methods, external_records, closures, layouts, consumed_signatures] = unit;

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

auto NestingDepthOf(const CompilationUnit& unit) -> std::uint64_t {
  std::uint64_t deepest = 0;
  ForEachCode(
      unit, [&](const CodeSource&, const CallableCode& code,
                std::span<const ExprId>) {
        if (code.body.has_value()) {
          deepest = std::max(deepest, NestingDepth(*code.body));
        }
      });
  return deepest;
}

}  // namespace lyra::mir
