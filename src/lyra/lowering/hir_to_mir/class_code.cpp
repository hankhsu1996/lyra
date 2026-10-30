#include "lyra/lowering/hir_to_mir/class_code.hpp"

#include <optional>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/overloaded.hpp"
#include "lyra/lowering/hir_to_mir/class_shape.hpp"
#include "lyra/lowering/hir_to_mir/library_entry_body.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/local.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// What `code` takes and answers, declared and not defined, entered on an
// object reached as `self`: what a class introducing it as a virtual method
// states, and what each override of it takes.
auto SlotPrototype(const mir::CallableCode& code, mir::TypeId self)
    -> mir::CallableCode {
  mir::CallableCode prototype;
  const mir::LocalId receiver = prototype.AddLocal(self);
  prototype.params.push_back(receiver);
  prototype.receiver = receiver;
  for (const mir::LocalId param : code.ParamsAfterReceiver()) {
    prototype.params.push_back(prototype.AddLocal(code.locals.Get(param).type));
  }
  prototype.result_type = code.result_type;
  return prototype;
}

}  // namespace

void MoveCodeIntoBody(
    UnitLowerer& unit_lowerer, mir::ClassId id, mir::Class& cls,
    const WalkFrame& declaring_frame, mir::ClassId published) {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Class& body = *declaring_frame.current_class.cls;
  const mir::TypeId published_pointer =
      unit_lowerer.GetClassShape(published).self_pointer_type;
  const DeclaringInstance& instance =
      *unit_lowerer.GetClassShape(id).declaring_instance;

  // The entry keeps the operation's signature and enters the code the
  // instance's body compiled: on the instance a construction or a static method
  // is handed (LRM 8.7, 8.10), and for any other on the one its object
  // recorded, which the construction has written by then.
  const auto entry_of = [&](const mir::CallableCode& code,
                            const ClassCodePoint& point, mir::CallableId slot) {
    mir::CallableCode entry = mir::CallableCode::Defined();
    for (const mir::LocalId param : code.params) {
      entry.params.push_back(entry.AddLocal(code.locals.Get(param).type));
    }
    if (code.TakesReceiver()) entry.receiver = entry.params.front();
    entry.result_type = code.result_type;
    mir::Block& block = entry.Body();
    const auto read = [&](mir::LocalId param) {
      return block.ReadLocal(param, entry.locals.Get(param).type);
    };
    const mir::ExprId handed = std::visit(
        Overloaded{
            [&](const ClassConstructorCode&) { return read(entry.params[1]); },
            [&](const ClassCallableCode&) {
              if (!entry.receiver.has_value()) {
                return read(entry.params.front());
              }
              return block.Read(block.exprs.Add(
                  mir::MakeFieldAccessExpr(
                      BuildObjectDeref(unit, block, read(*entry.receiver)),
                      mir::ClassFieldTarget{
                          .owner = id, .slot = instance.member},
                      instance.type)));
            }},
        point);
    std::vector<mir::ExprId> arguments;
    arguments.reserve(entry.params.size());
    for (const mir::LocalId param : entry.params) {
      arguments.push_back(read(param));
    }
    const mir::ExprId call = block.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Virtual{
                            .receiver = BuildObjectDeref(
                                unit, block,
                                ObjectAs(block, handed, published_pointer)),
                            .slot =
                                mir::VirtualSlot{
                                    .owner_class = published, .slot = slot}},
                    .arguments = std::move(arguments)},
            .type = entry.result_type});
    CompleteWith(unit, block, call, entry.result_type);
    return entry;
  };

  const auto move_into_body = [&](const ClassCodePoint& point,
                                  mir::CallableCode& code) {
    const mir::CallableId slot =
        unit_lowerer.ClassCodeSlotOf(published, id, point);
    mir::CallableCode entry = entry_of(code, point, slot);
    mir::CallableCode implementation = std::move(code);
    const mir::LocalId self = implementation.AddLocal(body.self_pointer_type);
    implementation.params.insert(implementation.params.begin(), self);
    implementation.receiver = self;
    unit_lowerer.StateClassCodePrototype(
        published, id, point, SlotPrototype(implementation, published_pointer));
    body.callables.Add(
        mir::CallableDecl{
            .code = std::move(implementation),
            .foreign = std::nullopt,
            .virtual_dispatch =
                mir::OverridesVirtualSlot{
                    .overridden =
                        mir::VirtualSlot{
                            .owner_class = published, .slot = slot}},
            .defining_unit = std::nullopt});
    code = std::move(entry);
  };

  move_into_body(ClassConstructorCode{}, cls.constructor->code);
  cls.callables =
      std::move(cls.callables)
          .TransformValues(
              [&](mir::CallableId callable, mir::CallableDecl decl) {
                if (decl.code.body.has_value()) {
                  move_into_body(
                      ClassCallableCode{.callable = callable}, decl.code);
                }
                return decl;
              });
}

}  // namespace lyra::lowering::hir_to_mir
