#include "lyra/mir/type_substitution.hpp"

#include <optional>
#include <span>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/keeper.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

namespace {

// `type` rebuilt with each type it is made of replaced by what `component`
// answers for it. A type made of no other type is itself.
template <typename Component>
auto WithComponents(
    const TypePool& types, TypeId type, const Component& component) -> TypeId {
  const auto rebuilt = [&](auto replaced) {
    return types.Intern(Type{std::move(replaced)});
  };
  const auto all = [&](std::vector<TypeId> of) {
    for (TypeId& each : of) {
      each = component(each);
    }
    return of;
  };
  return types.Get(type).Visit(
      Overloaded{
          [&](const IntegralType&) { return type; },
          [&](const EnumType&) { return type; },
          [&](UnpackedArrayType t) {
            t.element_type = component(t.element_type);
            return rebuilt(t);
          },
          [&](DynamicArrayType t) {
            t.element_type = component(t.element_type);
            return rebuilt(t);
          },
          [&](QueueType t) {
            t.element_type = component(t.element_type);
            return rebuilt(t);
          },
          [&](AssociativeArrayType t) {
            t.element_type = component(t.element_type);
            t.key_type = component(t.key_type);
            return rebuilt(t);
          },
          [&](const WildcardIndexType&) { return type; },
          [&](const StringType&) { return type; },
          [&](const AnyType&) { return type; },
          [&](const ErasedValueType&) { return type; },
          [&](const MachineCStringType&) { return type; },
          [&](const MachineBoolType&) { return type; },
          [&](const MachineIntType&) { return type; },
          [&](const MachineFloatType&) { return type; },
          [&](MachineArrayType t) {
            t.element = component(t.element);
            return rebuilt(t);
          },
          [&](MachineFunctionType t) {
            t.params = all(std::move(t.params));
            t.result = component(t.result);
            return rebuilt(std::move(t));
          },
          [&](const RealType&) { return type; },
          [&](const ShortRealType&) { return type; },
          [&](const ChandleType&) { return type; },
          [&](const VoidType&) { return type; },
          [&](const ObjectType&) { return type; },
          [&](const RuntimeClassType&) { return type; },
          [&](const RuntimeEffectsType&) { return type; },
          [&](const FilesType&) { return type; },
          [&](const DiagnosticType&) { return type; },
          [&](const RuntimeLibraryType&) { return type; },
          [&](CoroutineType t) {
            t.payload = component(t.payload);
            return rebuilt(t);
          },
          [&](VariableRefType t) {
            t.pointee = component(t.pointee);
            return rebuilt(t);
          },
          [&](ReferenceWrapperType t) {
            t.pointee = component(t.pointee);
            return rebuilt(t);
          },
          [&](PointerType t) {
            t.pointee = component(t.pointee);
            return rebuilt(t);
          },
          [&](ReferenceType t) {
            t.referent = component(t.referent);
            return rebuilt(t);
          },
          [&](ManagedRefType t) {
            t.pointee = component(t.pointee);
            return rebuilt(t);
          },
          [&](WeakManagedRefType t) {
            t.pointee = component(t.pointee);
            return rebuilt(t);
          },
          [&](VectorType t) {
            t.element = component(t.element);
            return rebuilt(t);
          },
          [&](TupleType t) {
            t.elements = all(std::move(t.elements));
            return rebuilt(std::move(t));
          },
          [&](const EmptyType&) { return type; },
          [&](ObservableType t) {
            t.value = component(t.value);
            return rebuilt(t);
          },
          [&](ResolvedType t) {
            t.value = component(t.value);
            return rebuilt(t);
          },
          [&](DriverType t) {
            t.value = component(t.value);
            return rebuilt(t);
          },
          [&](OpenWriteType t) {
            t.value = component(t.value);
            return rebuilt(t);
          },
          [&](DesignationType t) {
            t.value = component(t.value);
            return rebuilt(t);
          },
          [&](BitsReferenceType t) {
            t.bits = component(t.bits);
            return rebuilt(t);
          },
          [&](ObjectWriteType t) {
            t.object = component(t.object);
            return rebuilt(t);
          },
          [&](SampledHistoryType t) {
            t.value = component(t.value);
            return rebuilt(t);
          },
          [&](const EvaluationAttemptsType&) { return type; },
          [&](const LoopBlocksType&) { return type; },
          [&](const RecordType&) { return type; },
          [&](const ClosureType&) { return type; },
          [&](ErasedCallableType t) {
            t.params = all(std::move(t.params));
            t.result = component(t.result);
            return rebuilt(std::move(t));
          },
          [&](const TypeParameterType&) { return type; }});
}

}  // namespace

auto SubstituteTypeParameters(
    const TypePool& types, TypeId type, std::span<const TypeId> arguments)
    -> TypeId {
  if (const auto* parameter = types.Get(type).As<TypeParameterType>()) {
    if (parameter->index.value >= arguments.size()) {
      throw InternalError(
          "mir::SubstituteTypeParameters: a type parameter beyond the "
          "arguments the call names -- please report this as a bug");
    }
    return arguments[parameter->index.value];
  }
  return WithComponents(types, type, [&](TypeId component) {
    return SubstituteTypeParameters(types, component, arguments);
  });
}

namespace {

// How `keeper`, a type a bound said keeps deferred effects and a substitution
// made concrete, keeps them.
auto KeeperOf(const TypePool& types, TypeId keeper) -> KeeperEntries {
  const std::optional<KeeperEntries> entries = KeeperEntriesOf(types, keeper);
  if (!entries.has_value()) {
    throw InternalError(
        "mir::InstantiateDefinition: a keeper's bound was met by a type "
        "that keeps no deferred effects -- please report this as a bug");
  }
  return *entries;
}

// `block` at `arguments`, its expressions, statements and nested blocks added
// in the order they were, so every id names what it did.
auto InstantiatedBlock(
    const TypePool& types, const Block& block,
    std::span<const TypeId> arguments) -> Block {
  Block made;
  for (const Expr& expr : block.exprs) {
    Expr copy = expr;
    copy.type = SubstituteTypeParameters(types, expr.type, arguments);
    if (auto* call = std::get_if<CallExpr>(&copy.data)) {
      if (auto* direct = std::get_if<Direct>(&call->callee)) {
        for (TypeId& type : direct->type_arguments) {
          type = SubstituteTypeParameters(types, type, arguments);
        }
        if (auto* operation =
                std::get_if<TypeOperationTarget>(&direct->target)) {
          operation->type =
              SubstituteTypeParameters(types, operation->type, arguments);
        }
        if (std::holds_alternative<BoundCallTarget>(direct->target)) {
          if (!direct->receiver.has_value()) {
            throw InternalError(
                "mir::InstantiateDefinition: a call through a bound names "
                "no receiver -- please report this as a bug");
          }
          const TypeId receiver = made.exprs.Get(*direct->receiver).type;
          direct->target = ClosureTarget{
              .closure = types.Get(receiver).Get<ClosureType>().closure_id};
        }
        if (std::holds_alternative<BoundCommitTarget>(direct->target)) {
          if (!direct->receiver.has_value()) {
            throw InternalError(
                "mir::InstantiateDefinition: a commit through a bound names "
                "no keeper -- please report this as a bug");
          }
          direct->target =
              KeeperOf(types, made.exprs.Get(*direct->receiver).type).commit;
        }
      }
    }
    if (auto* allocation = std::get_if<NewExpr>(&copy.data);
        allocation != nullptr && allocation->placement.has_value() &&
        std::holds_alternative<BoundAllocation>(
            allocation->placement->allocate)) {
      const TypeId keeper =
          made.exprs.Get(allocation->placement->arguments.front()).type;
      allocation->placement->allocate =
          KeeperOf(types, types.Get(keeper).Get<PointerType>().pointee)
              .allocate;
    }
    made.exprs.Add(std::move(copy));
  }
  for (const Stmt& stmt : block.stmts) {
    made.stmts.Add(stmt);
  }
  for (const Block& child : block.child_scopes) {
    made.child_scopes.Add(InstantiatedBlock(types, child, arguments));
  }
  made.root_stmts = block.root_stmts;
  return made;
}

}  // namespace

auto InstantiateDefinition(
    const TypePool& types, const CallableCode& code,
    std::span<const TypeId> arguments) -> CallableCode {
  CallableCode made;
  made.params = code.params;
  made.receiver = code.receiver;
  made.result_type =
      SubstituteTypeParameters(types, code.result_type, arguments);
  for (const LocalDecl& local : code.locals) {
    LocalDecl copy = local;
    copy.type = SubstituteTypeParameters(types, local.type, arguments);
    made.locals.Add(std::move(copy));
  }
  made.named_locals = code.named_locals;
  if (code.body.has_value()) {
    made.body = InstantiatedBlock(types, *code.body, arguments);
  }
  return made;
}

}  // namespace lyra::mir
