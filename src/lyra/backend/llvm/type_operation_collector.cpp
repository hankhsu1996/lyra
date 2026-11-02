#include "lyra/backend/llvm/type_operation_collector.hpp"

#include <cstddef>
#include <iterator>
#include <memory>
#include <optional>
#include <span>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/backend/llvm/fn_abi.hpp"
#include "lyra/backend/llvm/target_lowering.hpp"
#include "lyra/backend/llvm/witness_tables.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/place_query.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/lir/value_step.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/value_operation.hpp"

namespace lyra::backend::llvm_backend {

// Everything another unit hands the library of a record is stated by the unit
// declaring it, whether or not it builds a value of it, and storage the whole
// program shares is built here.
TypeOperationCollector::TypeOperationCollector(const lir::CompilationUnit& unit)
    : unit_(&unit) {
  for (const lir::RecordId id : unit_->records.Ids()) {
    if (!unit_->records.Get(id).defining_unit.has_value()) {
      VisitValueTypeUse(
          unit_->types.Intern(lir::Type{lir::RecordType{.declaration = id}}));
    }
  }
  for (const lir::StaticStorage& storage : unit_->static_storage) {
    VisitAbi(ArrangeVariableBuild(*unit_, storage.type));
  }
}

auto TypeOperationCollector::CollectUsed() -> std::vector<lir::TypeOperation> {
  // A function a call named may still wait for its body, and is read once it
  // has one.
  for (const lir::FunctionId id : unit_->functions.Ids()) {
    if (id.value >= functions_seen_) {
      unread_.push_back(id);
    }
  }
  functions_seen_ = unit_->functions.size();
  std::vector<lir::FunctionId> waiting;
  for (const lir::FunctionId id : unread_) {
    if (unit_->functions.IsDefined(id)) {
      VisitFunction(unit_->functions.Get(id));
    } else {
      waiting.push_back(id);
    }
  }
  unread_ = std::move(waiting);
  return std::exchange(used_, {});
}

auto TypeOperationCollectionOf(const lir::CompilationUnit& unit)
    -> lir::UsedTypeOperations {
  return [collector = std::make_shared<TypeOperationCollector>(unit)] {
    return collector->CollectUsed();
  };
}

void TypeOperationCollector::Use(
    lir::TypeId type, support::ValueOperation answers) {
  used_.push_back(lir::TypeOperation{.type = type, .answers = answers});
}

void TypeOperationCollector::UseFilling(
    lir::TypeId type, const WitnessFilling& filling) {
  std::visit(
      Overloaded{
          [](lir::ValueStep) {},
          [&](const OwnOperationWitness& own) { Use(type, own.answers); },
          [&](const EitherStreamForm& either) {
            Use(type, either.fixed.answers);
            Use(type, either.run.answers);
          }},
      filling);
}

void TypeOperationCollector::UseTable(
    lir::TypeId type, std::span<const Witness> table) {
  for (const Witness& witness : table) {
    UseFilling(type, witness.filled_by);
  }
}

// A type's constant travels with its values, so what it names a function of is
// what every holder may ask: whether a write changed a value (LRM 4.3). A
// record is its declaring unit's whole.
void TypeOperationCollector::VisitValueTypeUse(lir::TypeId type) {
  const lir::Type& stated = unit_->types.Get(type);
  if (!lir::IsLaidOutByItsType(stated) ||
      !value_types_seen_.insert(type).second) {
    return;
  }
  if (const lir::RecordId* record = lir::RecordOf(stated)) {
    if (unit_->records.Get(*record).defining_unit.has_value()) {
      return;
    }
  } else {
    UseTable(type, ValueWitnesses());
  }
  const TypeConstantReferences references = ReferencesOf(*unit_, type);
  if (references.element_type.has_value()) {
    VisitElementTypeUse(*references.element_type);
  }
  for (const lir::TypeId component : references.components) {
    VisitValueTypeUse(component);
  }
}

// A record's default is its declaration's, so the unit declaring it states this
// constant of it.
void TypeOperationCollector::VisitElementTypeUse(lir::TypeId type) {
  if (lir::RecordOf(unit_->types.Get(type)) != nullptr ||
      !element_types_seen_.insert(type).second) {
    return;
  }
  Use(type, support::BuiltinFn::kDeclaredDefault);
  VisitValueTypeUse(type);
}

// No holder keeps these: a call comparing or streaming a value hands them, for
// the value and, in a container, for each element.
void TypeOperationCollector::VisitOperationsUse(lir::TypeId type) {
  if (!operation_types_seen_.insert(type).second) {
    return;
  }
  const lir::Type& stated = unit_->types.Get(type);
  if (lir::RecordOf(stated) != nullptr) {
    return;
  }
  if (lir::IsLaidOutByItsType(stated)) {
    VisitValueTypeUse(type);
    UseTable(type, OperationWitnesses());
  }
  if (const std::optional<lir::TypeId> element =
          stated.ContainerElementType()) {
    VisitOperationsUse(*element);
  }
}

void TypeOperationCollector::VisitNetFoldsUse(lir::TypeId type) {
  if (lir::RecordOf(unit_->types.Get(type)) == nullptr) {
    UseTable(type, NetFoldWitnesses());
  }
}

void TypeOperationCollector::VisitTakePositionsUse(lir::TypeId type) {
  if (lir::RecordOf(unit_->types.Get(type)) == nullptr) {
    Use(type, support::BuiltinFn::kTakePositions);
  }
}

void TypeOperationCollector::VisitAbi(const diag::Result<FnAbi>& abi) {
  if (abi) {
    VisitAbi(*abi);
  }
}

void TypeOperationCollector::VisitAbi(const std::optional<FnAbi>& abi) {
  if (abi.has_value()) {
    VisitAbi(*abi);
  }
}

void TypeOperationCollector::VisitMode(const PassMode& mode) {
  if (const auto* with = std::get_if<PassWithType>(&mode)) {
    VisitValueTypeUse(with->type);
  }
}

void TypeOperationCollector::VisitAbi(const FnAbi& abi) {
  std::visit(
      Overloaded{
          [](const NoImplicitArg&) {}, [](const WidthMetadataArg&) {},
          [&](const TypeMetadataArg& held) { VisitElementTypeUse(held.of); },
          [&](const TypeAndNetFoldsArg& held) {
            VisitElementTypeUse(held.of);
            VisitNetFoldsUse(held.of);
          }},
      abi.implicit_arg);
  for (const ArgAbi& arg : abi.args) {
    VisitMode(arg.mode);
  }
  for (const TypeArg& told : abi.type_args) {
    std::visit(
        Overloaded{
            [&](const TypeConstantArg& of) { VisitValueTypeUse(of.type); },
            [](const TypeExtentArg&) {},
            [&](const ElementTypeArg& of) { VisitElementTypeUse(of.type); },
            [&](const QueueTypeArg& of) {
              VisitElementTypeUse(
                  *unit_->types.Get(of.type).ContainerElementType());
            },
            [&](const TakePositionsArg& of) { VisitTakePositionsUse(of.type); },
            [](const IndexOrderArg&) {},
            [&](const OperationsArg& of) { VisitOperationsUse(of.type); }},
        told);
  }
}

void TypeOperationCollector::VisitPlace(
    const TargetLowering& lowering, const lir::Function& fn,
    const lir::Place& place) {
  for (std::size_t at = 0; at < place.chain.size(); ++at) {
    const auto* step = std::get_if<lir::ElementProjection>(&place.chain[at]);
    if (step == nullptr) {
      continue;
    }
    const lir::TypeId container = lir::PlaceTypeBefore(
        *unit_, fn, place, static_cast<std::ptrdiff_t>(at));
    for (const PlaceAccess access : {PlaceAccess::kRead, PlaceAccess::kWrite}) {
      auto taken = lowering.ElementStepOf(container, step->coordinates, access);
      if (!taken) {
        continue;
      }
      std::visit(
          Overloaded{
              [&](const StepWithinArray& within) {
                VisitAbi(within.no_element.abi);
              },
              [&](const StepByEntry& entry) {
                for (const PassMode& mode : entry.coordinates) {
                  VisitMode(mode);
                }
              }},
          *taken);
    }
  }
}

void TypeOperationCollector::VisitCall(
    const TargetLowering& lowering, const lir::CallInstr& call,
    lir::TypeId result_type) {
  auto form = lowering.CallFormOf(call, result_type);
  if (!form) {
    return;
  }
  std::visit(
      Overloaded{
          [&](const OrdinaryCall&) {
            VisitAbi(lowering.FnAbiOf(call, result_type));
          },
          [](const ForwardedOperand&) {}, [](const CellLoad&) {},
          [](const CellStore&) {}},
      *form);
}

void TypeOperationCollector::VisitRvalue(
    const TargetLowering& lowering, const lir::Function& fn,
    const lir::Rvalue& value, lir::TypeId result_type) {
  std::visit(
      Overloaded{
          [&](const lir::CallInstr& call) {
            VisitCall(lowering, call, result_type);
          },
          [&](const lir::LoadInstr& load) {
            VisitPlace(lowering, fn, load.place);
          },
          [&](const lir::OverlaidLoadInstr& load) {
            VisitPlace(lowering, fn, load.place);
          },
          [&](const lir::DiscriminantInstr& read) {
            VisitPlace(lowering, fn, read.place);
          },
          [&](const lir::AddrOfInstr& addr) {
            VisitPlace(lowering, fn, addr.place);
          },
          [&](const lir::BinaryInstr& binary) {
            auto form = lowering.OperatorFormOf(binary);
            if (!form) {
              return;
            }
            std::visit(
                Overloaded{
                    [&](const EntryCall& entry) { VisitAbi(entry.abi); },
                    [](const MachineOperator&) {},
                    [](const IdentityOperator&) {}, [](const RealOperator&) {},
                    [](const IntegralOperator&) {}},
                *form);
          },
          [&](const lir::BeginVariableInstr& begun) {
            VisitAbi(ArrangeVariableBuild(
                *unit_, fn.variables.at(begun.position.value)));
          },
          [](const lir::ProductInstr&) {}, [](const lir::UnionInstr&) {},
          [](const lir::ClosureInstr&) {}, [](const lir::ArrayInstr&) {},
          [](const lir::VectorGetInstr&) {}, [](const lir::UnaryInstr&) {},
          [](const lir::CastInstr&) {}, [](const lir::LandingInstr&) {},
          [](const lir::VariableStorageInstr&) {}, [](const lir::UseInstr&) {}},
      value);
}

void TypeOperationCollector::VisitStore(
    const TargetLowering& lowering, const lir::Function& fn,
    const lir::StoreInstr& store) {
  VisitPlace(lowering, fn, store.place);
  auto form = lowering.StoreFormOf(store);
  if (!form) {
    return;
  }
  std::visit(
      Overloaded{
          [&](const WrapperStore& wrapper) { VisitAbi(wrapper.entry.abi); },
          [&](const CellEntryStore& cell) { VisitAbi(cell.entry.abi); },
          [](const CellStore&) {}, [](const AddressStore&) {}},
      *form);
}

void TypeOperationCollector::VisitBitsStore(
    const TargetLowering& lowering, const lir::Function& fn,
    const lir::BitsStoreInstr& store) {
  VisitPlace(lowering, fn, store.place);
  VisitAbi(lowering
               .BitsStoreCallOf(
                   store,
                   lir::PlaceTypeBefore(
                       *unit_, fn, store.place, std::ssize(store.place.chain)))
               .abi);
}

void TypeOperationCollector::VisitBeginStorage(
    const TargetLowering& lowering, const lir::Function& fn,
    const lir::Place& place) {
  VisitPlace(lowering, fn, place);
  auto storage = lowering.StorageAt(place);
  if (storage && storage->has_value()) {
    VisitAbi(ArrangeMemberBuild(
        *unit_, lir::PlaceType(*unit_, fn, place), **storage));
  }
}

void TypeOperationCollector::VisitFunction(const lir::Function& fn) {
  const TargetLowering lowering(*unit_, fn);
  const auto visit_destination = [&](const lir::Destination& destination) {
    if (const auto* place = std::get_if<lir::Place>(&destination)) {
      VisitPlace(lowering, fn, *place);
    }
  };
  for (const lir::BasicBlock& block : fn.blocks) {
    for (const lir::Instr& instr : block.instrs) {
      std::visit(
          Overloaded{
              [&](const lir::AssignInstr& assign) {
                visit_destination(assign.destination);
                VisitRvalue(
                    lowering, fn, assign.value,
                    lir::DestinationType(*unit_, fn, assign.destination));
              },
              [&](const lir::StoreInstr& store) {
                VisitStore(lowering, fn, store);
              },
              [&](const lir::BitsStoreInstr& store) {
                VisitBitsStore(lowering, fn, store);
              },
              [&](const lir::BeginStorageInstr& begun) {
                VisitBeginStorage(lowering, fn, begun.place);
              },
              [&](const lir::EndStorageInstr& ended) {
                VisitPlace(lowering, fn, ended.place);
              },
              [&](const lir::EndAndReplaceInstr& replaced) {
                VisitPlace(lowering, fn, replaced.place);
                VisitRvalue(
                    lowering, fn, replaced.value,
                    lir::PlaceType(*unit_, fn, replaced.place));
              },
              [](const lir::StorageInstr&) {},
              [](const lir::EndVariableInstr&) {},
              [](const lir::SetObjectClassInstr&) {},
              [](const lir::BeginHandlingInstr&) {},
              [](const lir::EndHandlingInstr&) {}},
          instr.data);
    }
    if (const auto* invoke =
            std::get_if<lir::InvokeTerm>(&block.terminator.data)) {
      visit_destination(invoke->destination);
      VisitCall(
          lowering,
          lir::CallInstr{.target = invoke->target, .args = invoke->args},
          lir::DestinationType(*unit_, fn, invoke->destination));
    }
  }
}

}  // namespace lyra::backend::llvm_backend
