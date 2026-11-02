#pragma once

#include <cstddef>
#include <optional>
#include <set>
#include <span>
#include <vector>

#include "lyra/backend/llvm/fn_abi.hpp"
#include "lyra/backend/llvm/target_lowering.hpp"
#include "lyra/backend/llvm/witness_tables.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/type_id.hpp"
#include "lyra/support/value_operation.hpp"

namespace lyra::backend::llvm_backend {

// Finds the operations of types that what this target hands the library needs,
// off finished LIR, the role of rustc's `MirUsedCollector`: a value's type (LRM
// 4.3, Table 6-7), an element's (LRM 7.4.5), what compares and streams a value
// (LRM 11.4.5, 20.6.2, 20.9, 6.24.3), what takes some of its positions (LRM
// 10.6.2) and what a net folds it by (LRM 6.6). A table is made whole, as a
// vtable collects every method of its trait. This walks LIR and asks the
// target's lowering what each node hands over, the answer emission arranges
// too; what a table names in turn it follows from the one list emission fills
// the table from. A node this target refuses uses nothing, and the refusal is
// left to emission, which says where it stands.
//
// It reads a unit that grows: each call of `CollectUsed` reads what the unit
// has gained since the last.
class TypeOperationCollector {
 public:
  // What the storage and the records the unit declares use is read here.
  explicit TypeOperationCollector(const lir::CompilationUnit& unit);

  // What the functions given a body since the last call use, in the order they
  // use it, with what the unit declares on the first call. An operation may be
  // listed more than once.
  [[nodiscard]] auto CollectUsed() -> std::vector<lir::TypeOperation>;

 private:
  void VisitFunction(const lir::Function& fn);
  void VisitPlace(
      const TargetLowering& lowering, const lir::Function& fn,
      const lir::Place& place);
  void VisitRvalue(
      const TargetLowering& lowering, const lir::Function& fn,
      const lir::Rvalue& value, lir::TypeId result_type);
  void VisitCall(
      const TargetLowering& lowering, const lir::CallInstr& call,
      lir::TypeId result_type);
  void VisitStore(
      const TargetLowering& lowering, const lir::Function& fn,
      const lir::StoreInstr& store);
  void VisitBitsStore(
      const TargetLowering& lowering, const lir::Function& fn,
      const lir::BitsStoreInstr& store);
  void VisitBeginStorage(
      const TargetLowering& lowering, const lir::Function& fn,
      const lir::Place& place);

  void Use(lir::TypeId type, support::ValueOperation answers);
  void UseFilling(lir::TypeId type, const WitnessFilling& filling);
  void UseTable(lir::TypeId type, std::span<const Witness> table);
  void VisitValueTypeUse(lir::TypeId type);
  void VisitElementTypeUse(lir::TypeId type);
  void VisitOperationsUse(lir::TypeId type);
  void VisitNetFoldsUse(lir::TypeId type);
  void VisitTakePositionsUse(lir::TypeId type);

  // What a node hands over; a node the target refuses hands nothing.
  void VisitAbi(const FnAbi& abi);
  void VisitAbi(const std::optional<FnAbi>& abi);
  void VisitAbi(const diag::Result<FnAbi>& abi);
  void VisitMode(const PassMode& mode);

  const lir::CompilationUnit* unit_;
  std::size_t functions_seen_ = 0;
  std::vector<lir::FunctionId> unread_;
  std::vector<lir::TypeOperation> used_;
  std::set<lir::TypeId> value_types_seen_;
  std::set<lir::TypeId> element_types_seen_;
  std::set<lir::TypeId> operation_types_seen_;
};

// A collector of `unit` as what the pipeline is handed, which names no target.
[[nodiscard]] auto TypeOperationCollectionOf(const lir::CompilationUnit& unit)
    -> lir::UsedTypeOperations;

}  // namespace lyra::backend::llvm_backend
