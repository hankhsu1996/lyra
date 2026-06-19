#include "lyra/mir/builtin_method.hpp"

#include <variant>

#include "lyra/base/overloaded.hpp"

namespace lyra::mir {

auto IsMutatingBuiltinMethod(const BuiltinMethodCallee& callee) -> bool {
  return std::visit(
      Overloaded{
          [](const EnumMethodInfo&) { return false; },
          [](const StringMethodInfo& m) {
            // LRM 6.16: Putc and the int/real-to-string family overwrite the
            // receiver string.
            return m.kind == StringMethodKind::kPutc ||
                   m.kind == StringMethodKind::kItoa ||
                   m.kind == StringMethodKind::kHextoa ||
                   m.kind == StringMethodKind::kOcttoa ||
                   m.kind == StringMethodKind::kBintoa ||
                   m.kind == StringMethodKind::kRealtoa;
          },
          // LRM 15.5 Trigger / Triggered reach into RuntimeServices through an
          // explicit services argument and do not mutate the named event slot
          // through the wrapper API.
          [](const EventMethodInfo&) { return false; },
          [](const ArrayMethodInfo& m) {
            // Container-access (`kElementAt`, `kSlice`) and ref-to-value
            // (`kToOwned`) are non-mutating themselves -- they return a
            // borrowed view or an owning value; whether the chain ends in a
            // write is determined by what consumes the result, not by these
            // methods.
            return m.kind == ArrayMethodKind::kDelete ||
                   m.kind == ArrayMethodKind::kReverse ||
                   m.kind == ArrayMethodKind::kSort ||
                   m.kind == ArrayMethodKind::kRsort;
          },
          [](const QueueMethodInfo& m) {
            return m.kind != QueueMethodKind::kSize;
          },
          [](const AssociativeMethodInfo& m) {
            // `kRead` returns a borrowed view; `kElementRef` allocates on
            // miss but the mutation pipeline is closed by an outer
            // `AssignExpr`, so they read as non-mutating receivers from the
            // observable-mutate / LHS-render dispatch's point of view.
            return m.kind == AssociativeMethodKind::kDelete;
          },
          [](const ValueMethodInfo&) { return false; },
          [](const IteratorMethodInfo&) { return false; },
          [](const ScopeMethodInfo&) { return false; },
          // Get / Set / Mutate are the observable cell API itself -- their
          // mutating nature is encoded by which kind is invoked, not by a
          // surrounding wrapper. The wrap rule that drives this trait does
          // not recurse into them.
          [](const ObservableMethodInfo&) { return false; },
      },
      callee.method);
}

auto IsContainerAccessCall(const BuiltinMethodCallee& callee) -> bool {
  return std::visit(
      Overloaded{
          [](const ArrayMethodInfo& m) {
            return m.kind == ArrayMethodKind::kElementAt ||
                   m.kind == ArrayMethodKind::kSlice;
          },
          [](const AssociativeMethodInfo& m) {
            return m.kind == AssociativeMethodKind::kRead ||
                   m.kind == AssociativeMethodKind::kElementRef;
          },
          [](const QueueMethodInfo& m) {
            return m.kind == QueueMethodKind::kElementAt ||
                   m.kind == QueueMethodKind::kWriteRef ||
                   m.kind == QueueMethodKind::kSlice;
          },
          [](const auto&) { return false; },
      },
      callee.method);
}

}  // namespace lyra::mir
