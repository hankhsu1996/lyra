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
            // Container access (including the write-side reference forms)
            // is non-mutating at the call itself; whether the chain ends in
            // a write is decided by the outer `AssignExpr`. Only the in-
            // place mutators below name themselves as receiver mutators.
            return m.kind == ArrayMethodKind::kDelete ||
                   m.kind == ArrayMethodKind::kReverse ||
                   m.kind == ArrayMethodKind::kSort ||
                   m.kind == ArrayMethodKind::kRsort;
          },
          [](const QueueMethodInfo& m) {
            return m.kind != QueueMethodKind::kSize;
          },
          [](const AssociativeMethodInfo& m) {
            // Reads and the write-side reference both surface as
            // non-mutating receivers here; mutation is pinned by the outer
            // `AssignExpr` in the LHS-render dispatch.
            return m.kind == AssociativeMethodKind::kDelete;
          },
          [](const ValueMethodInfo&) { return false; },
          [](const IteratorMethodInfo&) { return false; },
          [](const ScopeMethodInfo&) { return false; },
          // The observable-cell methods carry their mutating intent on the
          // kind itself; the wrap rule that drives this trait does not
          // recurse into them.
          [](const ObservableMethodInfo&) { return false; },
      },
      callee.method);
}

auto IsContainerAccessCall(const BuiltinMethodCallee& callee) -> bool {
  return std::visit(
      Overloaded{
          [](const ArrayMethodInfo& m) {
            return m.kind == ArrayMethodKind::kElement ||
                   m.kind == ArrayMethodKind::kElementRef ||
                   m.kind == ArrayMethodKind::kSlice ||
                   m.kind == ArrayMethodKind::kSliceRef;
          },
          [](const AssociativeMethodInfo& m) {
            return m.kind == AssociativeMethodKind::kElement ||
                   m.kind == AssociativeMethodKind::kElementRef;
          },
          [](const QueueMethodInfo& m) {
            return m.kind == QueueMethodKind::kElement ||
                   m.kind == QueueMethodKind::kElementRef ||
                   m.kind == QueueMethodKind::kSlice;
          },
          [](const auto&) { return false; },
      },
      callee.method);
}

auto IsMutatingBuiltinFn(BuiltinFn id) -> bool {
  switch (id) {
    case BuiltinFn::kPutc:
    case BuiltinFn::kItoa:
    case BuiltinFn::kHextoa:
    case BuiltinFn::kOcttoa:
    case BuiltinFn::kBintoa:
    case BuiltinFn::kRealtoa:
    case BuiltinFn::kDelete:
    case BuiltinFn::kReverse:
    case BuiltinFn::kSort:
    case BuiltinFn::kRsort:
    case BuiltinFn::kInsert:
    case BuiltinFn::kPopFront:
    case BuiltinFn::kPopBack:
    case BuiltinFn::kPushFront:
    case BuiltinFn::kPushBack:
      return true;
    default:
      return false;
  }
}

auto IsContainerAccessFn(BuiltinFn id) -> bool {
  switch (id) {
    case BuiltinFn::kElement:
    case BuiltinFn::kElementRef:
    case BuiltinFn::kSlice:
    case BuiltinFn::kSliceRef:
      return true;
    default:
      return false;
  }
}

}  // namespace lyra::mir
