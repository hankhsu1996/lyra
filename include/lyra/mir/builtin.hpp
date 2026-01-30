#pragma once

namespace lyra::mir {

// BuiltinMethod classifies into two categories:
//
// PURE (rvalue via Compute instruction - no receiver mutation):
// - kNewArray: creates new handle (allocation, not mutation of existing state)
// - kArraySize, kQueueSize: read-only query
// - kEnumNext, kEnumPrev, kEnumName: pure computation
//
// CONTAINER-MUTATING (BuiltinCall instruction - mutates receiver):
// - kArrayDelete: clears dynamic array
// - kQueueDelete, kQueueDeleteAt: remove elements
// - kQueuePushBack, kQueuePushFront, kQueueInsert: add elements
// - kQueuePopBack, kQueuePopFront: remove and return element
//
// Pure builtins remain as BuiltinCallRvalueInfo in Compute instructions.
// Container-mutating builtins use the BuiltinCall instruction.
enum class BuiltinMethod {
  // Dynamic array operations
  kNewArray,     // new[size] or new[size](init) - returns array (pure)
  kArraySize,    // arr.size() - returns int (pure)
  kArrayDelete,  // arr.delete() - void, mutates receiver

  // Queue operations
  kQueueSize,       // q.size() - returns int (pure)
  kQueueDelete,     // q.delete() - clear all, mutates receiver
  kQueueDeleteAt,   // q.delete(idx) - remove at index, mutates receiver
  kQueuePushBack,   // q.push_back(val) - append, mutates receiver
  kQueuePushFront,  // q.push_front(val) - prepend, mutates receiver
  kQueuePopBack,    // q.pop_back() - returns element, mutates receiver
  kQueuePopFront,   // q.pop_front() - returns element, mutates receiver
  kQueueInsert,     // q.insert(idx, val) - insert at index, mutates receiver

  // Enum operations (runtime only - first/last/num are constant-folded)
  kEnumNext,  // e.next([N]) - returns next enum value (pure)
  kEnumPrev,  // e.prev([N]) - returns previous enum value (pure)
  kEnumName,  // e.name() - returns string name of current value (pure)
};

}  // namespace lyra::mir
