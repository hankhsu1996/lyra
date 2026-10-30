#pragma once

#include <cstdint>
#include <functional>
#include <optional>
#include <span>
#include <utility>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/fixed_name.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/enclosing_hops.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::mir {
struct Class;
struct Block;
}  // namespace lyra::mir

namespace lyra::lowering::hir_to_mir {

class BreakTarget;
class CallableBindings;
struct SettledPath;
struct WalkFrame;

// What a scope the walk is inside owes on its normal ways out -- falling off
// its end, or a `return`, `break` or `continue` jumping out of it -- and the
// next such scope around it. Each way out emits `emit` where it leaves, as
// clang's code generation keeps a normal cleanup on its scope stack and emits
// it on every normal edge out of the scope. A departure leaves without it. A
// scope pushes one only where it owes something, and what it owes is its own
// business: whoever leaves through it runs it without knowing what it is.
struct NormalCleanup {
  std::function<void(const WalkFrame& at)> emit;
  const NormalCleanup* outer = nullptr;
};

// The arguments of a call made later than its statement is reached, in a body
// of its own: a deferred assertion's action, whose by-value arguments are
// evaluated where the assertion is (LRM 16.4). Each of `operands` the call
// reads as a value is lowered in `statement` and carried into the body, as a
// C++ lambda's init-capture is; one it designates -- a `ref` actual -- is
// designated in the body, when the call runs.
struct ArgumentsEvaluatedEarlier {
  std::span<const hir::ExprId> operands;
  const WalkFrame* statement = nullptr;
};

// A class the walk is in or can climb to, and the identity it was minted
// under. Reaching one is reaching the other: a reference to a member reads its
// type off the declaration and qualifies its target with the owner, so a walk
// that resolved only one of the two would immediately go looking for the
// other. `enclosing` is the member the class a generate block published keeps
// the scope that built it in, which is how a walk climbs out of it, and
// `published` is the pointer to that published class a walk climbing to this
// one answers with: what a body reaches of a scope enclosing it is what that
// scope published, whichever body it is. So a scope the walk only climbs to is
// known by its published class alone, and holds no declaration in `cls`.
struct EnclosingClass {
  mir::Class* cls = nullptr;
  mir::ClassId id{};
  std::optional<mir::ClassFieldTarget> enclosing;
  std::optional<mir::TypeId> published;
};

// How a body reaches the instance of the structural scope its outward
// references count from. A body of a namespace unit belongs to no instance,
// since nothing replicates a namespace (LRM 26.2), and neither does a body of a
// class one declares. A body of a structural scope is that instance. A body of
// a class the scope declares belongs to one instance of it (LRM 6.22) and
// reaches that instance through what it was handed: the member the object
// records it in, or -- where no object records it, in a receiver-less callable
// of such a class or in the constructor that is still building one -- the
// parameter its callers supply, which is a binding of the body like any other
// so a closure inside it reaches the instance by capture. Whoever declares the
// body says which, because only it knows where the body was written; the four
// are one closed set because they answer one question, and every consumer that
// needs the instance asks it once.
struct NoScope {};

struct ScopeIsSelf {};

struct ScopeThroughMember {
  mir::FieldId member;
};

struct ScopeThroughParameter {};

using StructuralBase = std::variant<
    NoScope, ScopeIsSelf, ScopeThroughMember, ScopeThroughParameter>;

// Singly-linked node carrying a class's parent chain so a leaf reference
// can read the declared type of a member at `hops > 0`. Each node lives on
// the stack frame of the construction walk that pushed it; the chain extends
// one node per class opened during traversal.
struct ScopeChainNode {
  EnclosingClass outer;
  const ScopeChainNode* parent = nullptr;
};

// Which of a variable's two values a read of it answers with. The sampled value
// of an expression is defined over the sampled values of the variables it reads
// (LRM 16.5.1), so lowering an operand under `kPreponed` is that definition
// applied at the one site a leaf read is built.
enum class ReadsAsOf : std::uint8_t {
  kNow,
  kPreponed,
};

// Per-recursion traversal context for HIR-to-MIR. Carried by value through
// every dispatcher method and per-kind handler. Walk-invariant facts (the
// compilation unit being constructed, builtins) live on the Lowerer class,
// not here. WalkFrame holds only state that genuinely changes from one
// recursion to the next.
//
// A handler writes nested IR through the frame's current targets; output that
// belongs to the compilation unit as a whole -- a synthesized type, a
// deferred-check site -- is appended to the unit directly, since it has no
// place on a per-recursion frame.
struct WalkFrame {
  // The class write target: the class a class-constructing task builds,
  // entered via `WithClass`, with its identity, so an owner-qualified field or
  // method target names it without a reverse lookup. Its class is null
  // outside class handlers.
  EnclosingClass current_class;

  // The classes around the current one, innermost first. Populated by
  // `WithClass` when the construction
  // walk opens a new class. Read via `EnclosingClassAtHops` to resolve a member
  // reference at `hops > 0`. This is the object-graph axis, distinct from the
  // lexical binding axis carried by `bindings`.
  const ScopeChainNode* outer_classes = nullptr;

  // The block whatever is being lowered goes into, its nodes and any step it
  // takes. It runs exactly when the code lowered into it runs, so a construct
  // that evaluates an operand only on some runs lowers that operand into a
  // block of its own (LRM 11.3.5). Set when a walker opens a
  // new block (process body, nested block body, fork branch body, closure body,
  // a block expression) and entered via `WithBlock`. Null outside a block. A
  // block places statements and exprs; it does not resolve a reference to its
  // binding.
  mir::Block* current_block = nullptr;

  // Which value a read of an observable cell answers with while this subtree is
  // lowered. Set for the operand of a construct that reads sampled values (LRM
  // 16.5.1); the frame is carried by value, so it reaches that operand and
  // nothing beside it.
  ReadsAsOf reads_as_of = ReadsAsOf::kNow;

  // The report the evaluation being lowered states the places it reaches in --
  // an object a property is read on, an interface variable read through a
  // handle, a function called -- where a waiting process decides its wait by
  // this evaluation (LRM 9.4.2). Each place is stated where the evaluation
  // reaches it, from the value it reached it through, so the evaluation is the
  // one the source wrote. Absent for every other evaluation; a callable
  // boundary leaves it behind, since the report is a variable of this body.
  std::optional<mir::LocalId> reports_reached_to;

  // Whether the body being lowered runs as a function call, as a function, a
  // constructor and a final procedure do (LRM 9.2.3). Such a body cannot wait
  // (LRM 13.4), and nothing else can reach one of its variables while it runs
  // -- a branch it forks outlives it and so holds what it names in storage of
  // its own -- so a variable it declares has no one to report its writes to.
  // What it schedules past its return is allowed only where the calling thread
  // was created by an initial or always procedure, or a fork of one (LRM
  // 13.4.4). Set where a callable's frame is built; a fork branch is a process
  // of its own.
  bool runs_as_function_call = false;

  // The binding-resolution context of the callable body being lowered: a
  // reference resolves to a binding through it, and entering a closure body
  // installs a child context whose parent is this one (so a capture forwards
  // one callable boundary at a time). Stable through nested blocks of one
  // callable; replaced at a callable boundary.
  CallableBindings* bindings = nullptr;

  // The name of the scope the construct being lowered is written in, which
  // it reports as its own (LRM 21.2.1.5). Set where a declaration scope builds
  // the frame its bodies descend from, and again wherever a procedural scope
  // is entered; it points at an answer that outlives the lowering.
  const hir::FixedName* scope_name = nullptr;

  // The queue the innermost enclosing select is taken from, while that select's
  // index or bounds are lowered: what `$` there reads the last index of (LRM
  // 7.10.1). The select evaluates the queue once and owns this on its stack
  // frame; it is set per select, so each `$` of a nested `q[r[$]]` reads the
  // queue its own select is taken from. Null outside such a subtree.
  const SettledPath* selected_queue = nullptr;

  // The loop a `break` lowered here leaves: the innermost loop the source
  // wrote around it (LRM 12.8), the outermost of the nest a `foreach` is built
  // as, or a walk over an array's entries that a visit may end early. The
  // break names it rather than leaving the innermost loop, because what the
  // break stands in -- a case's items, built as a switch or a block left by a
  // break of its own -- is not that loop. The loop lowering owns it on its
  // stack, and it is null outside any loop of this body.
  BreakTarget* break_leaves = nullptr;

  // What the scopes of this body the walk is inside owe on their normal ways
  // out, innermost first, each owned on the stack of its scope's lowering.
  // `loop_normal_cleanups` is what of that stood outside the body of the loop
  // a `break` leaves, so a `break` or `continue` runs what is in front of it.
  const NormalCleanup* normal_cleanups = nullptr;
  const NormalCleanup* loop_normal_cleanups = nullptr;

  // How this body reaches the instance of the structural scope its outward
  // references count from. Settled once where the body's frame is built, so
  // nothing that resolves a reference asks what kind of body it is in.
  StructuralBase structural_base;

  // The call being lowered into a body that runs after its statement, whose
  // by-value arguments that statement evaluates. Null elsewhere.
  const ArgumentsEvaluatedEarlier* arguments_evaluated_earlier = nullptr;

  // The local holding the problem of the call of `randomize()` a constraint is
  // stated to (LRM 18.5, 18.7), in a constraint block's body or the
  // constraints written with a call. Absent anywhere no constraint is lowered.
  std::optional<mir::LocalId> constraint_problem;

  // The local holding the object a `randomize() with` call randomizes, which a
  // name in the constraints written with the call reaches its class's
  // properties through (LRM 18.7). Absent outside those constraints.
  std::optional<mir::LocalId> randomized_object;

  // How far out of `self` that instance sits, in class-chain steps. Answered
  // per alternative rather than by testing for one, so a body that reaches its
  // instance some further way has to say how far out it sits.
  [[nodiscard]] auto StructuralBaseHops() const -> std::uint32_t {
    return std::visit(
        Overloaded{
            [](const NoScope&) -> std::uint32_t { return 0; },
            [](const ScopeIsSelf&) -> std::uint32_t { return 0; },
            [](const ScopeThroughMember&) -> std::uint32_t { return 1; },
            [](const ScopeThroughParameter&) -> std::uint32_t { return 1; }},
        structural_base);
  }

  // Pushes `cls` as the current class and links the previous one into the
  // outer chain through `chain_node`, which the caller stack-allocates so its
  // lifetime spans the descent.
  [[nodiscard]] auto WithClass(
      const EnclosingClass& cls, ScopeChainNode& chain_node) const
      -> WalkFrame {
    chain_node.outer = current_class;
    chain_node.parent = outer_classes;
    WalkFrame next = *this;
    next.current_class = cls;
    next.outer_classes =
        current_class.cls != nullptr ? &chain_node : outer_classes;
    return next;
  }

  // Starts the walk of a structural scope's bodies on `cls`, standing inside
  // the scopes `around` chains, the nearest first.
  [[nodiscard]] auto WithinScopes(
      const EnclosingClass& cls, const ScopeChainNode* around) const
      -> WalkFrame {
    WalkFrame next = *this;
    next.current_class = cls;
    next.outer_classes = around;
    return next;
  }

  // Enters a scope the construct lowered below is written in.
  [[nodiscard]] auto WithScopeName(const hir::FixedName& name) const
      -> WalkFrame {
    WalkFrame next = *this;
    next.scope_name = &name;
    return next;
  }

  // The class at `hops` -- 0 is the one being written, N climbs N steps out --
  // and the identity it was minted under. One answer, because a member
  // reference needs both: the declaration to read the member's type off, and
  // the owner to qualify the target with.
  [[nodiscard]] auto EnclosingClassAtHops(mir::EnclosingHops raw_hops) const
      -> EnclosingClass {
    const mir::EnclosingHops hops{raw_hops.value + StructuralBaseHops()};
    if (hops.value == 0) {
      if (current_class.cls == nullptr) {
        throw InternalError(
            "WalkFrame::EnclosingClassAtHops: no current class");
      }
      return current_class;
    }
    const ScopeChainNode* node = outer_classes;
    for (std::uint32_t step = 1; step < hops.value && node != nullptr; ++step) {
      node = node->parent;
    }
    if (node == nullptr) {
      throw InternalError(
          "WalkFrame::EnclosingClassAtHops: hops exceed chain depth");
    }
    return node->outer;
  }

  // How many steps out of the scope this body counts from the class its unit
  // is rooted at sits, which is what a body reaching a member of its unit
  // instance names. The outer chain starts empty and grows one node per class
  // the walk opens, so its depth is the distance from the current class; a
  // body of a class an instance declares counts from that instance, which is
  // that many steps nearer.
  [[nodiscard]] auto HopsToUnitRoot() const -> mir::EnclosingHops {
    std::uint32_t hops = 0;
    for (const ScopeChainNode* node = outer_classes; node != nullptr;
         node = node->parent) {
      ++hops;
    }
    return mir::EnclosingHops{hops - StructuralBaseHops()};
  }

  [[nodiscard]] auto WithRunsAsFunctionCall(bool as_function_call) const
      -> WalkFrame {
    WalkFrame next = *this;
    next.runs_as_function_call = as_function_call;
    return next;
  }

  [[nodiscard]] auto WithArgumentsEvaluatedEarlier(
      const ArgumentsEvaluatedEarlier* arguments) const -> WalkFrame {
    WalkFrame next = *this;
    next.arguments_evaluated_earlier = arguments;
    return next;
  }

  [[nodiscard]] auto WithReadsAsOf(ReadsAsOf when) const -> WalkFrame {
    WalkFrame next = *this;
    next.reads_as_of = when;
    return next;
  }

  [[nodiscard]] auto WithReportingReachedTo(
      std::optional<mir::LocalId> report) const -> WalkFrame {
    WalkFrame next = *this;
    next.reports_reached_to = report;
    return next;
  }

  [[nodiscard]] auto WithBlock(mir::Block* block) const -> WalkFrame {
    WalkFrame next = *this;
    next.current_block = block;
    return next;
  }

  // Enters the index or a bound of a select taken from `queue`, which outlives
  // the descent on the caller's stack.
  [[nodiscard]] auto WithSelectedQueue(const SettledPath* queue) const
      -> WalkFrame {
    WalkFrame next = *this;
    next.selected_queue = queue;
    return next;
  }

  // Enters the body of `loop`, which a `break` there leaves by naming it.
  [[nodiscard]] auto WithBreakLeaving(BreakTarget& loop) const -> WalkFrame {
    WalkFrame next = *this;
    next.break_leaves = &loop;
    next.loop_normal_cleanups = normal_cleanups;
    return next;
  }

  // Enters a scope that owes `cleanup` on its normal ways out.
  [[nodiscard]] auto WithNormalCleanup(const NormalCleanup& cleanup) const
      -> WalkFrame {
    WalkFrame next = *this;
    next.normal_cleanups = &cleanup;
    return next;
  }

  [[nodiscard]] auto WithConstraintProblem(mir::LocalId problem) const
      -> WalkFrame {
    WalkFrame next = *this;
    next.constraint_problem = problem;
    return next;
  }

  [[nodiscard]] auto WithRandomizedObject(mir::LocalId object) const
      -> WalkFrame {
    WalkFrame next = *this;
    next.randomized_object = object;
    return next;
  }

  [[nodiscard]] auto WithBindings(CallableBindings* callable_bindings) const
      -> WalkFrame {
    WalkFrame next = *this;
    next.bindings = callable_bindings;
    // A label is one body's, so no break of another body leaves a loop of
    // this one, which LRM 12.8 forbids across a fork-join block as well; nor
    // does any jump of it leave a block of this one.
    next.break_leaves = nullptr;
    next.normal_cleanups = nullptr;
    next.loop_normal_cleanups = nullptr;
    return next;
  }

  // Enters a body of a class the enclosing structural scope declares, stating
  // how that body reaches the instance it counts from.
  [[nodiscard]] auto WithStructuralBase(StructuralBase base) const
      -> WalkFrame {
    WalkFrame next = *this;
    next.structural_base = std::move(base);
    return next;
  }
};

// Where the owner of a cell that outlives every activation brings it up, in the
// two phases every such cell takes: `install` receives the cell's declared
// representation and default, and `value` the LRM 10.5 value initializer. An
// owner that brings both up in one body names it twice; a namespace unit keeps
// them apart, so that every cell in the design is installed before any
// initializer reads one. The two travel together wherever a cell comes up, and
// pairing the wrong install with a value would put a cell's representation
// where its contents go, so they cross as one value.
struct StorageBringUp {
  WalkFrame install;
  WalkFrame value;
};

}  // namespace lyra::lowering::hir_to_mir
