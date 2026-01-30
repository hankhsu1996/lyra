# Module Hierarchy

How Lyra handles hierarchical module instantiation and port connections.

Key design: **Transform once at AST->MIR, simplify downstream stages**.

## Port Connections

### Input Ports

Both backends use **driver processes** for input port connections.

**MIR Transformation**: When AST->MIR encounters `Child c(.a(x))`:

1. Creates `AssignmentStatement` with hierarchical target: `child.a = x`
2. Wraps it in an implicit `always_comb` process with sensitivity to `x`

No binding is stored in MIR - input ports are handled purely through driver processes.

**Codegen**:

```cpp
class Child {
 public:
  Int a;  // Input port: public member variable
};

class Parent {
  Child child_;

  auto _port_driver_0() -> Task {
    while (true) {
      child_.a = x;  // Direct member access
      co_await AnyChange(&x);
    }
  }
};
```

**Interpreter**:

1. Child's input port is a regular variable in flat `VariableStore`
2. Parent's driver process executes `store_hierarchical()`
3. `store_hierarchical()` traverses `HierarchyContext::children` map to find child
4. Writes directly to flat variable storage

### Output Ports

Backends differ for output ports due to platform constraints.

**MIR Transformation**: When AST->MIR encounters `Child c(.out(result))`:

1. Slang represents this as `Assignment(result = internal_out)`
2. MIR extracts and stores `OutputBinding{port_name: "out", signal: result}`

**Codegen**:

```cpp
class Child {
 public:
  Child(Int& out) : out_(out) {}  // Constructor takes reference

 private:
  Int& out_;  // Reference member bound to parent's storage

  void some_process() {
    out_ = computed_value;  // Writes directly to parent
  }
};

class Parent {
  Int result;
  Child child_{result};  // Pass parent's variable to constructor
};
```

Child writes to `out_` -> goes directly to `Parent::result` (C++ reference semantics).

**Interpreter**:

1. During elaboration, creates `PortBinding` in child's `HierarchyContext`:

   ```
   child.port_bindings[out_symbol] = {target: result_symbol, instance: parent}
   ```

2. When child writes to output port:
   ```
   store_variable(out_symbol, value)
     -> ResolveBinding(out_symbol)
     -> returns (result_symbol, parent_instance)
     -> parent_instance->Write(result_symbol, value)
   ```

**Why the difference?** C++ reference semantics bind at compile time. The interpreter cannot replicate this - it must resolve bindings at runtime. This is a platform constraint, not an optimization choice.

### Inout Ports

Same as output ports - reference member in codegen, port binding in interpreter.

## Hierarchical Variable Access

### MIR Representation

**Hierarchical Read** (e.g., `result = u_child.value`):

```cpp
HierarchicalReferenceExpression {
  instance_path: [&u_child_symbol]  // Instance traversal path
  target_symbol: &value_symbol      // The variable to read
}
```

**Hierarchical Write** (e.g., `u_child.port = expr`):

```cpp
AssignmentTarget {
  instance_path: [&u_child_symbol]  // Instance traversal path
  target_symbol: &port_symbol       // Target variable
}
```

Both use symbol-based paths only. Codegen reconstructs string names on-demand from `symbol->name`.

### Codegen: Hierarchical Access

**Emit hierarchical path**: Converts symbol path to C++ member access.

```cpp
void EmitHierarchicalPath(
    const vector<SymbolRef>& instance_path, SymbolRef target_symbol) {
  for (const auto& inst_sym : instance_path) {
    out_ << inst_sym->name << "_.";  // Instance names get _ suffix
  }
  out_ << target_symbol->name;
}
```

**Read**: `result = u_child_.value;`
**Write**: `u_child_.port = expr;`

Both compile to direct C++ member access - no runtime lookup needed.

### Interpreter: Hierarchical Access

**`load_hierarchical(instance_path, target_symbol)`**:

```cpp
auto target_instance = current_instance;
for (const auto& inst_sym : instance_path) {
  target_instance = target_instance->LookupChild(inst_sym);  // O(1) hash lookup
}
return target_instance->Read(target_symbol);
```

**`store_hierarchical(instance_path, target_symbol, value)`**:

```cpp
auto target_instance = current_instance;
for (const auto& inst_sym : instance_path) {
  target_instance = target_instance->LookupChild(inst_sym);
}
target_instance->Write(target_symbol, value);
effect.RecordVariableModification(target_symbol, target_instance);
```

**Key insight**: Uses symbol pointers for O(1) lookup. Slang symbols are globally unique - the instance path provides navigation, the symbol provides identity.

## Sensitivity and Triggers

For `always_comb result = u_child.value + 1`, sensitivity includes the hierarchical reference.

### Trigger Structure

```cpp
Trigger {
  variable: &value_symbol           // Signal being watched
  instance_path: [&u_child_symbol]  // Path to instance (empty if local)
  edge_kind: AnyChange              // Or Posedge, Negedge
}
```

### Codegen: Trigger Emission

```cpp
string GetTriggerPath(const Trigger& trigger) {
  string path;
  for (const auto& inst_sym : trigger.instance_path) {
    path += inst_sym->name + "_.";  // Instance names get _ suffix
  }
  path += trigger.variable->name;
  return path;
}
```

Emits: `co_await AnyChange(&u_child_.value);`

### Interpreter: Trigger Resolution

When registering a wait:

1. Traverse `instance_path` to reach target instance
2. Resolve through port bindings (if signal is bound output port)
3. Register with `TriggerManager` using resolved (symbol, instance) pair

```cpp
auto binding_instance = current_instance;
for (const auto& inst_sym : trigger.instance_path) {
  binding_instance = binding_instance->LookupChild(inst_sym);
}
auto [target_symbol, resolved_instance] = binding_instance->ResolveBinding(trigger.variable);
trigger_manager.RegisterWaitingProcess(process, target_symbol, resolved_instance);
```

## Interpreter: Hierarchy Context

Each module instance has a `HierarchyContext` for port binding resolution:

```cpp
struct HierarchyContext {
  string hierarchy_path;  // "top.counter1" for debugging

  // Output port bindings: port symbol -> (parent_symbol, parent_instance)
  unordered_map<SymbolRef, PortBinding> port_bindings;

  // Child instances for hierarchical access
  unordered_map<SymbolRef, shared_ptr<HierarchyContext>> children;
};
```

Variables are stored in a flat `VariableStore` (keyed by symbol), not per-instance.
Port bindings resolve output port writes to the correct target in flat storage.

### Elaboration

During elaboration (`SimulationRunner::ElaborateSubmodules`):

1. For each submodule instance in MIR:
2. Create child `HierarchyContext` with output port bindings
3. Store child in parent's `children` map (keyed by instance symbol)
4. Initialize child's variables
5. Schedule child's processes with child's instance context
6. Recursively elaborate nested submodules

### Binding Flattening

Bindings are flattened at elaboration. For `Top -> Middle -> Inner`:

```systemverilog
module Inner(output int out);
module Middle(output int out);
  Inner i(.out(out));  // Inner.out -> Middle.out
endmodule
module Top;
  int result;
  Middle m(.out(result));  // Middle.out -> Top.result
endmodule
```

At elaboration:

- Middle's binding: `out -> (result, Top)`
- Inner's binding: `out -> (result, Top)` (flattened, not `out -> Middle`)

This avoids chained lookups - Inner writes directly to Top's storage.

## Summary

| Aspect          | Codegen                              | Interpreter                                 |
| --------------- | ------------------------------------ | ------------------------------------------- |
| **Input port**  | Public member, driver process writes | Driver process uses `store_hierarchical()`  |
| **Output port** | Reference member `T&`                | `PortBinding` resolved at runtime           |
| **Hier read**   | `child_.signal`                      | `load_hierarchical()` traverses `children`  |
| **Hier write**  | `child_.port = x`                    | `store_hierarchical()` traverses `children` |
| **Triggers**    | `GetTriggerPath()`                   | Traverse + resolve bindings                 |

## Related

- `docs/parameterized-modules.md` - Per-instance IR semantics and C++ template mapping
