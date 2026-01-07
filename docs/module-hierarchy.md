#Module Hierarchy

##Overview

        Lyra has two execution backends :

    |
    Aspect | C++ Codegen | Interpreter | | -- -- -- -- -|
    -- -- -- -- -- -- -- -- -- -- -- -- -| -- -- -- -- -- -- -- -- -- -- -- -| |
    Path | MIR → C++ → compile → run | MIR → LIR → interpret | | Speed |
    Fast(native code) | Slower(interpretation) |
    | Use case | Production simulation | Development,
    debugging | | Hierarchy | Constructor instantiation | Elaboration + context
        |

        Both backends support hierarchical modules and produce identical
        simulation results
            .

        ##Data Flow

```SV--->AST--->MIR--->C
        ++ Codegen(AssignmentStatement with hierarchical target) |
        +--->LIR--->Interpreter(InstanceContext + hierarchical store)
```

           Key design:

**Transform once at AST→MIR,
simplify downstream stages **.

    ##Input Port Connections

        All input port connections are transformed to implicit
            processes using hierarchical assignment :

```systemverilog
                // Both cases use the same model
                Child c(.a(x));  // Simple identifier
Child c(.a(x + y));              // Expression
```

### Transformation in AST→MIR

When AST→MIR encounters an input port connection, it:

1. Creates an `AssignmentStatement` with a hierarchical target (`child.port = expression`)
2. Wraps it in an implicit `always_comb`-like process
3. For simple identifiers, also stores binding for interpreter optimization

**MIR Result:**

```
module Top
  Adder add(.a(x), .out(result))  // Binding stored for interpreter

  process _port_driver_0          // Unified driver process
    while 1
      add.a = x                   // AssignmentStatement with hierarchical target
      @(x)
```

### Backend Handling

| Backend     | Simple `.a(x)`    | Expression `.a(x+y)` |
| ----------- | ----------------- | -------------------- |
| Codegen     | Driver process    | Driver process       |
| Interpreter | Binding + process | Process only         |

Both backends use the same `AssignmentStatement` with hierarchical `AssignmentTarget`. The interpreter additionally uses port bindings for simple connections as an optimization.

## MIR Representation

### Hierarchical Assignment Target

Port driving uses `AssignmentStatement` with a hierarchical target:

```cpp
struct AssignmentTarget {
  std::optional<common::Variable> variable;    // For local assignments
  std::vector<std::string> hierarchical_path;  // e.g., {"child", "a"}

  bool IsHierarchical() const {
    return !hierarchical_path.empty();
  }
};
```

    This represents driving a submodule's input port:

```add.a = x + y;  // hierarchical_path = {"add", "a"}

```

    ## #Process Creation Pattern

        Port driver processes follow the `always_comb` pattern :

```cpp
    // while (true) { assign_stmt; @(sensitivity_list); }
    auto
    CreateImplicitAlwaysComb(Statement driver_stmt, counter) -> Process {
  auto variables = CollectSensitivityList(*driver_stmt);
  // Create triggers from variables
  // Build: while(true) { driver; wait_event(triggers); }
}
```

    ##C++ Codegen

    ## #Input Port Model

        Input ports are public variables(not references)
    :

```cpp class Adder : public lyra::sdk::Module {
 public:
  Int a;  // Public for parent to drive
  // ...
};
```

    ## #Port Driver Process Emission

        The hierarchical assignment is emitted
            as :

```cpp auto _port_driver_0() -> lyra::sdk::Task {
  while (Bit<1>{1}) {
    add_.a = x + y;  // Direct assignment to child member
    co_await lyra::sdk::AnyChange(&y, &x);
  }
  co_return;
}
```

    ## #Output Port Model

        Output ports remain as references(parent owns storage)
    :

```cpp class Adder : public lyra::sdk::Module {
 public:
  Adder(Int& out) : out_(out) {
  }  // Reference to parent signal
  Int& out_;
};
```

## Interpreter Input Port Model

The interpreter supports input port connections through two mechanisms:

| Model        | Use Case             | How It Works                    |
| ------------ | -------------------- | ------------------------------- |
| Binding      | Simple `.a(x)`       | Symbol resolution to parent     |
| Hierarchical | Expression `.a(x+y)` | Process writes to child storage |

### Binding Model (Simple Ports)

For simple identifier connections:

1. **Elaboration**: Creates binding `child.input_port → parent.signal`
2. **Read**: Child reads `clk` → resolves to `Top.clk` → reads parent's value
3. **Wait**: Child waits on `@(posedge clk)` → resolves → waits on `Top.clk`

### Hierarchical Store Model (Expression Ports)

For expression connections:

1. **Process**: Driver process evaluates expression and writes to child
2. **Target**: Uses `InstanceContext::children` map to find child instance
3. **Store**: Writes directly to child's per-instance storage

Both models produce identical results - binding is an optimization that avoids the overhead of driver processes for simple cases.

## Port Directions

| Direction | C++ Codegen     | Interpreter        |
| --------- | --------------- | ------------------ |
| `input`   | Public variable | Binding or process |
| `output`  | `T&` reference  | Read-write binding |
| `inout`   | `T&` reference  | Read-write binding |

## Interpreter: Instance Context Design

The interpreter uses an LLVM-style instance context for hierarchy:

| Concept           | C++ Analogy      | Interpreter        |
| ----------------- | ---------------- | ------------------ |
| Module definition | Class definition | `lir::Module`      |
| Module instance   | Object instance  | `InstanceContext`  |
| Port binding      | Constructor ref  | Symbol mapping     |
| Process code      | Member function  | `lir::Process`     |
| Execution context | `this` pointer   | `InstanceContext*` |

Key insight: **One process definition, multiple instance contexts**.

### Symbol Resolution

```cpp
auto Resolve(SymbolRef symbol) const -> SymbolRef {
  auto it = port_bindings.find(symbol);
  return it != port_bindings.end() ? it->second : symbol;
}
```

    ## #Nested Hierarchy Flattening

        Bindings are flattened at elaboration time :

```systemverilog module
        Inner(input bit x);  // uses x
endmodule

    module
    Middle(input bit y);
Inner i(.x(y));
endmodule

    module Top;
bit sig;
Middle m(.y(sig));
endmodule
```

    Bindings :

```middle_instance : {y → Top.sig} inner_instance : {
  x → Top.sig
}  // Flattened! Not x → Middle.y
```

    ##Trigger Resolution

        When a process waits on an event,
    the trigger variable is resolved through the instance context :

```cpp for (const auto& trigger : instr.wait_triggers) {
  resolved_triggers.push_back({
      .edge_kind = trigger.edge_kind,
      .variable = resolve_symbol(trigger.variable)  // Resolve!
  });
}
```

Both `counter1` and `counter2` waiting on `@(posedge clk)` resolve to waiting on `Top.clk`.

## Limitations

1. **No parameter support**: Parameterized modules require future work
2. **No hierarchical reads**: `x = child.signal` not yet supported (future phase)

## Files

| File                                            | Purpose                           |
| ----------------------------------------------- | --------------------------------- |
| `include/lyra/mir/expression.hpp`               | HierarchicalReferenceExpression   |
| `include/lyra/mir/statement.hpp`                | AssignmentTarget with hier. path  |
| `src/lyra/lowering/ast_to_mir/module.cpp`       | Port connection transformation    |
| `src/lyra/compiler/codegen.cpp`                 | C++ emission for hier. assignment |
| `src/lyra/lowering/mir_to_lir/statement.cpp`    | LIR lowering for hier. store      |
| `include/lyra/interpreter/instance_context.hpp` | InstanceContext + children map    |
| `src/lyra/interpreter/simulation_runner.cpp`    | Elaboration and port bindings     |
| `src/lyra/interpreter/instruction_runner.cpp`   | Hierarchical store execution      |
