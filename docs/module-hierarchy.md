# Module Hierarchy

## Overview

Lyra has two execution backends:

| Aspect    | C++ Codegen               | Interpreter             |
| --------- | ------------------------- | ----------------------- |
| Path      | MIR → C++ → compile → run | MIR → LIR → interpret   |
| Speed     | Fast (native code)        | Slower (interpretation) |
| Use case  | Production simulation     | Development, debugging  |
| Hierarchy | Constructor instantiation | Elaboration + context   |

Both backends support hierarchical modules and produce identical simulation results.

## Data Flow

```
                        +---> C++ Codegen (PortDriverStatement emitted directly)
                        |
SV ---> AST ---> MIR ---+
                        |
                        +---> LIR ---> Interpreter (InstanceContext + resolution)
```

Key design: **Transform once at AST→MIR, simplify downstream stages**.

## Input Port Connections

All input port connections are transformed to implicit processes:

```systemverilog
// Both cases use the same model
Child c(.a(x));       // Simple identifier
Child c(.a(x + y));   // Expression
```

### Transformation in AST→MIR

When AST→MIR encounters an input port connection, it:

1. Creates a `PortDriverStatement` representing `submodule.port = expression`
2. Creates an implicit `always_comb`-like process containing the driver
3. Stores the signal expression in the port connection (for interpreter bindings)

**MIR Result:**

```
module Top
  Adder add(.a(x), .out(result))  // Signal stored for binding

  process _port_driver_0          // Process for codegen
    while 1
      add.a = x
      @(x)
```

### Backend-Specific Handling

| Stage   | Codegen                        | Interpreter                 |
| ------- | ------------------------------ | --------------------------- |
| MIR     | Uses PortDriverStatement       | Uses port connection signal |
| Runtime | Process drives child's storage | Binding resolves to parent  |

The MIR stores both representations, allowing each backend to use what it needs.

## MIR Representation

### New Statement: PortDriverStatement

```cpp
class PortDriverStatement : public Statement {
  std::string submodule_instance;  // e.g., "child"
  std::string port_name;           // e.g., "a"
  std::unique_ptr<Expression> value;
};
```

This statement represents driving a submodule's input port:

```
add.a = x + y;
```

### Process Creation Pattern

Port driver processes follow the `always_comb` pattern:

```cpp
// while (true) { driver_stmt; @(sensitivity_list); }
auto CreatePortDriverProcess(PortDriverStatement, counter) -> Process {
  auto variables = CollectSensitivityList(*driver_stmt->value);
  // Create triggers from variables
  // Build: while(true) { driver; wait_event(triggers); }
}
```

## C++ Codegen

### Input Port Model

Input ports are public variables (not references):

```cpp
class Adder : public lyra::sdk::Module {
 public:
  Int a;  // Public for parent to drive
  // ...
};
```

### Port Driver Process Emission

The `PortDriverStatement` is emitted as:

```cpp
auto _port_driver_0() -> lyra::sdk::Task {
  while (Bit<1>{1}) {
    add_.a = x + y;  // Direct assignment
    co_await lyra::sdk::AnyChange(&y, &x);
  }
  co_return;
}
```

### Output Port Model

Output ports remain as references (parent owns storage):

```cpp
class Adder : public lyra::sdk::Module {
 public:
  Adder(Int& out) : out_(out) {}  // Reference to parent signal
  Int& out_;
};
```

## Interpreter Input Port Model

The interpreter supports input port connections via **bindings** rather than driver processes:

| Approach    | Codegen                    | Interpreter               |
| ----------- | -------------------------- | ------------------------- |
| Model       | Child has storage          | Child aliases parent      |
| Input write | Parent drives via process  | N/A (binding resolves)    |
| Input read  | Child reads own storage    | Resolves to parent signal |
| Sensitivity | Process waits on variables | Wait resolves to parent   |

### How It Works

1. **Elaboration**: Creates binding `child.input_port → parent.signal`
2. **Read**: Child reads `clk` → resolves to `Top.clk` → reads parent's value
3. **Wait**: Child waits on `@(posedge clk)` → resolves → waits on `Top.clk`

The `PortDriverStatement` is skipped during MIR→LIR lowering since bindings handle input ports.

### Limitation: Port Expressions

Simple identifier connections work:

```systemverilog
Child c(.a(x));  // Works: binding a → x
```

Expressions do NOT work in the interpreter:

```systemverilog
Child c(.a(x + y));  // Interpreter: unsupported (binding can't evaluate expressions)
                     // Codegen: works via PortDriverStatement
```

For expression support, the interpreter would need to evaluate expressions, which
is what the PortDriverStatement process does in codegen.

## Port Directions

| Direction | C++ Codegen     | Interpreter        |
| --------- | --------------- | ------------------ |
| `input`   | Public variable | Read-only binding  |
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

### Nested Hierarchy Flattening

Bindings are flattened at elaboration time:

```systemverilog
module Inner(input bit x);  // uses x
endmodule

module Middle(input bit y);
  Inner i(.x(y));
endmodule

module Top;
  bit sig;
  Middle m(.y(sig));
endmodule
```

Bindings:

```
middle_instance: { y → Top.sig }
inner_instance:  { x → Top.sig }  // Flattened! Not x → Middle.y
```

## Trigger Resolution

When a process waits on an event, the trigger variable is resolved through the instance context:

```cpp
for (const auto& trigger : instr.wait_triggers) {
  resolved_triggers.push_back({
      .edge_kind = trigger.edge_kind,
      .variable = resolve_symbol(trigger.variable)  // Resolve!
  });
}
```

Both `counter1` and `counter2` waiting on `@(posedge clk)` resolve to waiting on `Top.clk`.

## Limitations

1. **Interpreter port expressions**: Expressions like `.a(x + y)` not supported (codegen only)
2. **No parameter support**: Parameterized modules require future work

## Files

| File                                            | Purpose                         |
| ----------------------------------------------- | ------------------------------- |
| `include/lyra/mir/statement.hpp`                | PortDriverStatement definition  |
| `include/lyra/mir/expression.hpp`               | PortDriverExpression definition |
| `src/lyra/lowering/ast_to_mir/module.cpp`       | Port expression transformation  |
| `src/lyra/compiler/codegen.cpp`                 | C++ emission for port drivers   |
| `src/lyra/lowering/mir_to_lir/statement.cpp`    | LIR lowering (skips PortDriver) |
| `include/lyra/interpreter/instance_context.hpp` | InstanceContext struct          |
| `src/lyra/interpreter/simulation_runner.cpp`    | Elaboration and port bindings   |
