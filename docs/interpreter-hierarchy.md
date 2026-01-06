#Interpreter Hierarchical Module Support

##Overview

        LYRA has two execution backends :

    |
    Aspect | C++ Codegen | Interpreter | | -- -- -- -- -|
    -- -- -- -- -- -- -- -- -- -- -- -- -|
    -- -- -- -- -- -- -- -- -- -- -- -- -- -- --| | Path |
    MIR → C++ → compile → run | MIR → LIR → interpret | | Speed |
    Fast(native code) | Slower(interpretation) |
    | Use case | Production simulation | Development,
    debugging | | Hierarchy | Constructor instantiation
        | Elaboration + instance context |

        Both backends support hierarchical modules and produce identical
        simulation results.

        ##Design:

LLVM - Style Instance Context

                SystemVerilog submodules are like
                    C++ member objects.The interpreter follows LLVM /
            Clang's approach to handling member access:

    | Concept | C++ Analogy | Interpreter | | -- -- -- -- -- -- -- -- -|
    -- -- -- -- -- -- -- -- -- -- -| -- -- -- -- -- -- -- -- -- -- -- -| |
    Module definition | Class definition | `lir::Module` (shared) |
    | Module instance | Object instance | `InstanceContext` | | Port binding
    | Constructor reference | Symbol mapping | | Process code | Member function
    | `lir::Process` (shared) | | Execution context | `this` pointer
    | `InstanceContext*` |

    Key insight : **One process definition,
    multiple instance contexts**.Just as a C
    ++ method is compiled once but called with different `this` pointers,
    a process is defined once but executed with different instance contexts.

```
    // C++ analogy:
    class Counter {
  void tick() {
    count_++;
  }  // One function definition
};

Counter c1, c2;
c1.tick();  // tick() with this = &c1
c2.tick();  // tick() with this = &c2
```

## Elaboration

Elaboration is the process of building the instance hierarchy at simulation start. It happens in `SimulationRunner::ElaborateHierarchy()`.

### Steps

1. **Create top instance context** (no port bindings)
2. **Initialize top module variables** in the variable table
3. **Schedule top module processes** with the top instance context
4. **Recursively elaborate submodules** via `ElaborateSubmodules()`

```
ElaborateHierarchy():
  top_instance = InstanceContext("Top", {})
  InitializeModuleVariables(top)
  ScheduleModuleProcesses(top, top_instance)
  ElaborateSubmodules(top, "Top", top_instance)
```

### Submodule Elaboration

For each submodule instance:

1. **Look up module definition** from the module map
2. **Build port bindings** - map child port symbols to parent signal symbols
3. **Resolve through parent bindings** for nested hierarchy (flattening)
4. **Initialize local variables** (non-port variables in the child)
5. **Schedule child processes** with the child's instance context
6. **Recurse** for nested submodules

```
ElaborateSubmodules(parent, parent_path, parent_instance):
  for each submodule in parent.submodules:
    child = LookupModule(submodule.module_type)
    instance_path = parent_path + "." + submodule.instance_name

    // Build port bindings
    bindings = {}
    for each connection in submodule.connections:
      port_symbol = FindPortInChild(child, connection.port_name)
      signal_symbol = parent_instance.Resolve(connection.signal)  // Flatten!
      bindings[port_symbol] = signal_symbol

    child_instance = InstanceContext(instance_path, bindings)
    InitializeModuleVariables(child)
    ScheduleModuleProcesses(child, child_instance)
    ElaborateSubmodules(child, instance_path, child_instance)
```

## Port Handling

### Port Directions

| Direction | C++ Codegen                  | Interpreter        |
| --------- | ---------------------------- | ------------------ |
| `input`   | `const T&` constructor param | Read-only binding  |
| `output`  | `T&` constructor param       | Read-write binding |
| `inout`   | `T&` constructor param       | Read-write binding |

### Port Bindings as Symbol Mapping

Ports are **aliases** to parent signals. When a child process accesses a port variable, the instance context resolves it to the actual parent signal.

```systemverilog
module Counter(input bit clk, output int count);
always_ff @(posedge clk) count <= count + 1;
endmodule

    module Top;
bit clk;
int c1, c2;
Counter counter1(.clk(clk), .count(c1));
Counter counter2(.clk(clk), .count(c2));
endmodule
```

    After elaboration :

```top_instance : {}(no bindings)

                        counter1_instance : {clk → Top.clk, count → Top.c1}

                                            counter2_instance : {
  clk → Top.clk, count → Top.c2
}
```

    ## #Port Connection Expressions

        Currently,
    port connections must be simple identifiers.Expressions in port connections
        are not yet supported :

```systemverilog
            // Supported:
            Counter c(.clk(clk), .count(result));

// Not yet supported:
Counter c(.clk(clk& enable), .count(result [3:0]));
```

    ##Symbol Resolution

        The `InstanceContext::Resolve()` method maps symbols through port
    bindings :

```cpp auto Resolve(SymbolRef symbol) const -> SymbolRef {
  auto it = port_bindings.find(symbol);
  return it != port_bindings.end() ? it->second : symbol;
}
```

    ## #Resolution Flow

        When a process accesses a variable :

    1. Instruction contains the **local symbol **(
        e.g., `count`)2. `resolve_symbol` lambda calls `instance_context
        ->Resolve(count)` 3. If `count` is a port,
    returns the bound signal(e.g., `c1`) 4. If not a port,
    returns the original symbol(local variable)

```Counter.always_ff writes to 'count' : 1. STORE count,
    value 2. resolve_symbol(count) via instance_context 3. If counter1
    : count → c1(from binding) 4. If counter2
    : count → c2(from binding) 5. Write to resolved symbol in variable_table
```

      ## #Nested Hierarchy Flattening

      For deeply nested modules,
      bindings are **flattened at elaboration time ** :

```systemverilog module Inner(input bit x);
// uses x
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

    This is done by resolving through the parent's bindings during elaboration:

```cpp signal_symbol = parent_instance->Resolve(connection.signal);

```

        ##Sensitivity Lists and Triggering

        ## #Event Control Types

        | SystemVerilog | EdgeKind | Trigger Condition | |
        -- -- -- -- -- -- -- -- -- -- -- -- -- -| -- -- -- -- -- -- -- -- -|
        -- -- -- -- -- -- -- -- -| | `@(x)` | `kAnyChange` | x changed |
        | `@(posedge x)` | `kPosedge` | x : 0→1 |
        | `@(negedge x)` | `kNegedge` | x : 1→0 |
        | `@(edge x)` | `kBothEdge` | x : 0→1 or
    1→0 | | `@(posedge a or negedge b)` | Multiple triggers | Either condition |

        ## #Multiple Triggers(Event Lists)

            A process can wait on multiple variables with different edge kinds :

```systemverilog @(posedge clk or negedge rst)

```

    Each trigger is registered separately with the TriggerManager :

```cpp for (const auto& trigger : result.triggers) {
  trigger_manager_.RegisterWaitingProcess(
      process, instance, trigger.variable, trigger.edge_kind, block_index,
      instruction_index);
}
```

    ## #TriggerManager Design

        The TriggerManager tracks waiting processes using two maps :

```cpp
            // Which (process, instance) pairs are waiting on each variable
            WaitMap
    : variable → set
      of(process, instance)

      // Info for each waiting (process, instance, variable) tuple
      WaitSet : (process, instance, variable) → {
  edge_kind, resume_position
}
```

    ** Key design** : The `WaitSet` key includes `variable` because a process
                          can wait on multiple variables with different edge
                              kinds.Without the variable in the key,
    the edge kind would be overwritten.

    ## #Trigger Resolution Through Instance Context

        When a process waits on an event,
    the trigger variable is resolved through the instance context :

```cpp
    // In instruction_runner.cpp, kWaitEvent case:
    for (const auto& trigger : instr.wait_triggers) {
  resolved_triggers.push_back({
      .edge_kind = trigger.edge_kind,
      .variable = resolve_symbol(trigger.variable)  // Resolve!
  });
}
```

    This means both `counter1` and `counter2` waiting on `
    @(posedge clk)` will resolve to waiting on `Top.clk`,
    so both wake when `Top.clk` rises.

    ## #Trigger Checking

    When a variable changes :

    1. Read old and new values from variable table 2. For
    each(process, instance) waiting on this variable:

- Look up the edge kind for this (process, instance, variable) tuple
- Check if the change matches the edge kind
- If yes, schedule the process with its instance context

3. Remove triggered entries from wait maps

```cpp
if (ShouldTrigger(old_value, new_value, edge_kind)) {
  events_to_trigger.push(
      {.process = process,
       .instance = instance,  // Preserved!
       .block_index = ...,
       .instruction_index = ...});
}
```

    ##Process Execution Across Hierarchy

    ## #ScheduledEvent Structure

        Each scheduled event carries its instance context :

```cpp struct ScheduledEvent {
  ProcessPtr process;
  std::shared_ptr<InstanceContext> instance;  // Like 'this' pointer
  size_t block_index;
  size_t instruction_index;
};
```

### Execution Flow

```
ExecuteOneEvent():
  event = active_queue.pop()
  process = event.process
  instance = event.instance  // The 'this' pointer

  result = RunProcess(process, ..., instance.get())

  // Handle result
  switch result.kind:
    kDelay:
      delay_queue.push({process, instance, ...})  // Preserve instance!
    kWaitEvent:
      for trigger in result.triggers:
        trigger_manager.Register(process, instance, ...)
    kComplete:
      // Done
```

### Process Sharing

All instances of the same module type share the same `lir::Process` objects:

```
Module "Counter":
  Process "always_ff_0"  (one definition)

Instances:
  counter1_instance → uses always_ff_0 with counter1 bindings
  counter2_instance → uses always_ff_0 with counter2 bindings
```

This is memory-efficient: processes are not cloned per instance.

## Variable Initialization

### InitializeModuleVariables

Registers variables in the global variable table with default values:

```cpp
void InitializeModuleVariables(const lir::Module& module) {
  for (const auto& variable : module.variables) {
    simulation_context_.variable_table.InitializeVariable(variable);
  }
}
```

### What Gets Initialized

| Variable Type          | Initialized? | Notes                          |
| ---------------------- | ------------ | ------------------------------ |
| Top module variables   | Yes          | In `ElaborateHierarchy()`      |
| Child module variables | Yes          | In `ElaborateSubmodules()`     |
| Port variables         | No           | Ports are aliases, not storage |

### Variable Table

The variable table is **flat** - all variables from all modules are stored together:

```
Variable Table:
  Top.clk → 0
  Top.c1 → 0
  Top.c2 → 0
  Counter.internal → 0  (if Counter has local variables)
```

Port accesses are redirected via instance context resolution.

## Comparison with C++ Codegen

| Aspect                | C++ Codegen          | Interpreter                |
| --------------------- | -------------------- | -------------------------- |
| **Port binding**      | C++ reference member | InstanceContext map        |
| **Process sharing**   | One method per type  | One Process per type       |
| **Context passing**   | Implicit `this`      | Explicit InstanceContext\* |
| **Memory layout**     | Per-instance members | Shared + context           |
| **Variable access**   | Direct member access | Symbol table + resolution  |
| **Hierarchy storage** | Object composition   | Module map + instance tree |

Both achieve identical semantics - the codegen uses C++ references for zero-copy port access, the interpreter uses symbol table indirection.

## Limitations

1. **Port expressions not supported**: Port connections must be simple identifiers
2. **No parameter support**: Parameterized modules require future work
3. **Codegen test API**: Test framework doesn't support hierarchy in codegen path (use CLI)

## Files

| File                                            | Purpose                                 |
| ----------------------------------------------- | --------------------------------------- |
| `include/lyra/interpreter/instance_context.hpp` | InstanceContext struct                  |
| `include/lyra/interpreter/trigger_manager.hpp`  | TriggerManager, ProcessInstanceVarKey   |
| `src/lyra/interpreter/simulation_runner.cpp`    | ElaborateHierarchy, ElaborateSubmodules |
| `src/lyra/interpreter/instruction_runner.cpp`   | Symbol resolution via resolve_symbol    |
| `src/lyra/lowering/ast_to_mir/ast_to_mir.cpp`   | CollectModulesRecursive                 |
