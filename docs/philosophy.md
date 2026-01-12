# Philosophy

## North Star

**Minimize iteration time = compile time + runtime + debug time.**

Peak simulation speed is secondary. Anything that significantly increases compile time at the cost of runtime speed is not acceptable by default.

This reflects real pain with tools like Verilator: compile times reaching hours, unreadable output, poor developer feedback.

## Priority Ordering

1. Iteration speed (build + run + debug)
2. Correctness (for supported subset)
3. Debuggability
4. Readable generated artifacts
5. Runtime performance
6. Peak optimization / research features

## Key Architectural Decisions

### C++ Code Generation (not LLVM IR)

Primary backend generates C++, not LLVM IR.

Reasons:

- Faster compile-time iteration
- Simpler debugging (step through generated code)
- Easier integration with existing toolchains
- Avoids LLVM/JIT complexity

LLVM IR remains a possible future backend, but does not drive initial architecture.

### Elaboration Models

Lyra has two execution paths with different elaboration strategies:

**C++ Codegen Path (Runtime Elaboration):**

- Hierarchy is constructed at C++ runtime
- All non-type parameters are constructor arguments
- `generate for/if` becomes constructor logic
- Variable access: `this->u_child_.value` (member traversal)

**Interpreter Path (Compile-time Elaboration via Slang):**

- Slang elaborates hierarchy at compile time
- Each variable gets a unique symbol pointer
- Variable access: flat lookup by symbol (no instance traversal)
- Symbol pointer IS the address

Slang performs legality checks, name resolution, and validation for both paths. For codegen, Slang does not emit a fully elaborated netlist. For the interpreter, Slang's elaborated symbols are used directly.

### MIR/LIR Role

MIR/LIR are module templates, not elaborated instance graphs.

They encode:

- Parameter interfaces
- Behavioral semantics (always/assign)
- Scheduling semantics (comb/ff/NBA)

They do not encode:

- Fully elaborated instance graphs
- Static per-instance specialization

### Readable C++ Output

Explicitly rejects Verilator's tradeoff of speed over readability.

Rules:

- Preserve original signal and parameter names
- One function per always block
- Readable runtime APIs, no macro soup
- Debug/readable mode as default, fast mode opt-in

### Event-Driven Simulation (Default)

Default mode:

- Event-driven / process-driven simulation
- No global flattening
- No global topo sort
- No whole-design static scheduling

Optional fast modes (future):

- Local comb sorting
- Partial specialization
- Hot-path optimization

## Constants and Parameters

- Compile-time constants must be resolved before simulation runtime
- Evaluation can occur in Slang, MIR, or C++ constructor
- Never during simulation event loop

Loss of human semantics (e.g., `2 ** addr_width` becomes `4096`) is acceptable. Execution semantics take priority over preserving design intent.

## Type Parameters

`parameter type` requires C++ templates, causing code specialization. This is acceptable only when required. Default strategy: runtime parameters for values, templates only for type parameters.
