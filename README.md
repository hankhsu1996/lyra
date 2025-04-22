# **Volans: Rethinking SystemVerilog Simulation**

**Volans** is a clean, structured, and extensible SystemVerilog compiler and simulator.
It features a modern multi-stage compilation pipeline, enabling accurate semantic analysis, flexible intermediate representations, and native execution through interpretation or LLVM.


## 🛠️ Build Instructions

Build the entire project:
```bash
bazel build //...
```

Generate compile commands for IDE integration:
```bash
bazel run @hedron_compile_commands//:refresh_all
```


## 📦 Project Structure

- `frontend/`: Slang-based SystemVerilog parser
- `mir/`: Middle-level IR preserving high-level structure
- `lir/`: Low-level IR with SSA-style instructions and interpreter
- `lowering/`: Transformations from AST to MIR to LIR
- `core/`: Runtime and execution context
- `codegen/`: LLVM code generation (planned)


## 🔍 Compilation Pipeline

```
SystemVerilog → AST → MIR → LIR → [ LLVM IR | Interpreter ] → Results
```

- **AST**: Generated using [Slang](https://github.com/MikePopoloski/slang)
- **MIR**: High-level, structure-preserving intermediate representation
- **LIR**: Linear SSA-style IR for simulation and codegen
- **Backend**: Interpreter (available) or LLVM native compilation (planned)


## ✅ Current Features

- SystemVerilog `module` support with variable declarations
- `initial` blocks and sequential execution
- Arithmetic operations (`+`) and assignments
- Signal management and runtime context
- LIR-based interpreter for simulation


## 🚧 Roadmap

- Event-driven scheduling system
- `always_ff` support with clock/event semantics
- Native binary generation via LLVM
- Broader SystemVerilog language coverage

## 💬 Get Involved

Got feedback or ideas? We're building Volans to make SystemVerilog simulation **cleaner**, **clearer**, and **more scalable** — contributions are welcome!
