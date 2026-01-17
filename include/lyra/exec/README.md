# lyra::exec (Reserved)

This namespace will contain LLVM-based execution infrastructure:

- JIT compilation of MIR
- Native code execution
- Integration with runtime scheduler

Relationship to other namespaces:

- `lyra::mir::interp`: Interpreter-based execution (semantic reference)
- `lyra::runtime`: Scheduler and process coordination
- `lyra::exec`: LLVM-based native execution (this namespace)

The interpreter and LLVM execution are alternative backends that share
the same runtime infrastructure. Both must produce identical observable
behavior (verified via differential testing).
