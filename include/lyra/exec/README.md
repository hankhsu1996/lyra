# lyra::exec (Reserved)

This namespace will contain LLVM-based execution infrastructure:

- JIT compilation of MIR
- Native code execution
- Integration with runtime scheduler

Relationship to other namespaces:

- `lyra::runtime`: Scheduler and process coordination
- `lyra::exec`: LLVM-based native execution (this namespace)
