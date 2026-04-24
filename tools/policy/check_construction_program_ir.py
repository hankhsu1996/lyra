#!/usr/bin/env python3
"""Check that emitted LLVM IR uses the pooled construction program shape.

Validates C1 invariants:
  - Zero per-instance path globals (inst_path_*)
  - Zero per-instance param payload globals (__lyra_param_payload_*)
  - Zero LyraConstructorAddInstance call sites
  - Exactly one LyraConstructorRunProgram call site

Usage:
  python3 tools/policy/check_construction_program_ir.py <llvm-ir-file>
  python3 tools/policy/check_construction_program_ir.py --lyra <lyra-binary> <project-dir>
"""

import argparse
import re
import subprocess
import sys


def check_ir_content(ir_text: str) -> list[str]:
    violations = []

    per_instance_paths = re.findall(r"@inst_path_\d+", ir_text)
    if per_instance_paths:
        violations.append(
            f"C1: {len(per_instance_paths)} per-instance path globals "
            f"(expected 0): {per_instance_paths[:5]}"
        )

    per_instance_payloads = re.findall(r"@__lyra_param_payload_\d+", ir_text)
    if per_instance_payloads:
        violations.append(
            f"C1: {len(per_instance_payloads)} per-instance param payload "
            f"globals (expected 0): {per_instance_payloads[:5]}"
        )

    add_instance_calls = re.findall(
        r"call\s+void\s+@LyraConstructorAddInstance\b", ir_text
    )
    if add_instance_calls:
        violations.append(
            f"C1: {len(add_instance_calls)} LyraConstructorAddInstance "
            f"call sites (expected 0)"
        )

    run_program_calls = re.findall(
        r"call\s+void\s+@LyraConstructorRunProgram\b", ir_text
    )
    if len(run_program_calls) != 1:
        violations.append(
            f"C1: {len(run_program_calls)} LyraConstructorRunProgram "
            f"call sites (expected 1)"
        )

    return violations


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Check construction program IR shape"
    )
    parser.add_argument(
        "input",
        help="LLVM IR file path, or project directory when using --lyra",
    )
    parser.add_argument(
        "--lyra",
        metavar="BINARY",
        help="Path to lyra binary; input is treated as project directory",
    )
    args = parser.parse_args()

    if args.lyra:
        result = subprocess.run(
            [args.lyra, "-C", args.input, "dump", "llvm"],
            capture_output=True,
            text=True,
        )
        if result.returncode != 0:
            print(f"lyra dump llvm failed:\n{result.stderr}", file=sys.stderr)
            return 1
        ir_text = result.stdout
    else:
        with open(args.input) as f:
            ir_text = f.read()

    violations = check_ir_content(ir_text)

    if violations:
        for v in violations:
            print(f"VIOLATION: {v}", file=sys.stderr)
        return 1

    print("Construction program IR shape: OK")
    return 0


if __name__ == "__main__":
    sys.exit(main())
