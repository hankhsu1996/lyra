#!/usr/bin/env python3
"""Central architecture / boundary policy enforcement.

Single entry point. Each rule is independent; rules are renumbered when
removed so the IDs stay contiguous.

Rules:

  A001  HIR sources may not reference MIR/runtime terms.
        Scope: src/lyra/hir/**, include/lyra/hir/**.
        Forbidden tokens: Runtime, Engine.

  A002  HIR may not include MIR headers.
        Scope: src/lyra/hir/**, include/lyra/hir/**.
        Forbidden include: #include "lyra/mir/...".

  A003  HIR and MIR may not include driver or backend headers.
        Scope: src/lyra/{hir,mir}/**, include/lyra/{hir,mir}/**.

  A004  Project APIs must use diag::Result<T>, not raw
        std::expected<..., diag::Diagnostic>.
        Scope: every .cpp/.hpp under src/lyra and include/lyra except
               include/lyra/diag/diagnostic.hpp.

  A005  No state class may hold a facts pointer/member. Facts are read-only
        inputs and must be passed alongside state, not embedded in it.
        Scope: every .hpp under src/lyra/lowering/**.

  A006  No `support::Unsupported(...)` helper. Unsupported features go
        through diag::Unsupported(...).

  A007  std::unexpected(...) is allowed only for direct propagation. New
        diagnostics must be built via diag::Unsupported / diag::Error /
        diag::HostError.

  A008  A function whose name starts with `Lower` and returns an ID
        (`...Id`) must include Append|Add|Intern|Declare|Insert in its
        name. Pure translators must return data, not IDs.

  A009  A function whose name starts with `Lower` and returns
        `diag::Result<void>` must include Into|Append|Populate in its
        name (the destination must be visible from the name).

  A010  Function name matching `Lower*Data` must return data, not an Id,
        and must not take a mutable `*State&` parameter. Pure translators
        do not own the arena.

  A011  No `Lowered*` staging structs in the lowering layer. Semantic
        lowering must return the real IR object (`mir::Stmt`,
        `mir::Expr`, `hir::Stmt`, ...). Inventing a parallel `Lowered*`
        struct just to ship pre-append data is a fake IR.

  A012  Append* function bodies in the lowering layer must be thin
        storage. They may not call `Lower*(`, inspect `.kind`, downcast
        `.as<`, emit `diag::Unsupported`/`Error`/`HostError`, or throw
        `support::InternalError`. Pure semantic lowering happens before
        Append; Append only stores already-lowered objects.
        Scope: src/lyra/lowering/**, include/lyra/lowering/**.
        Matches inline (`auto AppendXxx(...) -> Type {` /
        `void AppendXxx(...) {`) and out-of-line
        (`auto Class::AppendXxx(...) -> Type {` /
        `void Class::AppendXxx(...) {`) definition shapes.

When a rule fires, the printed message includes a fixed reminder that the
fix is to change the ownership boundary, NOT to rename the function.

Usage:
  python3 tools/policy/check_architecture.py
"""

import re
import sys
from pathlib import Path


# --- Rule A001 -----------------------------------------------------------
HIR_FORBIDDEN_TERMS = ["Runtime", "Engine"]
HIR_FORBIDDEN_TERM_PATTERN = re.compile(
    r"\b(" + "|".join(HIR_FORBIDDEN_TERMS) + r")\b"
)

# --- Rule A002 -----------------------------------------------------------
HIR_FORBIDDEN_INCLUDE_PATTERN = re.compile(r'#\s*include\s*"lyra/mir/')

# --- Rule A003 -----------------------------------------------------------
IR_FORBIDDEN_INCLUDE_PATTERN = re.compile(
    r'#\s*include\s*"lyra/(driver|backend)/'
)

# --- Rule A004 -----------------------------------------------------------
RAW_EXPECTED_DIAGNOSTIC_PATTERN = re.compile(
    r"std::expected\s*<\s*[^<>]*?,\s*diag::Diagnostic\s*>",
    re.DOTALL,
)
RAW_EXPECTED_ALLOWED = frozenset({
    "include/lyra/diag/diagnostic.hpp",
})

# --- Rule A005 -----------------------------------------------------------
STATE_HOLDS_FACTS_PATTERN = re.compile(
    r"\b(?:const\s+)?[A-Za-z_][A-Za-z0-9_]*Facts\s*[*&]?\s*[A-Za-z_][A-Za-z0-9_]*_;"
)

# --- Rule A006 -----------------------------------------------------------
SUPPORT_UNSUPPORTED_PATTERN = re.compile(r"\bsupport::Unsupported\b")

# --- Rule A007 -----------------------------------------------------------
RAW_UNEXPECTED_PATTERN = re.compile(
    r"std::unexpected\s*\(\s*(?:lyra::)?diag::Diagnostic\b"
)

# --- Rule A011 -----------------------------------------------------------
LOWERED_STAGING_STRUCT_PATTERN = re.compile(
    r"\b(?:struct|class)\s+(Lowered[A-Z]\w*)\b"
)

# --- Rule A012 -----------------------------------------------------------
# Matches Append* function definition shapes used in this repo, both
# inline and out-of-line:
#   auto AppendXxx(...) -> Type {
#   void AppendXxx(...) {
#   auto Class::AppendXxx(...) -> Type {
#   void Class::AppendXxx(...) {
APPEND_FN_DEF_PATTERN = re.compile(
    r"\b(?:auto|void)\s+(?:\w+::)?(Append\w+)\s*\(([^;{]*?)\)"
    r"\s*(?:->[^;{]*?)?\{",
    re.DOTALL,
)
FORBIDDEN_IN_APPEND = (
    (re.compile(r"\bLower\w+\s*\("),
     "calls Lower*; lower before Append"),
    (re.compile(r"\.kind\b"),
     "inspects .kind; AST/HIR inspection belongs in Lower*"),
    (re.compile(r"\.as<"),
     "downcasts via .as<>; Append must store already-lowered objects"),
    (re.compile(r"\bdiag::(?:Unsupported|Error|HostError)\b"),
     "emits diag::Unsupported|Error|HostError; Append must not error"),
    (re.compile(r"\bthrow\s+support::InternalError\b"),
     "throws support::InternalError; Append must not enforce semantic "
     "invariants -- check those at the lowering boundary"),
)

# --- Rules A008 / A009 / A010 -------------------------------------------
LOWER_FN_PATTERN = re.compile(
    r"\bauto\s+(Lower\w*)\s*\(([^;{]*?)\)\s*->\s*([^;{]+?)\s*[;{]",
    re.DOTALL,
)
APPENDER_VERBS = frozenset({"Append", "Add", "Intern", "Declare", "Insert"})
INTO_VERBS = frozenset({"Into", "Append", "Populate"})
CAMEL_WORD_RE = re.compile(r"[A-Z][a-z0-9]*")


def _camel_words(name: str) -> list[str]:
    return CAMEL_WORD_RE.findall(name)


def _has_appender_verb(name: str) -> bool:
    return any(w in APPENDER_VERBS for w in _camel_words(name))


def _has_into_verb(name: str) -> bool:
    return any(w in INTO_VERBS for w in _camel_words(name))


RETURN_HAS_ID_RE = re.compile(r"\b\w+Id\b")
RESULT_VOID_RE = re.compile(r"\bdiag::Result\s*<\s*void\s*>")
LOWER_DATA_RE = re.compile(r"^Lower\w*Data$")
MUTABLE_STATE_PARAM_RE = re.compile(
    r"\b[A-Za-z_][A-Za-z0-9_]*State\s*&\s*\w+"
)

SKIP_PREFIXES = ("archived/", "external/", "bazel-")
EXTENSIONS = frozenset({".cpp", ".hpp", ".cc", ".cxx", ".h", ".hh"})


# Architecture-violation hint printed under every rule. It tells the helper
# the fix is at the ownership boundary, not at the function name or IR shape.
VIOLATION_HINT = """
ARCHITECTURE POLICY VIOLATION

Do not fix this by renaming, carve-outs, or IR shape changes.
Fix the ownership boundary.

This usually means:
- semantic lowering must return data / full objects (no IDs, no
  arena mutation);
- append/add/insert functions must only store already-lowered objects
  and return IDs;
- raw std::expected must be wrapped as diag::Result;
- diagnostics must be built with diag::Unsupported / diag::Error /
  diag::HostError, not std::unexpected of a raw Diagnostic;
- arena allocation belongs at the owner boundary, not inside semantic
  lowering.

Stop and ask the user for architecture direction if the fix is not
obvious.
"""


def strip_comment(line: str) -> str:
    idx = line.find("//")
    return line[:idx] if idx != -1 else line


def iter_files(repo_root: Path, *roots: str, hpp_only: bool = False):
    for root in roots:
        base = repo_root / root
        if not base.exists():
            continue
        exts = ("*.hpp",) if hpp_only else ("*.cpp", "*.hpp")
        for ext in exts:
            for path in base.rglob(ext):
                rel = path.relative_to(repo_root).as_posix()
                if any(rel.startswith(p) for p in SKIP_PREFIXES):
                    continue
                yield path, rel


def iter_layer_files(repo_root: Path, layer: str):
    yield from iter_files(repo_root, f"src/lyra/{layer}", f"include/lyra/{layer}")


def iter_lyra_files(repo_root: Path):
    yield from iter_files(repo_root, "src/lyra", "include/lyra")


# --- Checks --------------------------------------------------------------

def check_a001(repo_root: Path) -> list[str]:
    errors = []
    for path, rel in iter_layer_files(repo_root, "hir"):
        for lineno, line in enumerate(path.read_text().splitlines(), 1):
            code = strip_comment(line)
            m = HIR_FORBIDDEN_TERM_PATTERN.search(code)
            if m:
                errors.append(
                    f"  {rel}:{lineno}: A001 forbidden term '{m.group(0)}'"
                )
    return errors


def check_a002(repo_root: Path) -> list[str]:
    errors = []
    for path, rel in iter_layer_files(repo_root, "hir"):
        for lineno, line in enumerate(path.read_text().splitlines(), 1):
            if HIR_FORBIDDEN_INCLUDE_PATTERN.search(line):
                errors.append(f"  {rel}:{lineno}: A002 {line.strip()}")
    return errors


def check_a003(repo_root: Path) -> list[str]:
    errors = []
    for layer in ("hir", "mir"):
        for path, rel in iter_layer_files(repo_root, layer):
            for lineno, line in enumerate(path.read_text().splitlines(), 1):
                if IR_FORBIDDEN_INCLUDE_PATTERN.search(line):
                    errors.append(f"  {rel}:{lineno}: A003 {line.strip()}")
    return errors


def check_a004(repo_root: Path) -> list[str]:
    errors = []
    for path, rel in iter_lyra_files(repo_root):
        if rel in RAW_EXPECTED_ALLOWED:
            continue
        text = path.read_text()
        for m in RAW_EXPECTED_DIAGNOSTIC_PATTERN.finditer(text):
            lineno = text.count("\n", 0, m.start()) + 1
            snippet = m.group(0).replace("\n", " ").strip()
            errors.append(f"  {rel}:{lineno}: A004 {snippet}")
    return errors


def check_a005(repo_root: Path) -> list[str]:
    errors = []
    for path, rel in iter_files(
            repo_root, "src/lyra/lowering", "include/lyra/lowering",
            hpp_only=True):
        text = path.read_text()
        for m in re.finditer(
                r"class\s+([A-Za-z_][A-Za-z0-9_]*State)\s*[^;{]*\{(.*?)\};",
                text, re.DOTALL):
            class_name = m.group(1)
            body = m.group(2)
            for f in STATE_HOLDS_FACTS_PATTERN.finditer(body):
                lineno = text.count(
                    "\n", 0, m.start() + m.group(0).find(f.group(0))) + 1
                errors.append(
                    f"  {rel}:{lineno}: A005 state '{class_name}' holds "
                    f"facts member: '{f.group(0)}'"
                )
    return errors


def check_a006(repo_root: Path) -> list[str]:
    errors = []
    for path, rel in iter_lyra_files(repo_root):
        for lineno, line in enumerate(path.read_text().splitlines(), 1):
            code = strip_comment(line)
            if SUPPORT_UNSUPPORTED_PATTERN.search(code):
                errors.append(
                    f"  {rel}:{lineno}: A006 use diag::Unsupported(...) instead"
                )
    return errors


def check_a007(repo_root: Path) -> list[str]:
    errors = []
    for path, rel in iter_lyra_files(repo_root):
        if rel in RAW_EXPECTED_ALLOWED:
            continue
        text = path.read_text()
        for m in RAW_UNEXPECTED_PATTERN.finditer(text):
            lineno = text.count("\n", 0, m.start()) + 1
            tail = text[m.start():m.start() + 80].split("\n", 1)[0]
            errors.append(f"  {rel}:{lineno}: A007 {tail.strip()}")
    return errors


def _iter_lower_fns(repo_root: Path):
    seen: set[tuple[str, str]] = set()
    for path, rel in iter_lyra_files(repo_root):
        text = path.read_text()
        for m in LOWER_FN_PATTERN.finditer(text):
            name = m.group(1)
            args = m.group(2)
            ret = m.group(3).strip()
            key = (name, args)
            if key in seen:
                continue
            seen.add(key)
            lineno = text.count("\n", 0, m.start()) + 1
            yield rel, lineno, name, args, ret


def check_a008(repo_root: Path) -> list[str]:
    errors = []
    for rel, lineno, name, _, ret in _iter_lower_fns(repo_root):
        if RETURN_HAS_ID_RE.search(ret) and not _has_appender_verb(name):
            errors.append(
                f"  {rel}:{lineno}: A008 '{name}' returns Id ({ret}); "
                "fix the lowering/storage boundary -- pure translators must "
                "return data, not IDs"
            )
    return errors


def check_a009(repo_root: Path) -> list[str]:
    errors = []
    for rel, lineno, name, _, ret in _iter_lower_fns(repo_root):
        if RESULT_VOID_RE.search(ret) and not _has_into_verb(name):
            errors.append(
                f"  {rel}:{lineno}: A009 '{name}' returns Result<void>; "
                "either rename with Into/Append/Populate to make the "
                "destination visible, or change the boundary so the function "
                "returns the produced object"
            )
    return errors


def check_a011(repo_root: Path) -> list[str]:
    errors = []
    for path, rel in iter_files(
            repo_root, "src/lyra/lowering", "include/lyra/lowering"):
        text = path.read_text()
        for m in LOWERED_STAGING_STRUCT_PATTERN.finditer(text):
            lineno = text.count("\n", 0, m.start()) + 1
            errors.append(
                f"  {rel}:{lineno}: A011 staging struct '{m.group(1)}'; "
                "return the real IR object (mir::Stmt / mir::Expr / "
                "hir::Stmt / ...) and do append at the owner boundary"
            )
    return errors


def _balanced_close(text: str, open_idx: int) -> int:
    depth = 1
    i = open_idx + 1
    while i < len(text) and depth > 0:
        c = text[i]
        if c == "{":
            depth += 1
        elif c == "}":
            depth -= 1
        i += 1
    return i


def check_a012(repo_root: Path) -> list[str]:
    errors = []
    for path, rel in iter_files(
            repo_root, "src/lyra/lowering", "include/lyra/lowering"):
        text = path.read_text()
        for m in APPEND_FN_DEF_PATTERN.finditer(text):
            name = m.group(1)
            brace_idx = m.end() - 1
            body_end = _balanced_close(text, brace_idx)
            body = text[brace_idx + 1:body_end - 1]
            for pat, msg in FORBIDDEN_IN_APPEND:
                bm = pat.search(body)
                if bm:
                    abs_offset = brace_idx + 1 + bm.start()
                    lineno = text.count("\n", 0, abs_offset) + 1
                    errors.append(
                        f"  {rel}:{lineno}: A012 '{name}' {msg}"
                    )
    return errors


def check_a010(repo_root: Path) -> list[str]:
    errors = []
    for rel, lineno, name, args, ret in _iter_lower_fns(repo_root):
        if not LOWER_DATA_RE.match(name):
            continue
        if RETURN_HAS_ID_RE.search(ret):
            errors.append(
                f"  {rel}:{lineno}: A010 '{name}' is a *Data translator but "
                f"returns an Id ({ret}); pure translators return data"
            )
        for sm in MUTABLE_STATE_PARAM_RE.finditer(args):
            start = sm.start()
            preceding = args[max(0, start - 8):start]
            if "const " in preceding:
                continue
            errors.append(
                f"  {rel}:{lineno}: A010 '{name}' takes mutable State& "
                f"({sm.group(0).strip()}); pure translators do not own arenas"
            )
    return errors


# --- Self-tests ----------------------------------------------------------

def run_self_tests() -> bool:
    def expect(cond, msg):
        if not cond:
            print(f"SELF-TEST FAILED: {msg}")
            return False
        return True

    ok = True
    # A001
    ok &= expect(HIR_FORBIDDEN_TERM_PATTERN.search("class Runtime {"),
                 "A001 Runtime")
    ok &= expect(HIR_FORBIDDEN_TERM_PATTERN.search("Engine{}"), "A001 Engine")
    ok &= expect(not HIR_FORBIDDEN_TERM_PATTERN.search("class Slot {};"),
                 "A001 false-pos")

    # A002 / A003
    ok &= expect(
        HIR_FORBIDDEN_INCLUDE_PATTERN.search('#include "lyra/mir/x.hpp"'),
        "A002")
    ok &= expect(
        IR_FORBIDDEN_INCLUDE_PATTERN.search('#include "lyra/driver/x.hpp"'),
        "A003 driver")
    ok &= expect(
        IR_FORBIDDEN_INCLUDE_PATTERN.search('#include "lyra/backend/x.hpp"'),
        "A003 backend")
    ok &= expect(
        not IR_FORBIDDEN_INCLUDE_PATTERN.search(
            '#include "lyra/support/x.hpp"'),
        "A003 false-pos")

    # A004
    ok &= expect(
        RAW_EXPECTED_DIAGNOSTIC_PATTERN.search(
            "auto f() -> std::expected<int, diag::Diagnostic>;"),
        "A004 single-line")
    ok &= expect(
        RAW_EXPECTED_DIAGNOSTIC_PATTERN.search(
            "std::expected<\n  int,\n  diag::Diagnostic\n>"),
        "A004 multi-line")
    ok &= expect(
        not RAW_EXPECTED_DIAGNOSTIC_PATTERN.search("diag::Result<int>"),
        "A004 wrapper alias")
    ok &= expect(
        not RAW_EXPECTED_DIAGNOSTIC_PATTERN.search(
            "std::expected<int, std::string>"),
        "A004 unrelated expected")

    # A005
    ok &= expect(
        STATE_HOLDS_FACTS_PATTERN.search("const UnitLoweringFacts* facts_;"),
        "A005 pointer member")
    ok &= expect(
        STATE_HOLDS_FACTS_PATTERN.search("ProcessLoweringFacts& facts_;"),
        "A005 ref member")
    ok &= expect(not STATE_HOLDS_FACTS_PATTERN.search("std::string name_;"),
                 "A005 false-pos")

    # A006
    ok &= expect(
        SUPPORT_UNSUPPORTED_PATTERN.search("support::Unsupported(\"x\");"),
        "A006 helper")
    ok &= expect(not SUPPORT_UNSUPPORTED_PATTERN.search("diag::Unsupported(...)"),
                 "A006 false-pos")

    # A007
    ok &= expect(
        RAW_UNEXPECTED_PATTERN.search(
            "return std::unexpected(diag::Diagnostic::Error(...));"),
        "A007 raw construction")
    ok &= expect(
        not RAW_UNEXPECTED_PATTERN.search(
            "return std::unexpected(std::move(r.error()));"),
        "A007 propagation OK")
    ok &= expect(
        not RAW_UNEXPECTED_PATTERN.search(
            "return std::unexpected(std::format(\"...\"));"),
        "A007 string-typed expected OK")

    ok &= expect(
        strip_comment("auto x = 1;  // Runtime here") == "auto x = 1;  ",
        "strip_comment")

    # A008 / A009 / A010 -- regex sanity
    src = (
        "auto LowerExpression(int x) -> diag::Result<hir::ExprId>;\n"
        "auto LowerScopeInto(int x) -> diag::Result<void>;\n"
        "auto AppendExpr(int x) -> hir::ExprId;\n"
        "auto LowerStmtData(int x, const State& s) -> mir::StmtData;\n"
        "auto LowerExprData(int x, MutableState& s)"
        " -> diag::Result<hir::ExprId>;\n"
    )
    matches = {m.group(1): (m.group(2), m.group(3))
               for m in LOWER_FN_PATTERN.finditer(src)}
    ok &= expect("LowerExpression" in matches,
                 "A008 parser saw LowerExpression")
    ok &= expect("LowerScopeInto" in matches,
                 "A009 parser saw LowerScopeInto")
    ok &= expect("AppendExpr" not in matches,
                 "parser only matches Lower*")
    ok &= expect("LowerStmtData" in matches,
                 "A010 parser saw LowerStmtData")

    args, ret = matches["LowerExpression"]
    ok &= expect(RETURN_HAS_ID_RE.search(ret) is not None,
                 "A008 detects Id return")
    ok &= expect(not _has_appender_verb("LowerExpression"),
                 "A008 LowerExpression has no append verb")
    ok &= expect(_has_appender_verb("AppendExpression"),
                 "A008 AppendExpression has append verb")
    ok &= expect(_has_appender_verb("LowerAndAddType"),
                 "A008 LowerAndAddType picked up Add")

    args, ret = matches["LowerScopeInto"]
    ok &= expect(RESULT_VOID_RE.search(ret) is not None,
                 "A009 detects Result<void>")
    ok &= expect(_has_into_verb("LowerScopeInto"),
                 "A009 LowerScopeInto has Into")
    ok &= expect(not _has_into_verb("LowerScope"),
                 "A009 LowerScope lacks Into")

    args, ret = matches["LowerExprData"]
    ok &= expect(LOWER_DATA_RE.match("LowerExprData") is not None,
                 "A010 LowerExprData matches Data shape")
    ok &= expect(MUTABLE_STATE_PARAM_RE.search(args) is not None,
                 "A010 detects MutableState&")

    # A011
    ok &= expect(
        LOWERED_STAGING_STRUCT_PATTERN.search("struct LoweredIfGenerate {"),
        "A011 detects struct LoweredIfGenerate")
    ok &= expect(
        LOWERED_STAGING_STRUCT_PATTERN.search("class LoweredCaseItem {"),
        "A011 detects class LoweredCaseItem")
    ok &= expect(
        not LOWERED_STAGING_STRUCT_PATTERN.search("struct LoweringContext {"),
        "A011 false-pos: Lowering* != Lowered*")
    ok &= expect(
        not LOWERED_STAGING_STRUCT_PATTERN.search("// Lowered above"),
        "A011 false-pos: bare word in comment")

    # A012 -- parser shapes covered today
    auto_form = (
        "auto AppendStmt(State& s, const slang::ast::Stmt& src)\n"
        "    -> mir::StmtId {\n"
        "  auto data = LowerStmtData(s, src);\n"
        "  return s.AppendStmt(std::move(data));\n"
        "}\n"
    )
    fm = APPEND_FN_DEF_PATTERN.search(auto_form)
    ok &= expect(fm is not None and fm.group(1) == "AppendStmt",
                 "A012 parser sees `auto Append*` definition")

    void_form = (
        "void AppendRootStmt(mir::StmtId id) {\n"
        "  body_.root_stmts.push_back(id);\n"
        "}\n"
    )
    vm = APPEND_FN_DEF_PATTERN.search(void_form)
    ok &= expect(vm is not None and vm.group(1) == "AppendRootStmt",
                 "A012 parser sees `void Append*` definition")

    # A012 -- forbidden patterns
    ok &= expect(
        FORBIDDEN_IN_APPEND[0][0].search("auto data = LowerStmtData(...)") is
        not None,
        "A012 detects Lower* call")
    ok &= expect(
        FORBIDDEN_IN_APPEND[1][0].search("if (sym.kind == X)") is not None,
        "A012 detects .kind")
    ok &= expect(
        FORBIDDEN_IN_APPEND[2][0].search("e.as<slang::ast::Foo>()") is not None,
        "A012 detects .as<>")
    ok &= expect(
        FORBIDDEN_IN_APPEND[3][0].search("return diag::Unsupported(\"x\")")
        is not None,
        "A012 detects diag::Unsupported")
    ok &= expect(
        FORBIDDEN_IN_APPEND[3][0].search("return diag::Error(span, \"x\")")
        is not None,
        "A012 detects diag::Error")
    ok &= expect(
        FORBIDDEN_IN_APPEND[4][0].search(
            "throw support::InternalError(\"oops\");") is not None,
        "A012 detects throw support::InternalError")

    # A012 -- thin body must pass
    thin_append = (
        "auto AppendExpr(hir::Expr expr) -> hir::ExprId {\n"
        "  return scope_->AppendExpr(std::move(expr));\n"
        "}\n"
    )
    tm = APPEND_FN_DEF_PATTERN.search(thin_append)
    ok &= expect(tm is not None,
                 "A012 parser sees thin AppendExpr definition")
    body = thin_append[tm.end() - 1:]
    ok &= expect(
        not any(p.search(body) for p, _ in FORBIDDEN_IN_APPEND),
        "A012 thin Append body has no violations")

    # A012 -- out-of-line shape (auto + trailing return)
    out_of_line_auto = (
        "auto Foo::AppendStmt(hir::Stmt s) -> hir::StmtId {\n"
        "  return s_.AppendStmt(std::move(s));\n"
        "}\n"
    )
    om = APPEND_FN_DEF_PATTERN.search(out_of_line_auto)
    ok &= expect(om is not None and om.group(1) == "AppendStmt",
                 "A012 parser sees `auto Class::Append*` definition")

    # A012 -- out-of-line shape (void)
    out_of_line_void = (
        "void Foo::AppendRootStmt(mir::StmtId id) {\n"
        "  body_.root_stmts.push_back(id);\n"
        "}\n"
    )
    om2 = APPEND_FN_DEF_PATTERN.search(out_of_line_void)
    ok &= expect(om2 is not None and om2.group(1) == "AppendRootStmt",
                 "A012 parser sees `void Class::Append*` definition")

    return ok


# --- Main ----------------------------------------------------------------

CHECKS = [
    ("A001 HIR forbidden terms", check_a001),
    ("A002 HIR includes MIR headers", check_a002),
    ("A003 HIR/MIR includes driver/backend headers", check_a003),
    ("A004 raw std::expected<..., diag::Diagnostic>", check_a004),
    ("A005 state class holds facts member", check_a005),
    ("A006 support::Unsupported helper", check_a006),
    ("A007 raw std::unexpected diagnostic construction", check_a007),
    ("A008 Lower* returning Id without append verb", check_a008),
    ("A009 Lower* returning Result<void> without Into/Append/Populate",
     check_a009),
    ("A010 Lower*Data shape violation", check_a010),
    ("A011 Lowered* staging struct in lowering layer", check_a011),
    ("A012 Append* body must be thin storage", check_a012),
]


def main() -> int:
    if not run_self_tests():
        return 1

    repo_root = Path(__file__).resolve().parent.parent.parent

    failed = False
    for label, fn in CHECKS:
        errors = fn(repo_root)
        if errors:
            failed = True
            print(f"ERROR: {label}:")
            for e in errors:
                print(e)
            print()

    if failed:
        print(VIOLATION_HINT)
        print("See tools/policy/check_architecture.py for rule definitions.")
        return 1

    print("OK: architecture policy enforced")
    return 0


if __name__ == "__main__":
    sys.exit(main())
