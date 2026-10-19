"""Build a target as a shipped artifact rather than as part of this build.

The runtime library is not a build product of the compiler; it is a product the
compiler hands to a user, and it runs their simulation. Its optimization is
therefore a property of the artifact, not of how the tool that ships it happened
to be built -- a compiler built for a quick edit-run loop must still hand out a
runtime worth running.

So is its compiler. A build under --lto optimizes the design's code together
with this library, which only works where the library carries bitcode
the LLVM release Lyra links can read; bitcode is read only by the release that
wrote it or a newer one. The library is therefore always compiled by the clang
`MODULE.bazel` pins to that release, whichever compiler builds Lyra, and its
objects carry that bitcode beside their machine code, so a link that does not
optimize across it reads them as ordinary objects.

The same holds for whatever else a build was asked to compile and link with. A
program a user builds links this library with their own compiler and nothing
this build knew, so a library instrumented because the compiler around it is
would ask that link for a runtime it was never told about. The one option it
keeps names which GCC installation provides the C++ library, since that is a
fact of the machine running the compiles rather than of the build, and the
library has to be the one the rest of that machine's builds use.

The surface a user compiles against answers to the same thing, and for a second
reason: it has to be reachable as a set. A source file is staged by its place in
this repository, so a tool that finds one of them and takes the directory around
it is handed everything that happens to share that directory. Gathering the set
under a root of its own makes the set what is there, so the tool reads what this
build stated rather than what the tree looks like.
"""

load("@rules_cc//cc/common:cc_info.bzl", "CcInfo")

_NAMES_GCC_INSTALLATION = ("--gcc-install-dir=", "--gcc-toolchain=")

def _gcc_installation(options):
    return [o for o in options if o.startswith(_NAMES_GCC_INSTALLATION)]

def _shipped_transition_impl(settings, _attr):
    return {
        "//command_line_option:compilation_mode": "opt",
        "//command_line_option:copt": _gcc_installation(settings["//command_line_option:copt"]) + ["-flto=thin", "-ffat-lto-objects"],
        "//command_line_option:extra_toolchains": ["@runtime_clang_toolchain//:all"],
        "//command_line_option:linkopt": [],
    }

_shipped_transition = transition(
    implementation = _shipped_transition_impl,
    inputs = ["//command_line_option:copt"],
    outputs = [
        "//command_line_option:compilation_mode",
        "//command_line_option:copt",
        "//command_line_option:extra_toolchains",
        "//command_line_option:linkopt",
    ],
)

def _shipped_artifact_impl(ctx):
    out = ctx.actions.declare_file(ctx.attr.out)
    ctx.actions.symlink(output = out, target_file = ctx.file.lib)
    return [DefaultInfo(files = depset([out]))]

shipped_artifact = rule(
    implementation = _shipped_artifact_impl,
    doc = "Re-exports `lib` as `out`, built optimized with none of the enclosing build's `--copt` but the one naming the machine's GCC installation, and no `--linkopt`.",
    attrs = {
        "lib": attr.label(
            allow_single_file = True,
            cfg = _shipped_transition,
            mandatory = True,
        ),
        "out": attr.string(
            doc = "Shipped file name, which its consumers spell independently.",
            mandatory = True,
        ),
        "_allowlist_function_transition": attr.label(
            default = "@bazel_tools//tools/allowlists/function_transition_allowlist",
        ),
    },
)

def _shipped_headers_impl(ctx):
    strip = ctx.attr.strip_prefix.rstrip("/") + "/"
    root = ctx.attr.root.rstrip("/") + "/"
    staged = []
    for header in ctx.files.hdrs:
        if not header.short_path.startswith(strip):
            fail("%s is not under %s" % (header.short_path, strip))
        out = ctx.actions.declare_file(root + header.short_path[len(strip):])
        ctx.actions.symlink(output = out, target_file = header)
        staged.append(out)
    return [DefaultInfo(files = depset(staged))]

shipped_headers = rule(
    implementation = _shipped_headers_impl,
    doc = """Gathers `hdrs` under a root holding them and nothing else.

Whoever ships the surface names it here, and a tool that reaches one of these
files can take the root around it without reaching anything this build did not
put there.""",
    attrs = {
        "hdrs": attr.label_list(
            allow_files = [".hpp"],
            doc = "The headers the surface consists of.",
            mandatory = True,
        ),
        "root": attr.string(
            doc = "Where the surface is staged, as one path segment or more.",
            mandatory = True,
        ),
        "strip_prefix": attr.string(
            doc = "Leading path the staged layout drops.",
            mandatory = True,
        ),
    },
)

_RuntimeSourcesInfo = provider(
    doc = "The files a C++ library and every library below it compile.",
    fields = {
        "sources": "depset of the translation units, assembly included",
        "private_headers": "depset of headers listed among sources",
    },
)

_TRANSLATION_UNITS = ["c", "cc", "cpp", "S"]

def _runtime_sources_aspect_impl(_target, ctx):
    files = getattr(ctx.rule.files, "srcs", [])
    below = [
        dep[_RuntimeSourcesInfo]
        for dep in getattr(ctx.rule.attr, "deps", [])
        if _RuntimeSourcesInfo in dep
    ]
    return [_RuntimeSourcesInfo(
        sources = depset(
            [f for f in files if f.extension in _TRANSLATION_UNITS],
            transitive = [info.sources for info in below],
        ),
        private_headers = depset(
            [f for f in files if f.extension not in _TRANSLATION_UNITS],
            transitive = [info.private_headers for info in below],
        ),
    )]

_runtime_sources_aspect = aspect(
    implementation = _runtime_sources_aspect_impl,
    attr_aspects = ["deps"],
)

def _shipped_sources_impl(ctx):
    lib = ctx.attr.lib[0]
    sources = lib[_RuntimeSourcesInfo].sources.to_list()
    context = lib[CcInfo].compilation_context
    root = ctx.attr.root.rstrip("/") + "/"

    staged = []
    for file in sources + context.headers.to_list() + lib[_RuntimeSourcesInfo].private_headers.to_list():
        out = ctx.actions.declare_file(root + file.path)
        ctx.actions.symlink(output = out, target_file = file)
        staged.append(out)

    # How every source is compiled, beside the files, so that whoever compiles
    # them reads this build's statement of it rather than restating it. Each
    # line is a flag and its argument, a path relative to the root.
    # A directory nothing was staged under is a place the build looks for
    # generated headers this library has none of, and is left out.
    staged_dirs = {".": True}
    for file in staged:
        path = file.short_path[len(root):]
        for _ in range(path.count("/")):
            path = path.rpartition("/")[0]
            staged_dirs[path] = True
    include_lines = [
        "%s %s" % (flag, d)
        for flag, dirs in [
            ("-iquote", context.quote_includes),
            ("-isystem", context.includes),
            ("-isystem", context.system_includes),
            ("-isystem", context.external_includes),
        ]
        for d in dirs.to_list()
        if d in staged_dirs
    ]
    manifests = {
        "defines.txt": context.defines.to_list(),
        "include_dirs.txt": include_lines,
        "sources.txt": [f.path for f in sources],
    }
    for name, lines in manifests.items():
        out = ctx.actions.declare_file(root + name)
        ctx.actions.write(out, "".join([line + "\n" for line in lines]))
        staged.append(out)
    return [DefaultInfo(files = depset(staged))]

shipped_sources = rule(
    implementation = _shipped_sources_impl,
    doc = """Gathers the source of `lib` and of every library below it under a root
holding them and nothing else, with how each is compiled.

A program's own compiler builds the runtime from these where it optimizes the
program together with it and cannot read the prebuilt library's bitcode. The
lists beside the files are this build's own: the translation units, the
include directories, and the macros every compile defines.""",
    attrs = {
        "lib": attr.label(
            aspects = [_runtime_sources_aspect],
            cfg = _shipped_transition,
            mandatory = True,
            providers = [CcInfo],
        ),
        "root": attr.string(
            doc = "Where the sources are staged, as one path segment or more.",
            mandatory = True,
        ),
        "_allowlist_function_transition": attr.label(
            default = "@bazel_tools//tools/allowlists/function_transition_allowlist",
        ),
    },
)
