"""Build a target as a shipped artifact rather than as part of this build.

The runtime library is not a build product of the compiler; it is a product the
compiler hands to a user, and it runs their simulation. Its optimization is
therefore a property of the artifact, not of how the tool that ships it happened
to be built -- a compiler built for a quick edit-run loop must still hand out a
runtime worth running.

The surface a user compiles against answers to the same thing, and for a second
reason: it has to be reachable as a set. A source file is staged by its place in
this repository, so a tool that finds one of them and takes the directory around
it is handed everything that happens to share that directory. Gathering the set
under a root of its own makes the set what is there, so the tool reads what this
build stated rather than what the tree looks like.
"""

def _shipped_transition_impl(_settings, _attr):
    return {"//command_line_option:compilation_mode": "opt"}

_shipped_transition = transition(
    implementation = _shipped_transition_impl,
    inputs = [],
    outputs = ["//command_line_option:compilation_mode"],
)

def _shipped_artifact_impl(ctx):
    out = ctx.actions.declare_file(ctx.attr.out)
    ctx.actions.symlink(output = out, target_file = ctx.file.lib)
    return [DefaultInfo(files = depset([out]))]

shipped_artifact = rule(
    implementation = _shipped_artifact_impl,
    doc = "Re-exports `lib` as `out`, built optimized whatever mode encloses it.",
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
