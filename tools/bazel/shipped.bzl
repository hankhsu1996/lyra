"""Build a target as a shipped artifact rather than as part of this build.

The runtime library is not a build product of the compiler; it is a product the
compiler hands to a user, and it runs their simulation. Its optimization is
therefore a property of the artifact, not of how the tool that ships it happened
to be built -- a compiler built for a quick edit-run loop must still hand out a
runtime worth running.
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
