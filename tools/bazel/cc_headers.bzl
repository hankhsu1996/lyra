"""Expose a `cc_library`'s transitive header closure as runfiles.

`cc_library` in `data` only propagates compiled outputs, not hdrs. Use this
when a forked compiler invocation needs the headers visible at `-I` time.
"""

load("@rules_cc//cc/common:cc_info.bzl", "CcInfo")

def _cc_header_files_impl(ctx):
    headers = depset(transitive = [
        ctx.attr.dep[CcInfo].compilation_context.headers,
    ])
    return [
        DefaultInfo(
            files = headers,
            runfiles = ctx.runfiles(transitive_files = headers),
        ),
    ]

cc_header_files = rule(
    implementation = _cc_header_files_impl,
    attrs = {
        "dep": attr.label(providers = [CcInfo], mandatory = True),
    },
)
