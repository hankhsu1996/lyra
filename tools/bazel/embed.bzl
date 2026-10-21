"""Rule for compiling a text file into the binary as a string."""

load("@rules_cc//cc/common:cc_common.bzl", "cc_common")
load("@rules_cc//cc/common:cc_info.bzl", "CcInfo")

def _embedded_text_impl(ctx):
    output = ctx.actions.declare_file(ctx.attr.out)
    ctx.actions.run_shell(
        outputs = [output],
        inputs = [ctx.file.src],
        command = """
set -e
exec > {output}
printf '// Generated from %s.\\n#pragma once\\n\\n#include <string_view>\\n\\n' '{src}'
printf 'namespace {namespace} {{\\n\\ninline constexpr std::string_view {symbol} = R"LYRA_EMBEDDED('
cat '{src}'
printf ')LYRA_EMBEDDED";\\n\\n}}  // namespace {namespace}\\n'
""".format(
            output = output.path,
            src = ctx.file.src.path,
            namespace = ctx.attr.namespace,
            symbol = ctx.attr.symbol,
        ),
    )
    return [
        DefaultInfo(files = depset([output])),
        CcInfo(
            compilation_context = cc_common.create_compilation_context(
                headers = depset([output]),
            ),
        ),
    ]

# A header declaring one `std::string_view` whose text is the file `src`, for a
# text the binary hands on rather than reads from disk.
embedded_text = rule(
    implementation = _embedded_text_impl,
    attrs = {
        "src": attr.label(allow_single_file = True, mandatory = True),
        "out": attr.string(mandatory = True),
        "namespace": attr.string(mandatory = True),
        "symbol": attr.string(mandatory = True),
    },
)
