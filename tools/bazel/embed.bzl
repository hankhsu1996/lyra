"""Rule for embedding file contents into C++ headers."""

load("@rules_cc//cc/common:cc_common.bzl", "cc_common")
load("@rules_cc//cc/common:cc_info.bzl", "CcInfo")

def _embed_headers_impl(ctx):
    output = ctx.actions.declare_file(ctx.attr.out)

    # Build the shell script content
    script = """
set -e
exec > {output}
cat << 'HEADER'
// Auto-generated file. Do not edit.
#pragma once
#include <map>
#include <string>

namespace lyra::embedded {{

inline const std::map<std::string, std::string> kEmbeddedFiles = {{
HEADER
for f in {files}; do
    relpath="${{f#*{strip_prefix}}}"
    printf '    {{"%s", R"SDK_RAW(' "$relpath"
    cat "$f"
    echo ')SDK_RAW"}},'
done
cat << 'FOOTER'
}};

}}  // namespace lyra::embedded
FOOTER
""".format(
        output = output.path,
        files = " ".join([f.path for f in ctx.files.srcs]),
        strip_prefix = ctx.attr.strip_prefix,
    )

    ctx.actions.run_shell(
        outputs = [output],
        inputs = ctx.files.srcs,
        command = script,
    )

    return [
        DefaultInfo(files = depset([output])),
        CcInfo(
            compilation_context = cc_common.create_compilation_context(
                headers = depset([output]),
            ),
        ),
    ]

embed_headers = rule(
    implementation = _embed_headers_impl,
    attrs = {
        "srcs": attr.label_list(allow_files = True),
        "strip_prefix": attr.string(default = "include/"),
        "out": attr.string(mandatory = True),
    },
)
