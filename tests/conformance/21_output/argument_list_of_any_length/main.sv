// The syntax of the formatting and scanning tasks takes a list of arguments
// and states no length for it, so a call carrying several hundred is carried
// out like one carrying three: every argument is converted, in the order it
// was written (LRM 21.2.1 Syntax 21-1, 21.3.3, 21.3.4.3).
`define X4(a) a, a, a, a
`define X16(a) `X4(a), `X4(a), `X4(a), `X4(a)
`define X64(a) `X16(a), `X16(a), `X16(a), `X16(a)
`define X320(a) `X64(a), `X64(a), `X64(a), `X64(a), `X64(a)

// 320 names sharing a prefix, `p0000` through `p4333`.
`define N4(p) p``0, p``1, p``2, p``3
`define N16(p) `N4(p``0), `N4(p``1), `N4(p``2), `N4(p``3)
`define N64(p) `N16(p``0), `N16(p``1), `N16(p``2), `N16(p``3)
`define N320(p) `N64(p``0), `N64(p``1), `N64(p``2), `N64(p``3), `N64(p``4)

module Top;
  int seven;
  int three;
  int `N320(got);
  int scanned;
  string every_decimal;
  string every_number;
  string written;
  string returned;

  initial begin
    seven = 7;
    three = 3;
    every_decimal = {321{"%0d"}};

    written = "unset";
    $sformat(written, every_decimal, `X320(seven), three);

    returned = "unset";
    returned = $sformatf(every_decimal, `X320(seven), three);

    every_number = {320{"5 "}};
    scanned = 0;
    scanned = $sscanf(every_number, {320{"%d"}}, `N320(got));
  end

  final begin
    if (written != {{320{"7"}}, "3"})
      $fatal(1, "$sformat of 321 arguments gave %0d characters: '%s'",
             written.len(), written);
    if (returned != {{320{"7"}}, "3"})
      $fatal(1, "$sformatf of 321 arguments gave %0d characters: '%s'",
             returned.len(), returned);
    if (scanned !== 320)
      $fatal(1, "$sscanf into 320 arguments assigned %0d", scanned);
    if (got0000 !== 5)
      $fatal(1, "$sscanf left %0d in its first argument, expected 5", got0000);
    if (got4333 !== 5)
      $fatal(1, "$sscanf left %0d in its last argument, expected 5", got4333);
    $display("All checks passed");
  end
endmodule
