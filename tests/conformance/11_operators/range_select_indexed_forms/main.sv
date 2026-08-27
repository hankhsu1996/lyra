// An indexed part-select names a fixed number of bits starting at a base
// that may vary at run time: vect[base +: width] ascends the bit range from
// the base and vect[base -: width] descends it, so from one base the two
// forms name different bits. The width is a positive constant expression.
// Both forms read and are written (LRM 11.5.1).
module Top;
  bit [15:0] source;
  bit [31:0] up_target;
  bit [31:0] down_target;
  bit [31:0] const_up_target;
  bit [31:0] const_down_target;
  int base;
  localparam int W = 4;
  bit [3:0] up_from_base;
  bit [3:0] down_from_base;
  bit [3:0] up_from_const;
  bit [3:0] down_from_const;
  bit [3:0] up_param_width;

  initial begin
    source = 16'hABCD;

    base = 8;
    up_from_base = source[base +: 4];
    down_from_base = source[base -: 4];
    up_param_width = source[base +: W];
    up_from_const = source[8 +: 4];
    down_from_const = source[8 -: 4];

    up_target = 32'h00000000;
    base = 12;
    up_target[base +: 8] = 8'hAB;

    down_target = 32'h00000000;
    base = 12;
    down_target[base -: 8] = 8'hAB;

    const_up_target = 32'h00000000;
    const_up_target[12 +: 8] = 8'hAB;

    const_down_target = 32'h00000000;
    const_down_target[12 -: 8] = 8'hAB;
  end

  final begin
    if (up_from_base !== 4'hB)
      $fatal(1, "source[8 +: 4] was %h, expected b", up_from_base);
    if (down_from_base !== 4'hE)
      $fatal(1, "source[8 -: 4] was %h, expected e", down_from_base);
    if (up_param_width !== 4'hB)
      $fatal(1, "source[8 +: W] was %h, expected b", up_param_width);
    if (up_from_const !== 4'hB)
      $fatal(1, "constant-base +: was %h, expected b", up_from_const);
    if (down_from_const !== 4'hE)
      $fatal(1, "constant-base -: was %h, expected e", down_from_const);
    if (up_target !== 32'h000AB000)
      $fatal(1, "write through [12 +: 8] gave %h, expected 000ab000",
             up_target);
    if (down_target !== 32'h00001560)
      $fatal(1, "write through [12 -: 8] gave %h, expected 00001560",
             down_target);
    if (const_up_target !== 32'h000AB000)
      $fatal(1, "constant-base +: write gave %h, expected 000ab000",
             const_up_target);
    if (const_down_target !== 32'h00001560)
      $fatal(1, "constant-base -: write gave %h, expected 00001560",
             const_down_target);
    $display("All checks passed");
  end
endmodule
