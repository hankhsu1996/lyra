// An output port may be connected to a concatenation of variables or of nets
// (LRM 23.3.3.2, 23.3.3.3), and the connection is a continuous assignment from
// the port to what it is connected to (LRM 23.3.3). So the members take the
// port's bits, first member most significant, and follow it when it changes.
// That holds whether the port is declared as a variable or as a net, for a
// member that is a part-select, and for a concatenation of one member. Each
// instance drives the members its own connection names.
module Doubler (
    input logic [7:0] value_i,
    output logic [7:0] variable_o,
    output wire [7:0] net_o
);
  assign variable_o = value_i;
  assign net_o = ~value_i;
endmodule

module Top;
  logic [7:0] first_in, second_in;

  logic [3:0] var_high, var_low;
  wire [3:0] net_high, net_low;
  logic [7:0] lone;
  wire [7:0] selected;
  logic [1:0] tail;
  logic [5:0] head;

  Doubler first (
      .value_i(first_in),
      .variable_o({var_high, var_low}),
      .net_o({net_high, net_low})
  );
  Doubler second (
      .value_i(second_in),
      .variable_o({lone}),
      .net_o({selected[7:2], tail})
  );
  assign selected[1:0] = 2'b10;

  logic [3:0] seen_var_high, seen_net_low;
  logic [7:0] seen_lone;

  initial begin
    first_in = 8'h96;
    second_in = 8'h0F;
    #1;
    seen_var_high = var_high;
    seen_net_low = net_low;
    seen_lone = lone;
    first_in = 8'h4B;
    second_in = 8'hC3;
    #1;
  end

  final begin
    if (seen_var_high !== 4'h9)
      $fatal(1, "var_high first held %h, expected 9", seen_var_high);
    if (seen_net_low !== 4'h9)
      $fatal(1, "net_low first held %h, expected 9", seen_net_low);
    if (seen_lone !== 8'h0F)
      $fatal(1, "lone first held %h, expected 0f", seen_lone);

    if (var_high !== 4'h4 || var_low !== 4'hB)
      $fatal(1, "variables held %h %h, expected 4 b", var_high, var_low);
    if (net_high !== 4'hB || net_low !== 4'h4)
      $fatal(1, "nets held %h %h, expected b 4", net_high, net_low);
    if (lone !== 8'hC3) $fatal(1, "lone held %h, expected c3", lone);
    // ~8'hC3 = 0011_1100: the upper six bits drive selected[7:2], the lower
    // two drive tail, and selected[1:0] keeps its own driver.
    if (selected !== 8'b001111_10)
      $fatal(1, "selected held %b, expected 00111110", selected);
    if (tail !== 2'b00) $fatal(1, "tail held %b, expected 00", tail);
    $display("All checks passed");
  end
endmodule
