// A continuous assignment's left-hand side may be a concatenation or a nested
// concatenation of nets, variables, and constant bit- and part-selects of them
// (LRM 10.3.2, Table 10-1). The right-hand side drives the members together,
// first member most significant, each taking as many bits as it is wide, and
// the members follow every change of an operand (LRM 10.3). A member that is a
// part-select is driven in the bits it selects and nowhere else, so two
// assignments may each drive a different part of one net.
module Top;
  logic [7:0] source;

  logic [3:0] var_high, var_low;
  wire [3:0] net_high, net_low;
  wire [3:0] nested_a, nested_b;
  logic [3:0] nested_c;
  wire [7:0] shared;
  logic [1:0] part_tail;
  wire [7:0] lone;

  assign {var_high, var_low} = source;
  assign {net_high, net_low} = source;
  assign {{nested_a, nested_b}, {nested_c}} = {source, source[3:0]};
  assign {shared[7:6], part_tail} = source[3:0];
  assign {shared[5:0]} = source[7:2];
  assign {lone} = ~source;

  logic [3:0] seen_var_high, seen_var_low, seen_net_high, seen_net_low;
  logic [3:0] seen_a, seen_b, seen_c;
  logic [7:0] seen_shared, seen_lone;
  logic [1:0] seen_tail;

  initial begin
    source = 8'hA5;
    #1;
    seen_var_high = var_high;
    seen_var_low = var_low;
    seen_net_high = net_high;
    seen_net_low = net_low;
    seen_a = nested_a;
    seen_b = nested_b;
    seen_c = nested_c;
    seen_shared = shared;
    seen_tail = part_tail;
    seen_lone = lone;
    source = 8'h3E;
    #1;
  end

  final begin
    if (seen_var_high !== 4'hA || seen_var_low !== 4'h5)
      $fatal(1, "variables first held %h %h", seen_var_high, seen_var_low);
    if (seen_net_high !== 4'hA || seen_net_low !== 4'h5)
      $fatal(1, "nets first held %h %h", seen_net_high, seen_net_low);
    if (seen_a !== 4'hA || seen_b !== 4'h5 || seen_c !== 4'h5)
      $fatal(1, "nested first held %h %h %h", seen_a, seen_b, seen_c);
    // source[3:0] = 0101 drives shared[7:6] = 01 and part_tail = 01, and
    // source[7:2] = 101001 drives shared[5:0].
    if (seen_shared !== 8'b01_101001)
      $fatal(1, "shared first held %b", seen_shared);
    if (seen_tail !== 2'b01) $fatal(1, "part_tail first held %b", seen_tail);
    if (seen_lone !== 8'h5A) $fatal(1, "lone first held %h", seen_lone);

    if (var_high !== 4'h3 || var_low !== 4'hE)
      $fatal(1, "variables held %h %h, expected 3 e", var_high, var_low);
    if (net_high !== 4'h3 || net_low !== 4'hE)
      $fatal(1, "nets held %h %h, expected 3 e", net_high, net_low);
    if (nested_a !== 4'h3 || nested_b !== 4'hE || nested_c !== 4'hE)
      $fatal(1, "nested held %h %h %h", nested_a, nested_b, nested_c);
    // source[3:0] = 1110, source[7:2] = 001111.
    if (shared !== 8'b11_001111) $fatal(1, "shared held %b", shared);
    if (part_tail !== 2'b10) $fatal(1, "part_tail held %b", part_tail);
    if (lone !== 8'hC1) $fatal(1, "lone held %h, expected c1", lone);
    $display("All checks passed");
  end
endmodule
