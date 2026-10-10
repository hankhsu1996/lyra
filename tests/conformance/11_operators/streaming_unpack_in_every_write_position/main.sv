// A streaming concatenation is a variable_lvalue (LRM A.8.5), so it may stand
// wherever one is written and not only on the left of a procedural assignment:
// as the target of a continuous assignment and as an output actual of a
// subroutine. Wherever it stands it unpacks the same way (LRM 11.4.14.3): the
// value is read as a stream of bits, `<<` reverses the order of its slices, and
// the members are filled in the order written from the most significant end.
module Top;
  logic [7:0] source;

  logic [3:0] cont_first, cont_second;
  logic [3:0] rev_first, rev_second;
  assign {>>{cont_first, cont_second}} = source;
  assign {<<4{rev_first, rev_second}} = source;

  logic [3:0] out_first, out_second;
  byte array_target[2];

  task automatic produce(output logic [7:0] value);
    value = 8'h2D;
  endtask

  task automatic produce_wide(output logic [15:0] value);
    value = 16'h1234;
  endtask

  logic [3:0] seen_cont_first, seen_rev_first;

  initial begin
    source = 8'h9A;
    #1;
    seen_cont_first = cont_first;
    seen_rev_first = rev_first;
    source = 8'h6F;
    #1;

    out_first = 4'h0;
    out_second = 4'h0;
    produce({<<4{out_first, out_second}});

    array_target[0] = 8'h00;
    array_target[1] = 8'h00;
    produce_wide({>>{array_target}});
  end

  final begin
    if (seen_cont_first !== 4'h9)
      $fatal(1, "cont_first first held %h, expected 9", seen_cont_first);
    if (seen_rev_first !== 4'hA)
      $fatal(1, "rev_first first held %h, expected a", seen_rev_first);
    if (cont_first !== 4'h6 || cont_second !== 4'hF)
      $fatal(1, "continuous >> left %h %h", cont_first, cont_second);
    if (rev_first !== 4'hF || rev_second !== 4'h6)
      $fatal(1, "continuous << left %h %h", rev_first, rev_second);
    if (out_first !== 4'hD || out_second !== 4'h2)
      $fatal(1, "output << left %h %h", out_first, out_second);
    if (array_target[0] !== 8'h12 || array_target[1] !== 8'h34)
      $fatal(1, "output >> left %h %h", array_target[0], array_target[1]);
    $display("All checks passed");
  end
endmodule
