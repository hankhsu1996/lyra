// An output or inout actual is assigned from its formal when the subroutine
// returns, and an inout actual is copied into its formal when the subroutine
// is called (LRM 13.5). The actual may be any expression that is legal on the
// left-hand side of a procedural assignment (LRM 13.5), which a concatenation
// is (Table 10-1), so its members take the returned value's bits, first member
// most significant, and an inout formal starts from the members joined. An
// index in a member is evaluated once for the copy in and the copy out.
module Top;
  logic [3:0] out_high, out_low;
  logic [3:0] inout_high, inout_low;
  logic [3:0] func_high, func_low;
  logic [3:0] nested_a, nested_b, nested_c;
  logic [3:0] slots[4];
  logic [3:0] beside;
  int calls;
  int answered;

  task automatic produce(output logic [7:0] value);
    value = 8'hB4;
  endtask

  task automatic invert(inout logic [7:0] value);
    value = ~value;
  endtask

  task automatic produce_wide(output logic [11:0] value);
    value = 12'h7D2;
  endtask

  function automatic int swap_halves(inout logic [7:0] value);
    value = {value[3:0], value[7:4]};
    return 5;
  endfunction

  function automatic int next_slot();
    calls++;
    return calls + 1;
  endfunction

  initial begin
    {out_high, out_low} = 8'h00;
    produce({out_high, out_low});

    {inout_high, inout_low} = 8'h3A;
    invert({inout_high, inout_low});

    {func_high, func_low} = 8'h6E;
    answered = 0;
    answered = swap_halves({func_high, func_low});

    {nested_a, nested_b, nested_c} = 12'h000;
    produce_wide({nested_a, {nested_b, nested_c}});

    slots[0] = 4'h0;
    slots[1] = 4'h0;
    slots[2] = 4'h9;
    slots[3] = 4'h0;
    beside = 4'h1;
    calls = 0;
    invert({slots[next_slot()], beside});
  end

  final begin
    if (out_high !== 4'hB || out_low !== 4'h4)
      $fatal(1, "output left %h %h, expected b 4", out_high, out_low);
    if (inout_high !== 4'hC || inout_low !== 4'h5)
      $fatal(1, "inout left %h %h, expected c 5", inout_high, inout_low);
    if (answered !== 5) $fatal(1, "the function answered %0d", answered);
    if (func_high !== 4'hE || func_low !== 4'h6)
      $fatal(1, "function inout left %h %h, expected e 6", func_high, func_low);
    if (nested_a !== 4'h7 || nested_b !== 4'hD || nested_c !== 4'h2)
      $fatal(1, "nested left %h %h %h", nested_a, nested_b, nested_c);
    if (calls !== 1) $fatal(1, "the index was evaluated %0d times", calls);
    if (slots[2] !== 4'h6) $fatal(1, "slots[2] was %h, expected 6", slots[2]);
    if (beside !== 4'hE) $fatal(1, "beside was %h, expected e", beside);
    if (slots[3] !== 4'h0) $fatal(1, "slots[3] was %h, expected 0", slots[3]);
    $display("All checks passed");
  end
endmodule
