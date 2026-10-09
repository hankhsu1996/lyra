// An always_comb is triggered once at time zero (LRM 9.2.2.2), and a function
// it calls is part of what it runs, so that run calls the function once. What
// the procedure is sensitive to includes what the function reads (LRM
// 9.2.2.2.1); learning that runs nothing, so a function counting its own calls
// counts one at time zero and one more for each change to what it reads. The
// same holds where the function is reached through another function.
module Top;
  logic [3:0] a = 4'd3;
  logic [3:0] b = 4'd5;
  logic [3:0] y;
  logic [3:0] z;
  int direct_calls;
  int inner_calls;

  function automatic logic [3:0] step(logic [3:0] v);
    direct_calls++;
    return v + 4'd1;
  endfunction

  function automatic logic [3:0] inner(logic [3:0] v);
    inner_calls++;
    return v + 4'd2;
  endfunction

  function automatic logic [3:0] outer(logic [3:0] v);
    return inner(v);
  endfunction

  always_comb y = step(a);
  always_comb z = outer(b);

  initial begin
    #1;
    if (direct_calls !== 1)
      $fatal(1, "direct_calls at time 1 was %0d, expected 1", direct_calls);
    if (inner_calls !== 1)
      $fatal(1, "inner_calls at time 1 was %0d, expected 1", inner_calls);
    if (y !== 4'd4) $fatal(1, "y at time 1 was %0d, expected 4", y);
    if (z !== 4'd7) $fatal(1, "z at time 1 was %0d, expected 7", z);

    a = 4'd8;
    b = 4'd1;
    #1;
    if (direct_calls !== 2)
      $fatal(1, "direct_calls at time 2 was %0d, expected 2", direct_calls);
    if (inner_calls !== 2)
      $fatal(1, "inner_calls at time 2 was %0d, expected 2", inner_calls);
    if (y !== 4'd9) $fatal(1, "y at time 2 was %0d, expected 9", y);
    if (z !== 4'd3) $fatal(1, "z at time 2 was %0d, expected 3", z);
    $display("All checks passed");
    $finish;
  end
endmodule
