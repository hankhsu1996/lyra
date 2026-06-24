module Top;
  function automatic int dbl(int x);
    return x * 2;
  endfunction

  logic [3:0] a;
  int fn_result;
  logic [11:0] rep;
  bit in_set;

  // Simulation-time value expressions in continuous-assignment position: a user
  // function call (LRM 13.4), a replication (LRM 11.4.12.1), and the inside
  // operator (LRM 11.4.13). All lower through the same context-free handlers as
  // their procedural counterparts.
  assign fn_result = dbl(a);
  assign rep = {3{a}};
  assign in_set = (a inside {4'd1, 4'd5, 4'd9});

  initial begin
    a = 4'd5;
  end
endmodule
