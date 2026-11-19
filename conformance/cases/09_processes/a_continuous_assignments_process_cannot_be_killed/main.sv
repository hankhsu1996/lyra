// @reports: error
//
// kill(), await(), suspend() and resume() are restricted to a process created
// by an initial or always procedure, or a fork of one (LRM 9.7). A continuous
// assignment is a process of its own (LRM 4.9.1) and neither, so killing it is
// an error even from an initial procedure, whose own process may be
// controlled.
module Top;
  logic [3:0] a;
  wire [3:0] y;
  process assignment;

  function automatic logic [3:0] follow(logic [3:0] v);
    assignment = process::self();
    return v;
  endfunction

  assign y = follow(a);

  initial begin
    a = 4'd1;
    #1;
    $display("Ready for the report");
    assignment.kill();
    $display("All checks passed");
  end
endmodule
