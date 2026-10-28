// @reports: error
//
// A method reached through a handle holding null is an illegal access whose
// result the standard leaves indeterminate, letting a tool report an error (LRM
// 8.4). A process's hooks do nothing (LRM 9.7, 18.6.2), and calling one is
// still a call made on the object a handle names, so Lyra ends the run with an
// error there as it does for any method a null handle reaches.
module Top;
  process p;

  initial begin
    $display("Ready for the report");
    p.pre_randomize();
    $display("All checks passed");
  end
endmodule
