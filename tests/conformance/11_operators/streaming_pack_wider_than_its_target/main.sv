// @reports: error
//
// A stream assigned to a fixed-size target narrower than the stream is an
// error, and where the stream's width is the running program's, three bytes
// of a queue reaching sixteen bits here, the error is raised when the
// assignment runs (LRM 11.4.14).
module Top;
  bit [15:0] target;

  initial begin
    byte bytes[$];
    bytes = {8'h01, 8'h02, 8'h03};
    $display("Ready for the report");
    target = {>>{bytes}};
    $display("All checks passed");
  end
endmodule
