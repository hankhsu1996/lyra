module Top;
  initial begin
    // LRM 21.3.1: STDOUT pre-bound FD is 32'h8000_0001. Output goes through
    // the same stream sink as $display.
    $fdisplay(32'h8000_0001, "via stdout FD");
  end
endmodule
