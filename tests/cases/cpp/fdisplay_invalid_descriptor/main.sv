module Top;
  initial begin
    // LRM 21.3.1: descriptor 0 is invalid; output is silently dropped.
    $fdisplay(0, "should not appear");
    $display("OK");
  end
endmodule
