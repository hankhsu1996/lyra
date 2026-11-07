// @reports-none: violation
//
// A unique-case one of whose items matches is no violation (LRM 12.5.3), and
// that holds however the statement is reached: a function a strobed write's
// argument calls executes it at the end of the time step (LRM 21.2.2), and a
// match there reports nothing.
module Top;
  function automatic int classify(bit [2:0] value);
    unique case (value)
      3'd0, 3'd1: return 1;
      3'd2: return 2;
    endcase
    return 7;
  endfunction

  initial begin
    $strobe("classified as %0d", classify(3'd2));
    #1 $display("All checks passed");
  end
endmodule
