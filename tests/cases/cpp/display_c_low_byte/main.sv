// LRM 21.2.1.1 example: %c emits the low byte of an integral argument
// (the verbatim example uses `logic [31:0]`).
module Top;
  logic [31:0] rval;
  initial begin
    rval = 101;
    $display("rval has %c ascii character value", rval);
  end
endmodule
