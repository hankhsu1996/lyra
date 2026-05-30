module Top;
  logic [3:0] val;
  int r;

  initial begin
    val = 4'b1010;
    r = 0;
    case (val) inside
      4'b00??: r = 1;
      4'b10??: r = 2;
      4'b11??: r = 3;
      default: r = 99;
    endcase
  end
endmodule
