module Top;
  int x;
  initial begin
    x = 1;
  end
  if (1) begin
    if (0) begin
    end
  end
  case (1)
    0: begin end
    1: begin end
    default: begin end
  endcase
endmodule
