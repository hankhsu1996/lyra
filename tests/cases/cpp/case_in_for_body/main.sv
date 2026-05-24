module Top;
  int zero;
  int pair;
  int three;
  int other;
  initial begin
    zero = 0;
    pair = 0;
    three = 0;
    other = 0;
    for (int i = 0; i < 5; i = i + 1) begin
      case (i)
        0:    zero  = zero  + 1;
        1, 2: pair  = pair  + 1;
        3:    three = three + 1;
        default: other = other + 1;
      endcase
    end
  end
endmodule
