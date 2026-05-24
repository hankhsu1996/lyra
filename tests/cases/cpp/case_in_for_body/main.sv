module Top;
  initial begin
    int zero = 0;
    int pair = 0;
    int three = 0;
    int other = 0;
    for (int i = 0; i < 5; i = i + 1) begin
      case (i)
        0:    zero  = zero  + 1;
        1, 2: pair  = pair  + 1;
        3:    three = three + 1;
        default: other = other + 1;
      endcase
    end
    $display("zero=%0d", zero);
    $display("pair=%0d", pair);
    $display("three=%0d", three);
    $display("other=%0d", other);
  end
endmodule
