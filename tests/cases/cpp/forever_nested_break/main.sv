module Top;
  int outer;
  int inner_total;
  initial begin
    outer = 0;
    inner_total = 0;
    forever begin
      int inner;
      outer = outer + 1;
      inner = 0;
      forever begin
        inner = inner + 1;
        if (inner == 2) break;
      end
      inner_total = inner_total + inner;
      if (outer == 3) break;
    end
  end
endmodule
