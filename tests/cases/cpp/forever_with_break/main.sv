module Top;
  int x;
  initial begin
    x = 0;
    forever begin
      x = x + 1;
      if (x == 3) break;
    end
  end
endmodule
