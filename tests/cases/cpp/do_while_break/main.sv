module Top;
  int x;
  initial begin
    x = 0;
    do begin
      x = x + 1;
      if (x == 2) break;
    end while (1);
  end
endmodule
