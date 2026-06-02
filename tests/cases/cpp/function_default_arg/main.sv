module Top;
  int base;
  int r_basic;
  int r_placeholder;
  int r_reeval_lo;
  int r_reeval_hi;

  function int addab(int a, int b = 5);
    return a + b;
  endfunction

  function int read3(int a = 1, int b = 2, int c = 3);
    return a * 100 + b * 10 + c;
  endfunction

  function int addbase(int x, int y = base);
    return x + y;
  endfunction

  initial begin
    r_basic = addab(7);
    r_placeholder = read3(, 5);
    base = 10;
    r_reeval_lo = addbase(1);
    base = 100;
    r_reeval_hi = addbase(1);
  end
endmodule
