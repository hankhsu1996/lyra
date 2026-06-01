module Top;
  int result;

  function int check(int x);
    if (x < 0)
      return -1;
    if (x == 0)
      return 0;
    return 1;
  endfunction

  initial begin
    result = check(5) + check(0) * 10 + check(-3) * 100;
  end
endmodule
