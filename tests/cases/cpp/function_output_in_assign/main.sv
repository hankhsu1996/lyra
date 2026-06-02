module Top;
  int r;
  int o;

  function int compute(input int a, output int leftover);
    leftover = a % 3;
    return a / 3;
  endfunction

  initial begin
    r = compute(10, o);
  end
endmodule
