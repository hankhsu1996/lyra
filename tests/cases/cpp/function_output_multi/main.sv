module Top;
  int a;
  int b;

  function void two(output int p, output int q);
    p = 3;
    q = 7;
  endfunction

  initial begin
    two(a, b);
  end
endmodule
