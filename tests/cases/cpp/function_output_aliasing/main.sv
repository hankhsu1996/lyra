module Top;
  int g;

  function void f(output int b);
    b = 1;
    g = 2;
    b = b + g;
  endfunction

  initial begin
    g = 99;
    f(g);
  end
endmodule
