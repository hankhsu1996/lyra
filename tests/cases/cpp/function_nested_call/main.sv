module Top;
  int result;

  function int inner(int x);
    return x * 2;
  endfunction

  function int outer(int x);
    return inner(x) + 10;
  endfunction

  initial begin
    result = outer(5);
  end
endmodule
