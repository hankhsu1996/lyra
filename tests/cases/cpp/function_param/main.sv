module Top;
  int result;

  function int double_it(int x);
    return x * 2;
  endfunction

  initial begin
    result = double_it(21);
  end
endmodule
