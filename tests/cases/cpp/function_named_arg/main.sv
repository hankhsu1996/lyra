module Top;
  int r_named;
  int r_mixed;

  function int sub(int a, int b);
    return a - b;
  endfunction

  initial begin
    r_named = sub(.b(3), .a(20));
    r_mixed = sub(20, .b(3));
  end
endmodule
