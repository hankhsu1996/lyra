module Top;
  int result;

  function int get_value();
    return 42;
  endfunction

  initial begin
    result = get_value();
  end
endmodule
