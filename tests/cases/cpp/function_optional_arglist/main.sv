module Top;
  int result;

  function void setit;
    result = 42;
  endfunction

  initial begin
    setit;
  end
endmodule
