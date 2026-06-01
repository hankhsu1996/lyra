module Top;
  int counter;

  function void increment();
    counter = counter + 1;
  endfunction

  initial begin
    counter = 0;
    increment();
    increment();
    increment();
  end
endmodule
