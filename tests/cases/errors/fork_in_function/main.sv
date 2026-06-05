module Test;
  int x;
  function void f();
    fork
      x = 1;
    join_none
  endfunction
  initial f();
endmodule
