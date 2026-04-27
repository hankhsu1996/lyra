module Top;
  function automatic int add(int a, int b);
    return a + b;
  endfunction

  task automatic do_thing(int a);
  endtask

  int x;
  initial begin
    x = add(1, 2);
    do_thing(x);
  end
endmodule
