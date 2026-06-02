module Top;
  int result;

  function automatic int doubled(input int n);
    return n << 1;
  endfunction

  task add_doubled(input int n, inout int acc);
    acc = acc + doubled(n);
  endtask

  task accumulate(output int total);
    total = 0;
    add_doubled(3, total);
    add_doubled(5, total);
  endtask

  initial begin
    result = 99;
    accumulate(result);
  end
endmodule
