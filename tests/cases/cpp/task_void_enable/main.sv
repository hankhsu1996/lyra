module Top;
  int counter;

  task noop;
  endtask

  task tick();
    counter = counter + 1;
  endtask

  initial begin
    counter = 0;
    noop;
    tick();
    tick();
    tick();
  end
endmodule
