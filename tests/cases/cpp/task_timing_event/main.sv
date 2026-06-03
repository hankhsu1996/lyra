module Top;
  int go;
  int done;

  // An event control inside a task suspends the enabling process until the
  // observed variable changes (LRM 13.3 / 9.4.2).
  task automatic wait_go();
    @(go);
    done = 1;
  endtask

  initial begin
    go = 0;
    done = 0;
    wait_go();
  end

  initial begin
    #1;
    go = 1;
  end
endmodule
