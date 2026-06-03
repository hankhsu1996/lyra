module Top;
  int o_val;
  int r_val;

  // Output and ref arguments written after a suspension: the output copy-out
  // and the ref aliasing both survive across the task's delay (LRM 13.3 /
  // 13.5.2), landing the writes when the task resumes at t=5.
  task automatic produce(output int o, ref int r);
    #5;
    o = 42;
    r = r + 1;
  endtask

  initial begin
    o_val = 0;
    r_val = 100;
    produce(o_val, r_val);
  end
endmodule
