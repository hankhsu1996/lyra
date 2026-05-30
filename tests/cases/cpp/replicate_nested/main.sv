module Top;
  bit [23:0] nested_replication;

  initial begin
    nested_replication = {2{{3{4'hA}}}};
  end
endmodule
