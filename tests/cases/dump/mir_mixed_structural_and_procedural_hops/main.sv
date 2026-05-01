module Top;
  int root_x;
  if (1) begin : g
    initial begin
      int local_x;
      begin
        int y;
        y = root_x + local_x;
      end
    end
  end
endmodule
