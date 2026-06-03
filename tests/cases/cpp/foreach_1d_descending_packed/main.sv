module Top;
  bit [3:0] data = 4'b1010;
  int first_idx;
  int last_idx;
  int sum_idx;
  int set_count;

  initial begin
    first_idx = -1;
    last_idx = -1;
    sum_idx = 0;
    set_count = 0;
    foreach (data[i]) begin
      if (first_idx == -1) first_idx = i;
      last_idx = i;
      sum_idx = sum_idx + i;
      if (data[i]) set_count = set_count + 1;
    end
  end
endmodule
