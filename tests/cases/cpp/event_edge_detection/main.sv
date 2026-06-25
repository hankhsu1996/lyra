// LRM 9.4.2 Table 9-2 edge classification on a single-bit signal. Each waiter
// arms on one edge form; the driver drives the matching (and a non-matching)
// transition at t=5 so each assertion isolates one row of the table:
//   posedge 0->1, negedge 1->0, `edge` on both transitions (count 2),
//   posedge must not wake on a 1->0 transition (wrong_wake stays 0),
//   and the 4-state corners 0->x / x->1 (posedge) and 1->x / x->0 (negedge).
// Declaration initializers set the pre-edge value without generating an edge.
module Top;
  logic clk_p = 0;
  logic clk_n = 1;
  logic clk_w = 1;
  logic clk_both = 0;
  logic clk_0x = 0;
  logic clk_x1 = 1'bx;
  logic clk_1x = 1;
  logic clk_x0 = 1'bx;

  int pos_done = 0;
  int neg_done = 0;
  int wrong_wake = 0;
  int both_count = 0;
  int p_0x = 0;
  int p_x1 = 0;
  int n_1x = 0;
  int n_x0 = 0;

  initial begin
    @(posedge clk_p);
    pos_done = 1;
  end
  initial begin
    @(negedge clk_n);
    neg_done = 1;
  end
  initial begin
    @(posedge clk_w);
    wrong_wake = 1;
  end
  initial begin
    @(edge clk_both);
    both_count = both_count + 1;
    @(edge clk_both);
    both_count = both_count + 1;
  end
  initial begin
    @(posedge clk_0x);
    p_0x = 1;
  end
  initial begin
    @(posedge clk_x1);
    p_x1 = 1;
  end
  initial begin
    @(negedge clk_1x);
    n_1x = 1;
  end
  initial begin
    @(negedge clk_x0);
    n_x0 = 1;
  end

  initial begin
    #5;
    clk_p = 1;
    clk_n = 0;
    clk_w = 0;
    clk_both = 1;
    clk_0x = 1'bx;
    clk_x1 = 1'b1;
    clk_1x = 1'bx;
    clk_x0 = 1'b0;
    #5;
    clk_both = 0;
  end
endmodule
