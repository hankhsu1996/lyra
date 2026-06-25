// Event-control integration. One waiter blocks on an event list (LRM 9.4.2.3)
// mixing a whole-signal posedge, an async-reset negedge, and a bit-select
// negedge; each member fires independently as the driver drives them at t=5 /
// t=10 / t=15, so list_wakes reaches 3. Two further waiters share the same
// posedge and both wake (w1, w2). A third waiter performs a non-blocking
// assignment after waking, which commits in the NBA region (nba_q == 42).
module Top;
  bit clk = 0;
  logic rst_n = 1;
  logic [7:0] bus = 8'b0000_1000;

  int list_wakes = 0;
  int w1 = 0;
  int w2 = 0;
  int nba_q = 0;
  int d = 42;

  initial begin
    @(posedge clk or negedge rst_n or negedge bus[3]);
    list_wakes = list_wakes + 1;
    @(posedge clk or negedge rst_n or negedge bus[3]);
    list_wakes = list_wakes + 1;
    @(posedge clk or negedge rst_n or negedge bus[3]);
    list_wakes = list_wakes + 1;
  end
  initial begin
    @(posedge clk);
    w1 = 1;
  end
  initial begin
    @(posedge clk);
    w2 = 2;
  end
  initial begin
    @(posedge clk);
    nba_q <= d;
  end

  initial begin
    #5 rst_n = 0;
    #5 bus[3] = 0;
    #5 clk = 1;
  end
endmodule
