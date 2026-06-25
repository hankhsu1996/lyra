// LRM 9.4.2 / 11.5.1: a value-change wait @(sel) projects onto exactly the
// bits the select names, honoring the vector's declared direction. Each waiter
// below watches a different select shape; for each, the t=5 write lands outside
// the projection (must not wake) and the t=10 write lands inside (wakes). Each
// waiter records $time, so a woke time of 10 proves the projection both
// excludes the out-of-range write and includes the in-range one:
//   bus[3]   1D descending  -> storage bit 3   (bit 5 changes at t=5, not bit 3)
//   md[1]    multi-dim       -> storage bits 15:8 (lower element md[0] at t=5)
//   asc[1:2] ascending [0:7] -> storage bits 5,6 (asc[5] is bit 2 at t=5)
//   neg[0]   negative [-1:6] -> storage bit 6   (neg[6] is bit 0 at t=5)
// bus additionally re-arms: a second in-range change at t=15 wakes it again.
module Top;
  logic [7:0] bus = 8'b0000_1000;
  logic [1:0][7:0] md = '0;
  logic [0:7] asc = '0;
  logic [-1:6] neg = '0;

  time bus_woke = 0;
  int bus_wakes = 0;
  time md_woke = 0;
  time asc_woke = 0;
  time neg_woke = 0;

  initial begin
    @(bus[3]);
    bus_woke = $time;
    bus_wakes = bus_wakes + 1;
    @(bus[3]);
    bus_wakes = bus_wakes + 1;
  end
  initial begin
    @(md[1]);
    md_woke = $time;
  end
  initial begin
    @(asc[1:2]);
    asc_woke = $time;
  end
  initial begin
    @(neg[0]);
    neg_woke = $time;
  end

  initial begin
    #5;
    bus = 8'b0010_1000;
    md[0] = 8'hFF;
    asc[5] = 1'b1;
    neg[6] = 1'b1;
    #5;
    bus[3] = 0;
    md[1] = 8'hAA;
    asc[2] = 1'b1;
    neg[0] = 1'b1;
    #5;
    bus[3] = 1;
  end
endmodule
