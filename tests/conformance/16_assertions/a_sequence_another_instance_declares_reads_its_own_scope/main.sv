// A name a sequence or property body writes that is not one of its formals
// binds in the scope declaring it (LRM 16.8), while the clock comes from where
// the instance is used. So a sequence an interface declares reads the signal
// of the interface instance it was reached in, whichever way it was reached:
// through a port, the instance the port is bound to (LRM 25.3), and by a
// hierarchical name, the instance the name leads to (LRM 23.6). Two ports or
// two names reaching two instances read two signals.
interface I(input logic clk);
  logic a;
  sequence s;
    a;
  endsequence
  property holds;
    s;
  endproperty
endinterface

module M(I p, I q);
  int passes_p = 0;
  int fails_p = 0;
  int passes_q = 0;
  int fails_q = 0;
  int property_passes = 0;
  assert property (@(posedge p.clk) p.s) passes_p++; else fails_p++;
  assert property (@(posedge q.clk) q.s) passes_q++; else fails_q++;
  assert property (@(posedge p.clk) p.holds) property_passes++;
endmodule

module Named;
  int passes_high = 0;
  int fails_low = 0;
  int property_passes = 0;
  assert property (@(posedge Top.clk) Top.high.s) passes_high++;
  assert property (@(posedge Top.clk) Top.low.s) else fails_low++;
  assert property (@(posedge Top.clk) Top.high.holds) property_passes++;
endmodule

module Top;
  logic clk = 0;
  I high(clk);
  I low(clk);
  M m(high, low);
  Named n();

  initial begin
    high.a = 1;
    low.a = 0;
    repeat (2) begin
      #1 clk = 1;
      #1 clk = 0;
    end
    #1;
    if (m.passes_p !== 2 || m.fails_p !== 0)
      $fatal(1, "through p: %0d passed, %0d failed", m.passes_p, m.fails_p);
    if (m.passes_q !== 0 || m.fails_q !== 2)
      $fatal(1, "through q: %0d passed, %0d failed", m.passes_q, m.fails_q);
    if (m.property_passes !== 2)
      $fatal(1, "the property passed %0d times", m.property_passes);
    if (n.passes_high !== 2)
      $fatal(1, "by name, high passed %0d times", n.passes_high);
    if (n.fails_low !== 2)
      $fatal(1, "by name, low failed %0d times", n.fails_low);
    if (n.property_passes !== 2)
      $fatal(1, "by name, the property passed %0d times", n.property_passes);
    $display("All checks passed");
  end
endmodule
