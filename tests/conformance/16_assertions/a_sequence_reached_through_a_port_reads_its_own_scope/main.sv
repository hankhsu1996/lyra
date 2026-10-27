// A name a sequence or property body writes that is not one of its formals
// binds in the scope declaring it (LRM 16.8), while the clock comes from where
// the instance is used. So a sequence an interface declares, used through a
// port, reads the signal of the interface instance that port is bound to, and
// two ports bound to two instances read two signals.
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

module Top;
  logic clk = 0;
  I high(clk);
  I low(clk);
  M m(high, low);

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
    $display("All checks passed");
  end
endmodule
