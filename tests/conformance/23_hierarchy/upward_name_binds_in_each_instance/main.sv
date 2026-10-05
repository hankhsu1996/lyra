// An upward name is resolved in each instance of the module that writes it,
// by searching the scopes that instance was instantiated in (LRM 23.8). So two
// instances of one module reach different declarations through the same name:
// another copy of the same variable, a variable of another width, a
// declaration of another module altogether, or a function answering with a
// value of another width. A name that starts at a top-level module reaches the
// same declaration from every instance (LRM 23.6) -- including one that names
// a single instance of the module writing it, which is that instance's own
// declaration in one instance and another's in every other, for a call and a
// disable as for a read.
module Ticker;
  int calls = 0;
  int finished = 0;

  function void bump();
    calls++;
  endfunction

  initial begin : waiter
    #5;
    finished = 1;
  end

  initial begin
    #1;
    Top.first_ticker.bump();
    disable Top.first_ticker.waiter;
  end
endmodule

module Counter;
  initial Holder.hits += Top.step;
endmodule

module Holder;
  int hits = 0;
  Counter c1(), c2();
endmodule

module WidthReader;
  int bits = -1;
  int value = -1;
  initial begin
    bits = $bits(Wide.v);
    value = Wide.v;
  end
endmodule

module Wide #(parameter int W = 2);
  logic [W-1:0] v = '1;
  WidthReader r();
endmodule

module DefinitionReader;
  int bits = -1;
  int value = -1;
  initial begin
    bits = $bits(u.x);
    value = u.x;
  end
endmodule

module OtherA;
  int x = 7;
endmodule

module OtherB;
  logic [3:0] x = 4'd9;
endmodule

module HostA;
  OtherA u();
  DefinitionReader r();
endmodule

module HostB;
  OtherB u();
  DefinitionReader r();
endmodule

module Caller;
  int bits = -1;
  int value = -1;
  initial begin
    bits = $bits(Answer.f());
    value = Answer.f();
  end
endmodule

module Answer #(parameter int W = 2);
  function logic [W-1:0] f();
    return '1;
  endfunction
  Caller c();
endmodule

module Top;
  int step = 1;

  Holder h1(), h2();
  Wide #(.W(4)) w4();
  Wide #(.W(12)) w12();
  HostA ha();
  HostB hb();
  Answer #(.W(2)) a2();
  Answer #(.W(5)) a5();
  Ticker first_ticker();
  Ticker second_ticker();

  final begin
    if (first_ticker.calls !== 2 || second_ticker.calls !== 0)
      $fatal(1, "a call named on one instance landed %0d and %0d, expected 2 and 0",
             first_ticker.calls, second_ticker.calls);
    if (first_ticker.finished !== 0 || second_ticker.finished !== 1)
      $fatal(1, "a disable named on one instance left %0d and %0d, expected 0 and 1",
             first_ticker.finished, second_ticker.finished);
    if (h1.hits !== 2) $fatal(1, "h1.hits was %0d, expected 2", h1.hits);
    if (h2.hits !== 2) $fatal(1, "h2.hits was %0d, expected 2", h2.hits);
    if (w4.r.bits !== 4) $fatal(1, "w4.r.bits was %0d, expected 4", w4.r.bits);
    if (w4.r.value !== 15)
      $fatal(1, "w4.r.value was %0d, expected 15", w4.r.value);
    if (w12.r.bits !== 12)
      $fatal(1, "w12.r.bits was %0d, expected 12", w12.r.bits);
    if (w12.r.value !== 4095)
      $fatal(1, "w12.r.value was %0d, expected 4095", w12.r.value);
    if (ha.r.bits !== 32) $fatal(1, "ha.r.bits was %0d, expected 32", ha.r.bits);
    if (ha.r.value !== 7) $fatal(1, "ha.r.value was %0d, expected 7", ha.r.value);
    if (hb.r.bits !== 4) $fatal(1, "hb.r.bits was %0d, expected 4", hb.r.bits);
    if (hb.r.value !== 9) $fatal(1, "hb.r.value was %0d, expected 9", hb.r.value);
    if (a2.c.bits !== 2) $fatal(1, "a2.c.bits was %0d, expected 2", a2.c.bits);
    if (a2.c.value !== 3) $fatal(1, "a2.c.value was %0d, expected 3", a2.c.value);
    if (a5.c.bits !== 5) $fatal(1, "a5.c.bits was %0d, expected 5", a5.c.bits);
    if (a5.c.value !== 31)
      $fatal(1, "a5.c.value was %0d, expected 31", a5.c.value);
    $display("All checks passed");
  end
endmodule
