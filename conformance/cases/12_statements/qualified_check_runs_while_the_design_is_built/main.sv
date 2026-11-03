// @reports-none: violation
//
// A function holding a unique, unique0 or priority statement is an ordinary
// function, and a constant function call of it may stand wherever an
// elaboration-time constant may (LRM 13.4.3): a localparam written from a
// parameter the instantiation hands in, a generate condition. Each call
// checks its statement as any execution does (LRM 12.4.2, 12.5.3), and an
// execution with exactly one arm matching reports nothing, wherever and
// whenever it runs -- while the design is built, in a variable's initializer
// (LRM 10.5), in a class's static property (LRM 8.9), or in a process -- and
// whether a package or the instantiated module itself declares the function.
package P;
  function automatic int UniqueCase(int n);
    int w = 0;
    unique case (n)
      2: w = 20;
      3: w = 30;
    endcase
    return w;
  endfunction

  function automatic int Unique0Case(int n);
    int w = 0;
    unique0 case (n)
      2: w = 21;
      3: w = 31;
    endcase
    return w;
  endfunction

  function automatic int PriorityCase(int n);
    int w = 0;
    priority case (n)
      2: w = 22;
      3: w = 32;
    endcase
    return w;
  endfunction

  function automatic int UniqueIf(int n);
    int w = 0;
    unique if (n == 2) w = 23;
    else if (n == 3) w = 33;
    return w;
  endfunction

  function automatic int All(int n);
    return UniqueCase(n) + Unique0Case(n) + PriorityCase(n) + UniqueIf(n);
  endfunction
endpackage

class Held;
  static int two = P::All(2);
endclass

module Leaf #(parameter int N = 2)
    (output int computed, output int own, output int chosen);
  function automatic int Own(int n);
    int w = 0;
    unique case (n)
      2: w = 4;
      3: w = 6;
    endcase
    return w;
  endfunction

  localparam int L = P::All(N);
  localparam int O = Own(N);
  assign computed = L;
  assign own = O;
  if (P::UniqueCase(N) == 30) begin : three
    assign chosen = 3;
  end else begin : other
    assign chosen = N;
  end
endmodule

module Top;
  int from_initializer = P::All(3);
  int computed2, computed3, own2, own3, chosen2, chosen3;

  Leaf #(.N(2)) two (.computed(computed2), .own(own2), .chosen(chosen2));
  Leaf #(.N(3)) three (.computed(computed3), .own(own3), .chosen(chosen3));

  initial begin
    #1;
    if (computed2 !== 86) $fatal(1, "N=2 localparam was %0d, expected 86", computed2);
    if (computed3 !== 126) $fatal(1, "N=3 localparam was %0d, expected 126", computed3);
    if (own2 !== 4) $fatal(1, "N=2 own localparam was %0d, expected 4", own2);
    if (own3 !== 6) $fatal(1, "N=3 own localparam was %0d, expected 6", own3);
    if (chosen2 !== 2) $fatal(1, "N=2 generate chose %0d, expected 2", chosen2);
    if (chosen3 !== 3) $fatal(1, "N=3 generate chose %0d, expected 3", chosen3);
    if (from_initializer !== 126)
      $fatal(1, "initializer was %0d, expected 126", from_initializer);
    if (Held::two !== 86) $fatal(1, "class static was %0d, expected 86", Held::two);
    if (P::All(2) !== 86) $fatal(1, "process call was %0d, expected 86", P::All(2));
    $display("All checks passed");
  end
endmodule
