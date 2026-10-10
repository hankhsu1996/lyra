// @error: too few arguments
//
// A name not found where it is written is searched for upward from each
// instance of the module writing it (LRM 23.8), so one call resolves to a
// different function under each parent. Under `B` the function it reaches has
// an argument with no default that the call leaves out, which shall be a
// compiler error (LRM 13.5.3). The instance in error is written second: a tool
// that checks the call once, for the first instance, misses it.
module NoArg;
  function automatic int f();
    return 1;
  endfunction
endmodule

module OneArg;
  function automatic int f(int a);
    return a;
  endfunction
endmodule

module Caller;
  int got;
  initial got = cfg.f();
endmodule

module A;
  NoArg cfg ();
  Caller c ();
endmodule

module B;
  OneArg cfg ();
  Caller c ();
endmodule

module Top;
  A a ();
  B b ();
endmodule
