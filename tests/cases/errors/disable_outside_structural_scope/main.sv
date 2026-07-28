// `disable` of a block whose identity no structural scope minted (LRM 9.6.2).
// A class method body is inside no structural scope, so the named block it
// disables has no scope identity to resolve against. It must be reported, not
// crash.
module Top;
  int x;

  class C;
    int v;
    function automatic int f();
      begin : cb
        v = 1;
        disable cb;
      end
      return v;
    endfunction
  endclass

  C c = new();
  initial x = c.f();
endmodule
