module Sib;
  int y;
endmodule

module Deep;
  int z;
endmodule

module MidH;
  Deep deep();
endmodule

module Bank;
  int y;
endmodule

module Reader;
  int a, b, c;
  always_comb a = Top.sib.y;        // climb to Top, down into scalar child `sib`
  always_comb b = Top.mid.deep.z;   // climb to Top, two scalar hops down
  always_comb c = Top.bank[2].y;    // climb to Top, array-index hop down
endmodule

module Top;
  Sib sib();
  MidH mid();
  Bank bank[3]();
  Reader rd();
  initial begin
    sib.y = 11;
    mid.deep.z = 22;
    bank[2].y = 33;
    #1;
    $display("%0d %0d %0d", rd.a, rd.b, rd.c);
    sib.y = 44;
    bank[2].y = 55;
    #1;
    $display("%0d %0d %0d", rd.a, rd.b, rd.c);
  end
endmodule
