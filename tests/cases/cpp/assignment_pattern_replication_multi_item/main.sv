module Top;
  typedef struct packed {
    logic [3:0] a;
    logic [3:0] b;
    logic [3:0] c;
    logic [3:0] d;
  } nibbles_t;
  nibbles_t s;
  int field0;
  int field1;
  int field2;
  int field3;
  initial begin
    s = '{2{4'h1, 4'h2}};
    field0 = s.a;
    field1 = s.b;
    field2 = s.c;
    field3 = s.d;
  end
endmodule
