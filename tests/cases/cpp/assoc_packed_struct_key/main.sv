module Top;
  // LRM 7.8.5: a packed struct is an integral index type, so it indexes an
  // associative array by its bit-pattern value with numerical ordering.
  typedef struct packed {
    bit [7:0] hi;
    bit [7:0] lo;
  } Pair;

  int by_struct [Pair];

  int va;
  int vb;
  int n;
  int ex_present;
  int ex_absent;

  initial begin
    Pair a;
    Pair b;
    a = '{hi: 8'd1, lo: 8'd2};
    b = '{hi: 8'd1, lo: 8'd3};
    by_struct[a] = 42;
    by_struct[b] = 43;
    va = by_struct[a];
    vb = by_struct[b];
    n = by_struct.num();
    ex_present = by_struct.exists(a);
    by_struct.delete(a);
    ex_absent = by_struct.exists(a);
  end
endmodule
