// LRM 20.7.1: naming a variable-sized dimension below the top is an error,
// since each outer element carries its own extent. A run-time dimension index
// could land on such a dimension, so the query over this operand shape is
// rejected rather than half-served.
module Top;
  int a[3][];
  int d;
  int rt;
  initial begin
    a[0] = new[4];
    d = 2;
    rt = $size(a, d);
  end
endmodule
