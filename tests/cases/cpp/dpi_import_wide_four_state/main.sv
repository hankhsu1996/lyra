module Top;
  import "DPI-C" function logic scalar_id(input logic x);
  import "DPI-C" function int count_unknown(input logic [7:0] v);
  import "DPI-C" function void set_zx(output logic [7:0] v);
  import "DPI-C" function void known_not(inout logic [3:0] v);
  import "DPI-C" function int int_add(input integer a, input integer b);
  import "DPI-C" function void low32(input bit [127:0] v, output bit [31:0] r);
  import "DPI-C" function byte b8_sum(input bit [7:0] a, input bit [7:0] b);

  logic s;
  logic [7:0] u;
  logic [3:0] w;
  int n;
  bit [31:0] r;
  byte b;

  initial begin
    s = scalar_id(1'bx);
    $display("scalar=%b", s);

    n = count_unknown(8'b1x0z1x0z);
    $display("unknown=%0d", n);

    set_zx(u);
    $display("out=%b", u);

    w = 4'b10xz;
    known_not(w);
    $display("inout=%b", w);

    n = int_add(32'sd100, 32'sd23);
    $display("intadd=%0d", n);

    low32(128'hdeadbeef, r);
    $display("low32=%h", r);

    b = b8_sum(8'd10, 8'd20);
    $display("b8=%0d", b);
  end
endmodule
