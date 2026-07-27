module Top;
  // LRM 35.5.6.1: one imported function serves actuals of any size and range,
  // because the unsized dimension takes its extent from the actual at the call.
  import "DPI-C" function int weigh(input byte data[]);
  import "DPI-C" function int describe(input byte data[]);
  import "DPI-C" function void fill(output byte data[]);
  import "DPI-C" function void bump(inout logic [15:0] w[]);
  import "DPI-C" function int trace(input int m[][]);
  import "DPI-C" function int width_of(input logic [] v);
  import "DPI-C" function int addressable(input bit [31:0] wide[]);

  byte few[4];
  byte many[3:1];
  byte out_buf[0:2];
  logic [15:0] words[0:2];
  int grid[2][3];
  logic [11:0] narrow;
  bit [31:0] wide[0:1];

  initial begin
    few[0] = 1;
    few[1] = 2;
    few[2] = 3;
    few[3] = 4;
    // An index-weighted sum, so a wrong index-to-element mapping shows up.
    $display("few=%0d", weigh(few));

    many[3] = 10;
    many[2] = 20;
    many[1] = 30;
    $display("many=%0d", weigh(many));

    // The same import over a descending range reports the declared bounds
    // rather than a normalized one (LRM Annex H.7.5, H.7.6).
    $display("shape=%0d", describe(many));

    fill(out_buf);
    $display("filled=%0d %0d %0d", out_buf[0], out_buf[1], out_buf[2]);

    words[0] = 16'h0001;
    words[1] = 16'h000x;
    words[2] = 16'h0003;
    bump(words);
    $display("words=%h %h %h", words[0], words[1], words[2]);

    grid[0][0] = 1;
    grid[0][1] = 2;
    grid[0][2] = 3;
    grid[1][0] = 4;
    grid[1][1] = 5;
    grid[1][2] = 6;
    $display("grid=%0d", trace(grid));

    narrow = 12'hABC;
    $display("width=%0d", width_of(narrow));

    wide[0] = 32'h11111111;
    wide[1] = 32'h22222222;
    $display("addressable=%0d", addressable(wide));
  end
endmodule
