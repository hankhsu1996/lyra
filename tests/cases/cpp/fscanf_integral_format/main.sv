module Top;
  // LRM 21.3.4.3: $fscanf format may be an expression of integral type or
  // string. This case exercises the format-side conversion lift for the
  // file scan path, matching what sscanf_integral_format covers for the
  // string scan path. Format bytes spell "%d %h" (5 bytes -> 40 bits).
  int fd, code;
  logic [39:0] fmt = 40'h25_64_20_25_68;     // "%d %h"
  int a;
  logic [15:0] b;

  initial begin
    fd = $fopen("fscanf_fmt.txt", "w");
    $fdisplay(fd, "99 cafe");
    $fclose(fd);

    fd = $fopen("fscanf_fmt.txt", "r");
    code = $fscanf(fd, fmt, a, b);
    $fclose(fd);
  end
endmodule
