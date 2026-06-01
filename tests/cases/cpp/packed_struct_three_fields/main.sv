module Top;
  typedef struct packed {
    logic [7:0] x;
    logic [7:0] y;
    logic [7:0] z;
  } vec3_t;
  vec3_t v;
  int result_x;
  int result_y;
  int result_z;
  int result_total;
  initial begin
    v.x = 8'h12;
    v.y = 8'h34;
    v.z = 8'h56;
    result_x = v.x;
    result_y = v.y;
    result_z = v.z;
    result_total = v;
  end
endmodule
