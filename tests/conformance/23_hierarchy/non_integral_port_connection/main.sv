// Values of all data types can be passed through ports, because a port
// connection only requires the two sides to be assignment compatible
// (LRM 23.3.3). An input port takes any expression of a compatible data type
// and an output port drives any variable of one (LRM 23.3.3.2), so a string, an
// unpacked array, an enumeration, and a packed structure each cross a port in
// either direction on the same terms an integral value does, and the continuous
// assignment on each side re-evaluates when its source changes.
typedef enum logic [1:0] {
  Idle,
  Busy,
  Done
} state_e;

typedef struct packed {
  logic [3:0] hi;
  logic [3:0] lo;
} pair_t;

module Child(
    input string label, output string echo, input int din [2],
    output int dsum, input state_e mode, output state_e echoed_mode,
    input pair_t pin, output logic [3:0] psum);
  always_comb echo = label;
  always_comb dsum = din[0] + din[1];
  always_comb echoed_mode = mode;
  always_comb psum = pin.hi + pin.lo;
endmodule

module Top;
  string sent;
  string returned;
  int arr [2];
  int sum;
  state_e mode;
  state_e echoed;
  pair_t pin;
  logic [3:0] psum;

  Child u(.label(sent), .echo(returned), .din(arr), .dsum(sum), .mode(mode),
          .echoed_mode(echoed), .pin(pin), .psum(psum));

  initial begin
    sent = "hi";
    arr[0] = 3;
    arr[1] = 4;
    mode = Idle;
    pin.hi = 4'd2;
    pin.lo = 4'd1;
    #5;
    sent = "hello";
    arr[0] = 30;
    mode = Busy;
    pin.hi = 4'd9;
    pin.lo = 4'd5;
  end

  final begin
    if (returned != "hello")
      $fatal(1, "returned was '%s', expected 'hello'", returned);
    if (sum !== 34) $fatal(1, "sum was %0d, expected 34", sum);
    if (echoed !== Busy) $fatal(1, "echoed was %0d, expected Busy", echoed);
    if (psum !== 4'd14) $fatal(1, "psum was %0d, expected 14", psum);
    $display("All checks passed");
  end
endmodule
