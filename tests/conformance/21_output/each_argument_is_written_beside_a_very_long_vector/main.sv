// A display list writes each of its arguments, formatted as its directive
// says (LRM 21.2.1), whatever else the list holds: a string a function makes,
// an expression, the time and the format's own text are written as they are
// beside a vector far longer than a word, before it and after it, as they are
// beside a short one. It holds in a list formatted into a string (LRM 21.3.3),
// one written into a variable, and one whose format string is known only when
// the call runs.
module Top;
  timeunit 1ns;
  timeprecision 1ns;

  localparam int W = 8192;

  logic [2*W-1:0] parts;
  logic [63:0] narrow;
  string runtime_format;
  string beside_long [2];
  string beside_short;
  string written;
  string runtime_written;

  function automatic string name_of(int lane);
    return $sformatf("lane_%0d_named_past_any_short_string", lane);
  endfunction

  initial begin
    beside_long = '{"unwritten", "unwritten"};
    beside_short = "unwritten";
    written = "unwritten";
    runtime_written = "unwritten";
    parts = '0;
    parts[7:0] = 8'hab;
    parts[W +: 8] = 8'hcd;
    narrow = 64'hef;
    runtime_format = "%s|%0d|%0h|%s";
    #5;
    for (int lane = 0; lane < 2; lane++)
      beside_long[lane] = $sformatf("%0t: a=%s n=%0d v=%0h b=%s", $time, name_of(lane), lane + 1,
                                    parts[lane * W +: W], name_of(lane + 2));
    beside_short = $sformatf("%0t: a=%s n=%0d v=%0h b=%s", $time, name_of(0), 1, narrow,
                             name_of(2));
    $sformat(written, "a=%s n=%0d v=%0h b=%s", name_of(1), 2, parts[W +: W], name_of(3));
    runtime_written = $sformatf(runtime_format, name_of(0), 7, parts[0 +: W], name_of(4));
  end

  final begin
    if (beside_long[0] != "5: a=lane_0_named_past_any_short_string n=1 v=ab b=lane_2_named_past_any_short_string")
      $fatal(1, "the first list beside a long vector was '%s'", beside_long[0]);
    if (beside_long[1] != "5: a=lane_1_named_past_any_short_string n=2 v=cd b=lane_3_named_past_any_short_string")
      $fatal(1, "the second list beside a long vector was '%s'", beside_long[1]);
    if (beside_short != "5: a=lane_0_named_past_any_short_string n=1 v=ef b=lane_2_named_past_any_short_string")
      $fatal(1, "the list beside a short vector was '%s'", beside_short);
    if (written != "a=lane_1_named_past_any_short_string n=2 v=cd b=lane_3_named_past_any_short_string")
      $fatal(1, "the list written into a variable was '%s'", written);
    if (runtime_written != "lane_0_named_past_any_short_string|7|ab|lane_4_named_past_any_short_string")
      $fatal(1, "the list with a format known only when it runs was '%s'", runtime_written);
    $display("All checks passed");
  end
endmodule
