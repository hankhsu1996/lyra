// A bit-select or a part-select on the left of a blocking assignment stores
// into exactly the positions it names and leaves every other bit of the
// variable as it was. That holds of a variable a subroutine declares as it
// does of one a module declares -- automatic or static, read back at once or
// after the subroutine has waited -- however wide the variable is and through
// however many selects the positions were reached. A select whose address
// holds x or z, or lies outside the vector, stores nothing, and a part-select
// lying partly outside stores only the bits inside (LRM 10.4, 11.5.1, 11.5.2,
// 6.21). A subroutine that is disabled while it waits is left where it
// stands, whatever it holds, and the statement after its call is not reached
// (LRM 9.6.2).
module Top;
  typedef logic [3:0][7:0] one_word_t;
  typedef logic [19:0][7:0] three_words_t;
  typedef logic [1023:0][7:0] wide_t;

  typedef struct packed {
    logic [7:0] tag;
    logic [1:0][7:0] body;
    logic [3:0] hi;
    bit [3:0] two;
  } small_record_t;

  typedef struct packed {
    logic [7:0] tag;
    wide_t body;
    logic [3:0] hi;
    bit [3:0] two;
  } wide_record_t;

  one_word_t element_32;
  logic [31:0] constant_part_32;
  logic [31:0] variable_part_32;
  one_word_t part_of_part_32;
  one_word_t unknown_position_32;
  logic [31:0] outside_32;
  small_record_t member_32;
  one_word_t waited_32;

  three_words_t element_160;
  logic [159:0] variable_part_160;
  three_words_t part_of_part_160;
  logic [159:0] outside_160;
  three_words_t static_first;
  three_words_t static_second;

  wide_t element_wide;
  logic [8191:0] variable_part_wide;
  wide_t part_of_part_wide;
  logic [8191:0] outside_wide;
  wide_record_t member_wide;
  wide_t waited_wide;
  bit reached_past_the_disabled_call;
  bit finished_after_the_disable;

  function automatic one_word_t write_element_32(int i);
    one_word_t m;
    m = {4{8'hA5}};
    m[i] = 8'h3C;
    return m;
  endfunction

  function automatic logic [31:0] write_constant_part_32();
    logic [31:0] f;
    f = {4{8'hA5}};
    f[15:8] = 8'h3C;
    return f;
  endfunction

  function automatic logic [31:0] write_variable_part_32(int p);
    logic [31:0] f;
    f = {4{8'hA5}};
    f[p+:8] = 8'hC3;
    return f;
  endfunction

  // The write into element 2 starts two bits below that element's top, so
  // only those two bits are inside the element it selects.
  function automatic one_word_t write_part_of_part_32(int j, int k, int o);
    one_word_t m;
    m = {4{8'hA5}};
    m[1][5:2] = 4'b0110;
    m[0][j] = 1'b0;
    m[3][k+:2] = 2'b11;
    m[2][o+:4] = 4'b1111;
    return m;
  endfunction

  function automatic one_word_t write_at_unknown_position_32(
      logic [3:0] ix, logic [3:0] iz);
    one_word_t m;
    m = {4{8'hA5}};
    m[ix] = 8'h00;
    m[iz] = 8'h00;
    m[ix][3] = 1'b1;
    m[1][ix] = 1'b1;
    m[2][iz+:4] = 4'h0;
    return m;
  endfunction

  // `past` and `short_of` lie wholly outside the element they are taken in;
  // `top` and `under` put half of the byte written outside the vector.
  function automatic logic [31:0] write_outside_32(
      int above, int below, int past, int short_of, int top, int under);
    one_word_t m;
    logic [31:0] f;
    m = {4{8'hA5}};
    m[above] = 8'h00;
    m[below] = 8'h00;
    m[1][past+:4] = 4'h0;
    m[1][short_of+:4] = 4'h0;
    f = m;
    f[top+:8] = 8'hC3;
    f[under+:8] = 8'hC3;
    return f;
  endfunction

  function automatic small_record_t write_member_32(int i);
    small_record_t r;
    r = {4{8'hA5}};
    r.tag = 8'h3C;
    r.body[i] = 8'h7E;
    r.hi[2] = 1'b1;
    r.two[1] = 1'b1;
    return r;
  endfunction

  task automatic write_across_a_wait_32(input int i, output one_word_t now);
    one_word_t m;
    m = {4{8'hA5}};
    m[i] = 8'h3C;
    #1;
    m[i+1][3:0] = 4'hF;
    #1;
    m[i-1][7] = 1'b0;
    now = m;
  endtask

  function automatic three_words_t write_element_160(int i);
    three_words_t m;
    m = {20{8'hA5}};
    m[i] = 8'h3C;
    return m;
  endfunction

  function automatic logic [159:0] write_variable_part_160(int p);
    logic [159:0] f;
    f = {20{8'hA5}};
    f[p+:8] = 8'hC3;
    return f;
  endfunction

  function automatic three_words_t write_part_of_part_160(
      int i, int j, int k, int o, int above);
    three_words_t m;
    m = {20{8'hA5}};
    m[9][5:2] = 4'b0110;
    m[i][j] = 1'b0;
    m[18][k+:2] = 2'b11;
    m[8][o+:4] = 4'b1111;
    m[above] = 8'h00;
    m[9][above+:4] = 4'h0;
    return m;
  endfunction

  function automatic logic [159:0] write_outside_160(int top, int below);
    logic [159:0] f;
    f = {20{8'hA5}};
    f[top+:8] = 8'hC3;
    f[below+:8] = 8'hC3;
    return f;
  endfunction

  task static stamp(input int i, output three_words_t now);
    static three_words_t held = {20{8'hA5}};
    held[i] = 8'h3C;
    now = held;
  endtask

  function automatic wide_t write_element_wide(int i);
    wide_t m;
    m = {1024{8'hA5}};
    m[i] = 8'h3C;
    return m;
  endfunction

  function automatic logic [8191:0] write_variable_part_wide(int p);
    logic [8191:0] f;
    f = {1024{8'hA5}};
    f[p+:8] = 8'hC3;
    return f;
  endfunction

  function automatic wide_t write_part_of_part_wide(
      int i, int j, int k, int o, int above, logic [3:0] ix);
    wide_t m;
    m = {1024{8'hA5}};
    m[700][5:2] = 4'b0110;
    m[i][j] = 1'b0;
    m[1022][k+:2] = 2'b11;
    m[699][o+:4] = 4'b1111;
    m[above] = 8'h00;
    m[700][above+:4] = 4'h0;
    m[ix] = 8'h00;
    m[700][ix] = 1'b1;
    m[ix][2] = 1'b1;
    return m;
  endfunction

  function automatic logic [8191:0] write_outside_wide(int top, int below);
    logic [8191:0] f;
    f = {1024{8'hA5}};
    f[top+:8] = 8'hC3;
    f[below+:8] = 8'hC3;
    return f;
  endfunction

  function automatic wide_record_t write_member_wide(int i);
    wide_record_t r;
    r = {1026{8'hA5}};
    r.tag = 8'h3C;
    r.body[i] = 8'h7E;
    r.hi[2] = 1'b1;
    r.two[1] = 1'b1;
    return r;
  endfunction

  task automatic write_across_a_wait_wide(input int i, output wide_t now);
    wide_t m;
    m = {1024{8'hA5}};
    m[i] = 8'h3C;
    #1;
    m[i+1][3:0] = 4'hF;
    #1;
    m[i-1][7] = 1'b0;
    now = m;
  endtask

  task automatic wait_holding_wide(input int i);
    wide_t m;
    m = {1024{8'hA5}};
    m[i] = 8'h3C;
    #10;
    m[i+1] = 8'h00;
  endtask

  initial begin : holder
    wait_holding_wide(700);
    reached_past_the_disabled_call = 1'b1;
  end

  initial begin
    #5 disable holder;
    #10 finished_after_the_disable = 1'b1;
  end

  initial begin
    element_32 = write_element_32(2);
    constant_part_32 = write_constant_part_32();
    variable_part_32 = write_variable_part_32(12);
    part_of_part_32 = write_part_of_part_32(0, 3, 6);
    unknown_position_32 = write_at_unknown_position_32(4'bxxxx, 4'b00z1);
    outside_32 = write_outside_32(4, -1, 8, -8, 28, -4);
    member_32 = write_member_32(1);

    element_160 = write_element_160(8);
    variable_part_160 = write_variable_part_160(60);
    part_of_part_160 = write_part_of_part_160(7, 0, 3, 6, 20);
    outside_160 = write_outside_160(156, -4);
    stamp(8, static_first);
    stamp(11, static_second);

    element_wide = write_element_wide(700);
    variable_part_wide = write_variable_part_wide(5604);
    part_of_part_wide = write_part_of_part_wide(0, 0, 3, 6, 1024, 4'bx01x);
    outside_wide = write_outside_wide(8188, -4);
    member_wide = write_member_wide(700);

    write_across_a_wait_32(2, waited_32);
    write_across_a_wait_wide(700, waited_wide);
  end

  final begin
    if (element_32 !== 32'hA53CA5A5)
      $fatal(1, "an element of one word was %h, expected a53ca5a5", element_32);
    if (constant_part_32 !== 32'hA5A53CA5)
      $fatal(1, "[15:8] of one word was %h, expected a5a53ca5",
             constant_part_32);
    if (variable_part_32 !== 32'hA5AC35A5)
      $fatal(1, "[p+:8] of one word was %h, expected a5ac35a5",
             variable_part_32);
    if (part_of_part_32 !== 32'hBDE599A4)
      $fatal(1, "parts of elements of one word were %h, expected bde599a4",
             part_of_part_32);
    if (unknown_position_32 !== 32'hA5A5A5A5)
      $fatal(1, "writes at unknown positions gave %h, expected a5a5a5a5",
             unknown_position_32);
    if (outside_32 !== 32'h35A5A5AC)
      $fatal(1, "writes outside one word gave %h, expected 35a5a5ac",
             outside_32);
    if (member_32 !== 32'h3C7EA5E7)
      $fatal(1, "members of one word were %h, expected 3c7ea5e7", member_32);
    if (waited_32 !== 32'hAF3C25A5)
      $fatal(1, "one word written across waits was %h, expected af3c25a5",
             waited_32);

    if (element_160 !== 160'hA5A5A5A5A5A5A5A5A5A5A53CA5A5A5A5A5A5A5A5)
      $fatal(1, "an element of three words was %h", element_160);
    if (variable_part_160 !== 160'hA5A5A5A5A5A5A5A5A5A5A5AC35A5A5A5A5A5A5A5)
      $fatal(1, "[p+:8] across two words was %h", variable_part_160);
    if (part_of_part_160 !== 160'hA5BDA5A5A5A5A5A5A5A599E5A4A5A5A5A5A5A5A5)
      $fatal(1, "parts of elements of three words were %h", part_of_part_160);
    if (outside_160 !== 160'h35A5A5A5A5A5A5A5A5A5A5A5A5A5A5A5A5A5A5AC)
      $fatal(1, "writes partly outside three words gave %h", outside_160);
    if (static_first !== 160'hA5A5A5A5A5A5A5A5A5A5A53CA5A5A5A5A5A5A5A5)
      $fatal(1, "a static variable after one write was %h", static_first);
    if (static_second !== 160'hA5A5A5A5A5A5A5A53CA5A53CA5A5A5A5A5A5A5A5)
      $fatal(1, "a static variable after two writes was %h", static_second);

    if (element_wide[1023:701] !== {323{8'hA5}})
      $fatal(1, "an element write changed an element above it");
    if (element_wide[700] !== 8'h3C)
      $fatal(1, "element 700 was %h, expected 3c", element_wide[700]);
    if (element_wide[699:0] !== {700{8'hA5}})
      $fatal(1, "an element write changed an element below it");

    if (variable_part_wide[8191:5616] !== {322{8'hA5}})
      $fatal(1, "a part-select write changed bits above it");
    if (variable_part_wide[5615:5600] !== 16'hAC35)
      $fatal(1, "[p+:8] of a wide vector gave %h, expected ac35",
             variable_part_wide[5615:5600]);
    if (variable_part_wide[5599:0] !== {700{8'hA5}})
      $fatal(1, "a part-select write changed bits below it");

    if (part_of_part_wide[1023] !== 8'hA5)
      $fatal(1, "element 1023 was %h, expected a5", part_of_part_wide[1023]);
    if (part_of_part_wide[1022] !== 8'hBD)
      $fatal(1, "element 1022 was %h, expected bd", part_of_part_wide[1022]);
    if (part_of_part_wide[1021:701] !== {321{8'hA5}})
      $fatal(1, "a write into an element changed elements 1021 to 701");
    if (part_of_part_wide[700] !== 8'h99)
      $fatal(1, "element 700 was %h, expected 99", part_of_part_wide[700]);
    if (part_of_part_wide[699] !== 8'hE5)
      $fatal(1, "element 699 was %h, expected e5", part_of_part_wide[699]);
    if (part_of_part_wide[698:1] !== {698{8'hA5}})
      $fatal(1, "a write into an element changed elements 698 to 1");
    if (part_of_part_wide[0] !== 8'hA4)
      $fatal(1, "element 0 was %h, expected a4", part_of_part_wide[0]);

    if (outside_wide[8191:8184] !== 8'h35)
      $fatal(1, "the top byte was %h, expected 35", outside_wide[8191:8184]);
    if (outside_wide[8183:8] !== {1022{8'hA5}})
      $fatal(1, "a write partly outside changed bits it does not name");
    if (outside_wide[7:0] !== 8'hAC)
      $fatal(1, "the bottom byte was %h, expected ac", outside_wide[7:0]);

    if (member_wide.tag !== 8'h3C)
      $fatal(1, "tag was %h, expected 3c", member_wide.tag);
    if (member_wide.body[1023:701] !== {323{8'hA5}})
      $fatal(1, "a write into a member's element changed elements above it");
    if (member_wide.body[700] !== 8'h7E)
      $fatal(1, "body[700] was %h, expected 7e", member_wide.body[700]);
    if (member_wide.body[699:0] !== {700{8'hA5}})
      $fatal(1, "a write into a member's element changed elements below it");
    if (member_wide.hi !== 4'hE)
      $fatal(1, "hi was %h, expected e", member_wide.hi);
    if (member_wide.two !== 4'h7)
      $fatal(1, "two was %h, expected 7", member_wide.two);

    if (waited_wide[1023:702] !== {322{8'hA5}})
      $fatal(1, "writes across waits changed elements above them");
    if (waited_wide[701] !== 8'hAF)
      $fatal(1, "element 701 was %h, expected af", waited_wide[701]);
    if (waited_wide[700] !== 8'h3C)
      $fatal(1, "element 700 was %h, expected 3c", waited_wide[700]);
    if (waited_wide[699] !== 8'h25)
      $fatal(1, "element 699 was %h, expected 25", waited_wide[699]);
    if (waited_wide[698:0] !== {699{8'hA5}})
      $fatal(1, "writes across waits changed elements below them");
    if (reached_past_the_disabled_call !== 1'b0)
      $fatal(1, "the statement after a disabled call was reached");
    if (finished_after_the_disable !== 1'b1)
      $fatal(1, "the process that disabled another did not go on");
    $display("All checks passed");
  end
endmodule
