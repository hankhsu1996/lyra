// A continuous assignment is evaluated again whenever an operand of its
// right-hand side changes value (LRM 10.3.2), and its own update is such a
// change: the assignment is a process sensitive to the source elements of its
// expression, and its new value is an update event of its own, which schedules
// an evaluation of every process sensitive to the object it updates (LRM 4.5,
// 4.9.1). So an assignment whose expression reads its own target settles,
// evaluation by evaluation, on the value that no longer changes it. Converting
// a gray code to binary this way settles one bit per evaluation. A procedural
// continuous assignment, an `assign` or a `force`, is the same kind of process
// (LRM 4.9.2), and so is the assignment a port connection makes (LRM 4.9.6),
// and one in a program, whose processes run in the reactive region set (LRM
// 24.3.1), settles there the same way. A procedure waiting at an event control
// is woken only by what changes once it waits there (LRM 9.4.2), so one that
// reads what it writes runs once.
module Child (
    input wire [3:0] i
);
endmodule

// The program's own items are not named from outside it, so it hands what it
// converted to the module, before the run ends when its procedure does (LRM
// 24.7).
program Converter (
    input logic [3:0] gray
);
  logic [3:0] binary;
  assign binary = {1'b0, binary[3:1]} ^ gray;
  initial begin
    #5 Top.program_first <= binary;
    #14 Top.program_last <= binary;
  end
endprogram

module Top;
  function automatic logic [3:0] settle_step(logic [3:0] previous,
                                             logic [3:0] gray);
    return {1'b0, previous[3:1]} ^ gray;
  endfunction

  logic [3:0] g = 4'b0011;

  wire [3:0] on_a_net;
  assign on_a_net = {1'b0, on_a_net[3:1]} ^ g;

  wire [3:0] declared = {1'b0, declared[3:1]} ^ g;

  wire [3:0] delayed;
  assign #1 delayed = {1'b0, delayed[3:1]} ^ g;

  logic [3:0] on_a_variable;
  assign on_a_variable = {1'b0, on_a_variable[3:1]} ^ g;

  wire [7:0] wide;
  assign wide[7:4] = 4'b1010;
  assign wide[3:0] = {1'b0, wide[3:1]} ^ g;

  wire [3:0] elements[2];
  assign elements[0] = 4'b0101;
  assign elements[1] = {1'b0, elements[1][3:1]} ^ g;

  wire [3:0] through_a_call;
  assign through_a_call = settle_step(through_a_call, g);

  Child connected (.i({1'b0, connected.i[3:1]} ^ g));

  logic [3:0] program_first = 4'b0101, program_last = 4'b0101;
  Converter converter (.gray(g));

  logic [3:0] procedural;
  wire [3:0] forced;

  logic [3:0] waits_at_an_event_control = 4'b0000;
  always @* waits_at_an_event_control = {1'b0, waits_at_an_event_control[3:1]} ^ g;

  logic [3:0] on_a_net_first, declared_first, delayed_first;
  logic [3:0] on_a_variable_first, wide_first, element_first;
  logic [3:0] through_a_call_first, connected_first;
  logic [3:0] procedural_first, forced_first;

  initial begin
    assign procedural = {1'b0, procedural[3:1]} ^ g;
    force forced = {1'b0, forced[3:1]} ^ g;
    #10;
    on_a_net_first = on_a_net;
    declared_first = declared;
    delayed_first = delayed;
    on_a_variable_first = on_a_variable;
    wide_first = wide[3:0];
    element_first = elements[1];
    through_a_call_first = through_a_call;
    connected_first = connected.i;
    procedural_first = procedural;
    forced_first = forced;

    g = 4'b1000;
    #10;
  end

  final begin
    if (on_a_net_first !== 4'b0010)
      $fatal(1, "on_a_net_first was %b, expected 0010", on_a_net_first);
    if (declared_first !== 4'b0010)
      $fatal(1, "declared_first was %b, expected 0010", declared_first);
    if (delayed_first !== 4'b0010)
      $fatal(1, "delayed_first was %b, expected 0010", delayed_first);
    if (on_a_variable_first !== 4'b0010)
      $fatal(1, "on_a_variable_first was %b, expected 0010",
             on_a_variable_first);
    if (wide_first !== 4'b0010)
      $fatal(1, "wide_first was %b, expected 0010", wide_first);
    if (element_first !== 4'b0010)
      $fatal(1, "element_first was %b, expected 0010", element_first);
    if (through_a_call_first !== 4'b0010)
      $fatal(1, "through_a_call_first was %b, expected 0010",
             through_a_call_first);
    if (connected_first !== 4'b0010)
      $fatal(1, "connected_first was %b, expected 0010", connected_first);
    if (procedural_first !== 4'b0010)
      $fatal(1, "procedural_first was %b, expected 0010", procedural_first);
    if (forced_first !== 4'b0010)
      $fatal(1, "forced_first was %b, expected 0010", forced_first);
    if (program_first !== 4'b0010)
      $fatal(1, "program_first was %b, expected 0010", program_first);

    if (on_a_net !== 4'b1111)
      $fatal(1, "on_a_net was %b, expected 1111", on_a_net);
    if (declared !== 4'b1111)
      $fatal(1, "declared was %b, expected 1111", declared);
    if (delayed !== 4'b1111)
      $fatal(1, "delayed was %b, expected 1111", delayed);
    if (on_a_variable !== 4'b1111)
      $fatal(1, "on_a_variable was %b, expected 1111", on_a_variable);
    if (wide !== 8'b1010_1111)
      $fatal(1, "wide was %b, expected 10101111", wide);
    if (elements[0] !== 4'b0101 || elements[1] !== 4'b1111)
      $fatal(1, "elements were %b %b, expected 0101 1111", elements[0],
             elements[1]);
    if (through_a_call !== 4'b1111)
      $fatal(1, "through_a_call was %b, expected 1111", through_a_call);
    if (connected.i !== 4'b1111)
      $fatal(1, "connected.i was %b, expected 1111", connected.i);
    if (procedural !== 4'b1111)
      $fatal(1, "procedural was %b, expected 1111", procedural);
    if (forced !== 4'b1111)
      $fatal(1, "forced was %b, expected 1111", forced);
    if (program_last !== 4'b1111)
      $fatal(1, "program_last was %b, expected 1111", program_last);

    // The procedure ran once, when g changed, and its own write before it
    // reached the event control again woke nothing: one step of converting
    // 1000 from 0000 is 1000, where settling would have reached 1111.
    if (waits_at_an_event_control !== 4'b1000)
      $fatal(1, "waits_at_an_event_control was %b, expected 1000",
             waits_at_an_event_control);
    $display("All checks passed");
  end
endmodule
