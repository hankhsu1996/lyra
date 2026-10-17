// A program's code is scheduled in the reactive region set: its initial
// procedures and every resumption of a thread they start run in Reactive, a
// #0 resumes in Re-Inactive and a nonblocking assignment lands in Re-NBA, a
// subroutine it calls runs the same way wherever it is declared, and so does
// its continuous assignment and what it drives through a port (LRM 24.3.1,
// 24.3.2, 24.5, 4.4.2.6-8). Once the reactive set has begun, it drains before
// the design runs again (LRM 4.5).
module Top;
  logic clk = 0, q = 0, clk2 = 0, q2 = 0;
  logic pw = 0, dn = 0, pn = 0;
  logic r = 0;
  wire dw1, dw2;
  logic v;
  logic saw_v = 'x;
  string order = "";
  logic saw_q = 'x, saw_q2 = 'x, saw_dn = 'x, saw_pn = 'x, saw_dw2 = 'x;

  always @(posedge clk) q <= 1;
  always @(posedge clk2) q2 <= 1;
  always @(pw) dn = pw;

  initial #1 order = {order, "d"};
  initial #2 clk = 1;
  initial #3 clk2 = 1;

  // Called from the program, so it waits and reads as the program does.
  task automatic read_q2;
    @(posedge clk2) saw_q2 = q2;
  endtask

  assign dw1 = r;
  p up (dw2, dw1, v);

  initial begin
    #6 r = 1;
    #0 saw_dw2 = dw2;
  end

  final begin
    if (order != "dp") $fatal(1, "order was \"%s\", expected \"dp\"", order);
    if (saw_q !== 1) $fatal(1, "the program read q as %b before the NBA", saw_q);
    if (saw_q2 !== 1) $fatal(1, "the design task read q2 as %b", saw_q2);
    if (saw_dn !== 0) $fatal(1, "the design ran before the program's #0 resumed");
    if (saw_pn !== 0) $fatal(1, "the program's NBA landed before Re-Inactive");
    if (saw_dw2 !== 0) $fatal(1, "the program's assign ran in the active set");
    if (dw2 !== 1) $fatal(1, "the program's assign never ran");
    if (saw_v !== 1) $fatal(1, "the program's output port read %b, expected 1", saw_v);
    $display("All checks passed");
  end
endmodule

program p (output pw2, input pw1, output logic pv);
  assign pw2 = pw1;
  initial begin
    #1 Top.order = {Top.order, "p"};
    @(posedge Top.clk) Top.saw_q = Top.q;
    Top.read_q2();
    #1 Top.pw = 1;
    #0 Top.saw_dn = Top.dn;
    Top.pn <= 1;
    #0 Top.saw_pn = Top.pn;
    #1 pv = 1;
    #0 Top.saw_v = Top.v;
    #5;
  end
endprogram
