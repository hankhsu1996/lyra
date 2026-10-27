// Every execution of a nonblocking assignment schedules an update event of its
// own, holding the value and the target computed when the statement executed,
// and the updates land in the order the statements executed (LRM 4.6, 4.9.4,
// 10.4.2). One statement executed many times in one time step schedules that
// many updates; one carrying a delay schedules into the slot the delay names,
// and two landing in one later slot keep their order. What an update holds is
// held until it lands, a string and an object handle included, even where
// nothing else holds it then (LRM 8.29), and so is a wide value together with
// the position its target was selected at. A program's nonblocking update is
// deferred as well (LRM 24.3.1).
class C;
  int n;
  function new(int n);
    this.n = n;
  endfunction
endclass

// The run ends once the program's initial procedure does (LRM 24.3), so it
// outlasts what the module checks.
program Prog(output int landed, output int before_landing);
  initial begin
    landed <= 41;
    landed <= 42;
    before_landing = landed;
    #100;
  end
endprogram

module Top;
  int many [3000];
  int last;
  int last_before_step_end;

  string delayed [10];
  string same_slot;
  string same_slot_seen;

  C target;
  int landed_n;

  logic [3999:0] wide [4];
  logic [3999:0] wide_value;
  int wide_at;

  int program_landed;
  int program_before_landing;
  Prog prog(program_landed, program_before_landing);

  initial begin
    for (int i = 0; i < 3000; i++) begin
      many[i] <= i * 3;
      last <= i;
    end
    last_before_step_end = last;
    #1;
    for (int i = 0; i < 3000; i++)
      if (many[i] !== i * 3)
        $fatal(1, "many[%0d] was %0d, expected %0d", i, many[i], i * 3);
    if (last_before_step_end !== 0)
      $fatal(1, "last before the step ended was %0d, expected 0",
             last_before_step_end);
    if (last !== 2999) $fatal(1, "last was %0d, expected 2999", last);
  end

  // `delayed[k]` lands at time 2k + 2 and is looked at a step later, before the
  // next one is due. A delay may not read an automatic variable (LRM 13.3.2),
  // so the loop that schedules them counts in a static one.
  int scheduled;
  initial begin
    for (scheduled = 0; scheduled < 10; scheduled++)
      delayed[scheduled] <= #(2 * scheduled + 2)
          $sformatf("v%0d", scheduled);
    same_slot <= #6 "first";
    same_slot <= #6 "second";
    #1;
    for (int k = 0; k < 10; k++) begin
      #2;
      if (delayed[k] != $sformatf("v%0d", k))
        $fatal(1, "delayed[%0d] was \"%s\" at time %0t", k, delayed[k], $time);
      if (k + 1 < 10 && delayed[k + 1] != "")
        $fatal(1, "delayed[%0d] landed before its slot", k + 1);
      if (k == 2) same_slot_seen = same_slot;
    end
    if (same_slot_seen != "second")
      $fatal(1, "same_slot was \"%s\", expected \"second\"", same_slot_seen);
  end

  initial begin
    static C obj = new(7);
    target <= #3 obj;
    obj = null;
    #4;
    landed_n = target.n;
  end

  // The value and the position are read when each statement executes, so
  // changing both before the updates land changes neither update.
  initial begin
    wide_value = '1;
    wide_value[7:0] = 8'h5a;
    wide_at = 2;
    wide[wide_at] <= wide_value;
    wide[wide_at + 1] <= #2 wide_value;
    wide_value = '0;
    wide_at = 0;
    #3;
    if (wide[2][7:0] !== 8'h5a || wide[2][3999] !== 1'b1)
      $fatal(1, "wide[2] landed as %h", wide[2][7:0]);
    if (wide[3][7:0] !== 8'h5a || wide[3][3999] !== 1'b1)
      $fatal(1, "wide[3] landed as %h", wide[3][7:0]);
    if (wide[0] !== 'x) $fatal(1, "wide[0] was written");
  end

  final begin
    if (landed_n !== 7)
      $fatal(1, "the update landed an object holding %0d, expected 7",
             landed_n);
    if (program_before_landing !== 0)
      $fatal(1, "the program's update landed before its region, as %0d",
             program_before_landing);
    if (program_landed !== 42)
      $fatal(1, "the program's update landed %0d, expected 42", program_landed);
    $display("All checks passed");
  end
endmodule
