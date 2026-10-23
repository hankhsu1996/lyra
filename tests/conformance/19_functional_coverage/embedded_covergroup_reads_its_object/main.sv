// A covergroup a class embeds names the object's properties and methods, may
// take arguments, and is made in the class's constructor; one with a clocking
// event samples the object as it stands at each occurrence (LRM 19.4).
module Top;
  class C;
    bit [3:0] x;
    bit e;
    int lim = 2;

    function int twice(int v);
      return v * 2;
    endfunction

    covergroup cv (int arg);
      option.at_least = arg;
      a: coverpoint x { bins low = {[0:lim]}; bins high = {[lim + 1:15]}; }
      b: coverpoint twice(x) { bins few = {[0:7]}; }
    endgroup

    covergroup ev @(e);
      coverpoint x { bins all[] = {[0:3]}; }
    endgroup

    function new(int p1);
      cv = new(p1);
      ev = new;
    endfunction
  endclass

  real cv_cov, a_cov, b_cov, ev_cov;

  initial begin
    static C obj = new(1);
    obj.x = 1; obj.cv.sample();
    obj.x = 9; obj.cv.sample();
    obj.x = 2; #1 obj.e = 1;
    #1 obj.x = 3; obj.e = 0;
    #1;
    cv_cov = obj.cv.get_inst_coverage();
    a_cov = obj.cv.a.get_inst_coverage();
    b_cov = obj.cv.b.get_inst_coverage();
    ev_cov = obj.ev.get_inst_coverage();
  end

  final begin
    if (cv_cov != 100.0 || a_cov != 100.0 || b_cov != 100.0)
      $fatal(1, "the embedded coverage was %0f (a %0f, b %0f), expected 100", cv_cov, a_cov,
             b_cov);
    // 2 and 3 of 0 to 3.
    if (ev_cov != 50.0) $fatal(1, "the event's coverage was %0f, expected 50", ev_cov);
    $display("All checks passed");
  end
endmodule
