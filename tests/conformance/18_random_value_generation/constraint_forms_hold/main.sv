// Every form a constraint takes holds of each answer: an implication and an
// if-else hold where their condition does (LRM 18.5.5, 18.5.6), a set
// membership (LRM 18.5.4 via 11.4.13), where an unpacked array in the set
// stands for each of its elements, a member of a packed union, which lies in
// the union's low bits (LRM 7.3.1), a uniqueness over variables and the
// elements of an array (LRM 18.5.4), a foreach over each index (LRM 18.5.7), a
// distribution that draws only values of nonzero weight in proportion to their
// weights (LRM 18.5.3), a soft constraint where the hard ones allow it, the
// later of two that conflict winning, and a disabled one not at all (LRM
// 18.5.13), and `solve before`, which changes how likely a value is and not
// which values are allowed (LRM 18.5.9).
module Top;
  typedef union packed {
    bit [7:0] low;
    bit [7:0] same;
  } overlay_t;

  class Forms;
    rand bit mode;
    rand bit [7:0] value;
    rand bit [3:0] pick;
    rand bit [7:0] lanes[4];
    rand bit [7:0] extra;
    rand bit [3:0] weighted;
    rand bit [7:0] preferred;
    rand bit [7:0] freed;
    rand bit flag;
    rand bit [7:0] after;
    rand bit [7:0] listed_in;
    bit [7:0] allowed[3] = '{3, 7, 11};
    bit [7:0] more[] = '{20, 21};
    rand overlay_t overlay;

    constraint gated {
      mode -> value > 100;
      if (!mode) value < 50; else value != 200;
    }
    constraint member { pick inside {[2:4], 9}; }
    constraint from_arrays { listed_in inside {allowed, more, 30}; }
    constraint overlaid { overlay.low > 10; overlay.same < 20; }
    constraint distinct {
      unique { lanes, extra };
      foreach (lanes[i]) lanes[i] < 5;
      extra < 5;
    }
    constraint drawn { weighted dist { 0 := 1, [1:3] :/ 2, 15 := 0 }; }
    constraint preferences {
      soft preferred == 3;
      soft preferred == 9;
      soft freed == 4;
      disable soft freed;
      freed > 4;
    }
    constraint ordering { flag -> after == 0; solve flag before after; }
  endclass

  int calls;
  int violations;
  int weighted_zero;
  int weighted_low;
  int flags_set;

  initial begin
    Forms f;
    f = new;
    calls = -1;
    violations = -1;
    weighted_zero = -1;
    weighted_low = -1;
    flags_set = -1;

    calls = 0;
    violations = 0;
    weighted_zero = 0;
    weighted_low = 0;
    flags_set = 0;
    for (int n = 0; n < 600; n++) begin
      calls += f.randomize();
      if (f.mode && f.value <= 100) violations++;
      if (!f.mode && f.value >= 50) violations++;
      if (f.mode && f.value == 200) violations++;
      if (!(f.pick inside {[2:4], 9})) violations++;
      if (!(f.listed_in inside {3, 7, 11, 20, 21, 30})) violations++;
      if (f.overlay.low <= 10 || f.overlay.low >= 20) violations++;
      foreach (f.lanes[i]) begin
        if (f.lanes[i] >= 5 || f.lanes[i] == f.extra) violations++;
        foreach (f.lanes[j]) if (i < j && f.lanes[i] == f.lanes[j]) violations++;
      end
      if (f.weighted > 3) violations++;
      if (f.weighted == 0) weighted_zero++;
      else weighted_low++;
      if (f.preferred != 9) violations++;
      if (f.freed <= 4) violations++;
      if (f.flag && f.after != 0) violations++;
      flags_set += f.flag;
    end
  end

  final begin
    if (calls !== 600) $fatal(1, "%0d of 600 calls succeeded", calls);
    if (violations !== 0) $fatal(1, "%0d constraint violations", violations);
    // Weight 1 for 0 against 2 shared by 1 to 3: about a third are 0.
    if (weighted_zero < 140 || weighted_zero > 260)
      $fatal(1, "0 was drawn %0d times in 600", weighted_zero);
    // Solved first, flag is 1 about half the time rather than once in 257.
    if (flags_set < 220 || flags_set > 380)
      $fatal(1, "flag was set %0d times in 600", flags_set);
    $display("All checks passed");
  end
endmodule
