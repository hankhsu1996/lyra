module Leaf;
  initial begin
    #2 $display("leaf=%m");
  end
endmodule

module Mid;
  Leaf l();
  initial begin
    #1 $display("mid=%m");
  end
  generate
    if (1) begin : gblk
      initial begin
        #3 $display("gblk=%m");
      end
    end
    for (genvar i = 0; i < 1; i = i + 1) begin : loop
      initial begin
        #4 $display("loop=%m");
      end
    end
  endgenerate
endmodule

module Cell;
  int v;
endmodule

module Top;
  Mid m();
  Cell c[2]();
  generate
    for (genvar i = 0; i < 2; i = i + 1) begin : g
      int x = i + 10;
    end
  endgenerate
  initial begin
    $display("top=%m");
    #5;
    c[0].v = 100;
    c[1].v = 200;
    $display("c0=%0d c1=%0d g0=%0d g1=%0d",
             c[0].v, c[1].v, g[0].x, g[1].x);
    $strobe("strobe=%m");
    #1;
  end
  // A named block is a scope, so it is part of the hierarchical name; an
  // unnamed one is not, and names the innermost named block around it.
  initial begin : nb
    #6;
    $display("nb=%m");
    begin : deeper
      $display("deeper=%m");
    end
    begin
      $display("unnamed=%m");
    end
    // LRM 21.2.1.5 names the scope that *invokes* the task, so a deferred
    // display still reports the call site.
    $strobe("nb_strobe=%m");
  end
  function void func();
    $display("func=%m");
  endfunction
  initial #7 func();
  // LRM 12.7.1 / 12.7.3: a loop that declares its own control variables gets an
  // implicit begin-end block around it, and a statement label names that block
  // (LRM 9.3.5), so the label joins the hierarchical name of everything inside.
  int arr[2];
  initial begin : lp
    #8;
    lbl: foreach (arr[k]) begin : body
      static int hits = 0;
      hits = hits + 1;
      $display("loopblk=%m hits=%0d", hits);
    end
    fl: for (int n = 0; n < 1; n = n + 1) begin : fbody
      $display("forblk=%m");
    end
    $display("deep=%0d", Top.lp.lbl.body.hits);
  end
endmodule
