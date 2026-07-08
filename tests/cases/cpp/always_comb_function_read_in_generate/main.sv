// LRM 9.2.2.2.1: an always_comb inside a generate block is sensitive to a
// signal read only inside a function it calls, even when that function is
// declared in an enclosing scope and the signal lives in a sibling generate
// block. Reaching the signal's subscription depends on the target's elaborated
// position, not on which scope lowered first: the reader (in `rdr`) and the
// function (`read_tgt`, at module scope) occupy different scopes, and the
// target (`tgt.v`) is a sibling of the reader's enclosing block.
module Test;
  logic [7:0] src;
  logic [7:0] obs;

  if (1) begin : tgt
    logic [7:0] v;
    always_comb v = src;
  end

  function automatic logic [7:0] read_tgt();
    return tgt.v;
  endfunction

  if (1) begin : rdr
    logic [7:0] o;
    always_comb o = read_tgt();
  end

  initial begin
    src = 8'd1;
    #1;
    src = 8'd9;
    #1;
    obs = rdr.o;
  end
endmodule
