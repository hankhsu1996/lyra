// A generate block may declare a type (Syntax 27-1 admits a data declaration
// as a generate item), and what it declares acts as it would in a module
// brought into existence by an instantiation (LRM 27.3). So each block
// instance of a loop holds its own variable of the type its block declares,
// and a hierarchical name reaches a member of each (LRM 23.6).
module Top;
  for (genvar i = 0; i < 3; i++) begin : g
    typedef struct {
      int count;
      logic [3:0] tag;
    } entry_t;

    entry_t held;
    entry_t copied;

    initial begin
      held.count = -1;
      held.tag = 4'hf;
      #1;
      held.count = (i + 1) * 7;
      held.tag = 4'(i + 1);
      copied = held;
    end
  end

  final begin
    if (g[0].held.count !== 7)
      $fatal(1, "g[0].held.count is %0d, expected 7", g[0].held.count);
    if (g[1].copied.count !== 14)
      $fatal(1, "g[1].copied.count is %0d, expected 14", g[1].copied.count);
    if (g[2].copied.count !== 21)
      $fatal(1, "g[2].copied.count is %0d, expected 21", g[2].copied.count);
    if (g[1].copied.tag !== 4'h2)
      $fatal(1, "g[1].copied.tag is %h, expected 2", g[1].copied.tag);
    if (g[2].held.tag !== 4'h3)
      $fatal(1, "g[2].held.tag is %h, expected 3", g[2].held.tag);
    $display("All checks passed");
  end
endmodule
