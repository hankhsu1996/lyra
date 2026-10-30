// Each block of a loop is a scope of its own (LRM 27.4), so a class each
// block declares is a type of that block, and an object of one block's class
// is not of another's (LRM 8.16): a cast to a block's own class takes the
// object the first block made only in the first block. That holds where
// nothing the blocks hold reads the index, where only their code does, and
// where the class is declared in a scope inside the block.
module Top;
  class Base;
  endclass

  for (genvar i = 0; i < 3; i++) begin : unread
    class C extends Base;
    endclass
    Base kept;
    int cast = -1;
    initial begin
      C mine = new;
      kept = mine;
      #1;
      cast = $cast(mine, unread[0].kept);
    end
  end

  Base first_coded;
  for (genvar i = 0; i < 3; i++) begin : coded
    class C extends Base;
    endclass
    int cast = -1;
    initial begin
      C mine;
      if (i == 0) begin
        mine = new;
        first_coded = mine;
      end
      #1;
      cast = $cast(mine, first_coded);
    end
  end

  for (genvar i = 0; i < 3; i++) begin : deeper
    if (1) begin : inner
      class C extends Base;
      endclass
      Base kept;
      int cast = -1;
      initial begin
        C mine = new;
        kept = mine;
        #1;
        cast = $cast(mine, deeper[0].inner.kept);
      end
    end
  end

  final begin
    if (unread[0].cast !== 1 || unread[1].cast !== 0 || unread[2].cast !== 0)
      $fatal(1, "unread casts %0d %0d %0d, expected 1 0 0", unread[0].cast,
             unread[1].cast, unread[2].cast);
    if (coded[0].cast !== 1 || coded[1].cast !== 0 || coded[2].cast !== 0)
      $fatal(1, "coded casts %0d %0d %0d, expected 1 0 0", coded[0].cast,
             coded[1].cast, coded[2].cast);
    if (deeper[0].inner.cast !== 1 || deeper[1].inner.cast !== 0 ||
        deeper[2].inner.cast !== 0)
      $fatal(1, "deeper casts %0d %0d %0d, expected 1 0 0",
             deeper[0].inner.cast, deeper[1].inner.cast, deeper[2].inner.cast);
    $display("All checks passed");
  end
endmodule
