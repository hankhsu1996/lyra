// Input-port connection value resolution. Omitting a port inserts its declared
// default (LRM 23.2.2.4); an explicit empty connection `.din()` suppresses the
// default and leaves the port at its type default (LRM 23.3.2.2 / 23.3.3.2); an
// explicit expression overrides. (Some simulators reuse the default for `.din()`
// too; that is non-conformant, so this diverges from them by design.)
module Child (input int din = 171, input int ein, input int cin, output int dout);
  assign dout = din + ein + cin;
endmodule

module Top;
  int c;
  int q_def;
  int q_conn;
  int q_empty;
  Child u_def (.cin(c), .dout(q_def));
  Child u_conn (.din(8), .ein(100), .cin(c), .dout(q_conn));
  Child u_empty (.din(), .cin(c), .dout(q_empty));
  initial begin
    c = 5;
    #1 $display("%0d %0d %0d", q_def, q_conn, q_empty);
  end
endmodule
