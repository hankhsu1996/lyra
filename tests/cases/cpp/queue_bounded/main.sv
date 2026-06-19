// LRM 7.10.5 bounded queue `int q[$:N]`: the queue never holds an element whose
// index exceeds N, so any write growing it past N+1 elements discards the
// overflow (a warning is also issued). The bound reaches the variable however
// it is first built -- empty declaration, assignment-pattern initializer, or a
// later whole-queue assignment -- and is retained across later assignments.
module Top;
  bit [7:0] grow [$:3];
  bit [7:0] init_big [$:2] = '{1, 2, 3, 4, 5};
  bit [7:0] assigned [$:2];
  bit [7:0] retained [$:3] = '{1, 2};

  int s_grow;
  int s_init;
  int s_assigned;
  int s_retained;

  initial begin
    grow.push_back(8'd10);
    grow.push_back(8'd20);
    grow.push_back(8'd30);
    grow.push_back(8'd40);
    grow.push_back(8'd50);
    s_grow = grow.size();

    s_init = init_big.size();

    assigned = {8'd1, 8'd2, 8'd3, 8'd4};
    s_assigned = assigned.size();

    retained.push_back(8'd3);
    retained.push_back(8'd4);
    retained.push_back(8'd5);
    s_retained = retained.size();
  end
endmodule
