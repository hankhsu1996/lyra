// A generate loop whose blocks each declare a type and hold a variable of it
// (LRM 27.3). N is how many blocks the loop counts out.
module Top #(parameter int N = 4);
  int sink [N];
  for (genvar i = 0; i < N; i += 1) begin : g
    typedef struct {
      int count;
      logic [3:0] tag;
    } entry_t;

    entry_t held;
    initial begin
      held.count = i;
      sink[i] = held.count;
    end
  end
endmodule
