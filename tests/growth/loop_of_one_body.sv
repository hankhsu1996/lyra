// A generate loop whose blocks all state the same thing: each declares a
// variable and reads its index, which is a value its construction is handed
// (LRM 27.4). N is how many blocks the loop counts out.
module Top #(parameter int N = 4);
  int sink [N];
  for (genvar i = 0; i < N; i += 1) begin : g
    int here;
    initial begin
      here = i;
      sink[i] = here;
    end
  end
endmodule
