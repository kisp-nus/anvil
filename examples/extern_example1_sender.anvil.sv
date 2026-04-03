/* verilator lint_off UNOPTFLAT */
/* verilator lint_off WIDTHTRUNC */
/* verilator lint_off WIDTHEXPAND */
/* verilator lint_off WIDTHCONCAT */
module extern_example1_sender (
  input logic[0:0] clk_i,
  input logic[0:0] rst_ni,
  input logic[0:0] _ep_req_ack,
  output logic[0:0] _ep_req_valid,
  output logic[7:0] _ep_req_0
);
  logic[7:0] r_q;
  always_ff @(posedge clk_i or negedge rst_ni) begin : _proc_transition
    if (~rst_ni) begin
    end
  end
  logic[7:0] thread_0_wire$4;
  logic[7:0] thread_0_wire$3;
  logic[7:0] thread_0_wire$2;
  logic[7:0] thread_0_wire$0;
  assign thread_0_wire$0 = r_q;
  localparam logic[7:0] thread_0_wire$1 = 8'b1;
  assign thread_0_wire$2 = thread_0_wire$0 + thread_0_wire$1;
  assign thread_0_wire$3 = r_q;
  assign thread_0_wire$4 = r_q;
  for (genvar i = 0; i < 4; i ++) begin : EVENTS0
    logic event_current;
    end
  logic _init_0;
  logic[1:0] _thread_0_event_counter_3_q, _thread_0_event_counter_3_n;
  logic _thread_0_event_syncstate_2_q, _thread_0_event_syncstate_2_n;
  logic _thread_0_event_counter_1_1_q, _thread_0_event_counter_1_1_n;
  assign EVENTS0[3].event_current = _thread_0_event_counter_3_q == 2'd2;
    assign _thread_0_event_counter_3_n = EVENTS0[2].event_current ? 2'd1 : EVENTS0[3].event_current ? '0 : _thread_0_event_counter_3_q ? (_thread_0_event_counter_3_q + 2'd1) : _thread_0_event_counter_3_q;
  assign EVENTS0[2].event_current = (EVENTS0[1].event_current || _thread_0_event_syncstate_2_q) && _ep_req_ack;
    assign _thread_0_event_syncstate_2_n = (EVENTS0[1].event_current || _thread_0_event_syncstate_2_q) && !_ep_req_ack;
  assign EVENTS0[1].event_current = _thread_0_event_counter_1_1_q;
  assign _thread_0_event_counter_1_1_n = EVENTS0[0].event_current;
  assign EVENTS0[0].event_current = _init_0 || EVENTS0[3].event_current;
  assign _ep_req_valid = (EVENTS0[1].event_current || _thread_0_event_syncstate_2_q);
  assign _ep_req_0 = thread_0_wire$4;
  always_ff @(posedge clk_i or negedge rst_ni) begin : _thread_0_st_transition
    if (~rst_ni) begin
      _init_0 <= 1'b1;
      r_q <= '0;
      _thread_0_event_counter_3_q <= '0;
      _thread_0_event_syncstate_2_q <= '0;
      _thread_0_event_counter_1_1_q <= '0;
    end else begin
      if (EVENTS0[1].event_current) begin
        $display("Data Send = %d", thread_0_wire$3);
      end
      if (EVENTS0[0].event_current) begin
        r_q[0 +: 8] <= thread_0_wire$2;
      end
      _init_0 <= 1'b0;
      _thread_0_event_counter_3_q <= _thread_0_event_counter_3_n;
      _thread_0_event_syncstate_2_q <= _thread_0_event_syncstate_2_n;
      _thread_0_event_counter_1_1_q <= _thread_0_event_counter_1_1_n;
    end
  end
endmodule
