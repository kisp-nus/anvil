
property ack_low_when_valid_high (state_curr_0, valid_0, ack_0, state_prev_0);
    @(posedge clk_i) ((state_curr_0==WAIT_ACK && $rose(valid_0) && state_prev_0==WAIT_ACK) |-> !ack_0);
endproperty

// property  ack_high_valid_low (state_curr_0, valid_0, ack_0);
//     @(posedge clk_i) (state_curr_0==DROP_VALID) |-> (!_ep_req_valid && $past(ack_0));
// endproperty

property ack_low_after_handshake (state_prev_0, ack_0);
    @(posedge clk_i) (state_prev_0==DROP_VALID) |-> !ack_0;
endproperty

// property data_stable_N_cycles_after_ack_high (state_curr_0, counter_0, data_0, valid_0);
//     @(posedge clk_i) ((state_curr_0==DROP_VALID || counter_0!=0 && !valid_0) |-> (data_0 == $past(data_0)));
// endproperty
