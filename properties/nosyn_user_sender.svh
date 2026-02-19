
property data_stable_valid_high (_endp_res_valid_0, state_curr_0, _endp_res_0_0);
    @(posedge clk_i) ((_endp_res_valid_0 && !$rose(_endp_res_valid_0)) && (state_curr_0 inside {WAIT_REQ, WAIT_ACK})) |-> _endp_res_0_0 == $past(_endp_res_0_0);
endproperty

property valid_high_until_ack_high (_endp_res_valid_0, state_curr_0);
    @(posedge clk_i) ((_endp_res_valid_0 || $rose(_endp_res_valid_0)) && (state_curr_0 inside {WAIT_REQ, WAIT_ACK}) |-> _endp_res_valid_0);
endproperty

property  ack_high_valid_low(_endp_res_valid_0, state_curr_0, _endp_res_ack_0);
    @(posedge clk_i) (state_curr_0==DROP_VALID) |-> (!_endp_res_valid_0 && $past(_endp_res_ack_0));
endproperty

property data_stable_N_cycles_after_ack_high (state_curr_0, counter_0, _endp_res_0_0);
    @(posedge clk_i) ((state_curr_0==DROP_VALID || counter_0!=0) |-> (_endp_res_0 == $past(_endp_res_0_0)));
endproperty

property wait_req_ack_low (state_curr_0, _endp_res_ack_0);
    @(posedge clk_i) ((state_curr_0==WAIT_REQ) |-> !_endp_res_ack_0);
endproperty
