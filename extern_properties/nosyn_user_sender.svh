
property data_stable_valid_high (valid, current_state, data);
    @(posedge clk_i) ((valid && !$rose(valid)) && (current_state inside {WAIT_REQ, WAIT_ACK})) |-> data == $past(data);
endproperty 

property valid_high_until_ack_high (valid, current_state, ack);
    @(posedge clk_i) ((valid || $rose(valid)) && (current_state inside {WAIT_REQ, WAIT_ACK}) && !ack |=> valid);
endproperty

property  ack_high_valid_low (valid, current_state, ack);
    @(posedge clk_i) (current_state==DROP_VALID) |-> (!valid && $past(ack));
endproperty

property data_stable_N_cycles_after_ack_high (current_state, counter_0, data);
    @(posedge clk_i) ((current_state==DROP_VALID || counter_0!=0) |-> (data == $past(data)));
endproperty

property wait_req_ack_low (valid, ack);
    @(posedge clk_i) ((valid && !ack) |=> valid);
endproperty


