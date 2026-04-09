
property ack_low_during_DROP_ACK (state_curr_0, ack_0);
    @(posedge clk_i) (state_curr_0==DROP_ACK) |-> !ack_0;
endproperty

property data_stable_N_cycles (state_curr_0, counter_0, data_0);
    @(posedge clk_i) ((state_curr_0==DROP_ACK || counter_0!=0) |-> (data_0 == $past(data_0)));
endproperty 
