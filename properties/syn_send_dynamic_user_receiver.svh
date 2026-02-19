property data_stable_N_cycles (state_curr_0, counter_0, data_0);
    @(posedge clk_i) ((state_curr_0==DROP_VALID || counter_0!=0) |-> (data_0 == $past(data_0)));
endproperty 