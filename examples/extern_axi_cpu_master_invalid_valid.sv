module extern_axi_cpu_master_invalid_valid (
    input  logic        clk_i,
    input  logic        rst_ni,
    output logic [31:0] data_o,
    output logic        valid_o,
    input  logic        ack_i
);

logic toggle;

always_ff @(posedge clk_i or negedge rst_ni) begin
    if (!rst_ni) begin
        data_o  <= 32'h0000_0000;
        valid_o <= 1'b0;
        toggle  <= 1'b0;
    end else begin
        data_o <= 32'h1000_0040;

        // INTENTIONALLY WRONG:
        // valid is asserted for one cycle, then dropped
        // regardless of whether ack has arrived.
        if (!toggle) begin
            valid_o <= 1'b1;
            toggle  <= 1'b1;
        end else begin
            valid_o <= 1'b0;
            toggle  <= 1'b0;
        end
    end
end

endmodule