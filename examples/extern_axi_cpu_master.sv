module extern_axi_cpu_master (
    input  logic        clk_i,
    input  logic        rst_ni,
    output logic [31:0] data_o,
    output logic        valid_o,
    input  logic        ack_i
);

    // Simple AXI-like requester: hold a command valid until the memory controller accepts it.
    always_ff @(posedge clk_i or negedge rst_ni) begin
        if (!rst_ni) begin
            data_o  <= 32'h0000_0000;
            valid_o <= 1'b0;
        end else begin
            data_o  <= 32'h1000_0040;
            valid_o <= 1'b1;
        end
    end

endmodule


