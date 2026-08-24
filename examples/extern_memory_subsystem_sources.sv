module extern_memory_subsystem_sources (
    input  logic        clk_i,
    input  logic        rst_ni,

    // TB1: Dynamic -> Dynamic
    output logic [31:0] axi_data_o,
    output logic        axi_valid_o,
    input  logic        axi_ack_i,

    // TB2: Static -> Dynamic
    output logic [63:0] read_data_o,
    input  logic        read_ack_i,

    // TB3: Dynamic -> Static
    output logic [15:0] cfg_data_o,
    output logic        cfg_valid_o,

    // TB4: Static -> Static
    output logic [15:0] timing_data_o
);

    always_ff @(posedge clk_i or negedge rst_ni) begin
        if (!rst_ni) begin
            // Keep all data values initialized to the same values
            // that will be used after reset so the static-data
            // assertions do not see an unnecessary transition.
            axi_data_o    <= 32'h1000_0040;
            axi_valid_o   <= 1'b0;

            read_data_o   <= 64'hDEAD_BEEF_0000_0042;

            cfg_data_o    <= 16'h00A5;
            cfg_valid_o   <= 1'b0;

            timing_data_o <= 16'h0137;
        end else begin
            // TB1:
            // Dynamic sender keeps valid asserted.
            axi_data_o  <= 32'h1000_0040;
            axi_valid_o <= 1'b1;

            // TB2:
            // Static data remains stable.
            read_data_o <= 64'hDEAD_BEEF_0000_0042;

            // TB3:
            // Dynamic sender keeps valid asserted while data
            // remains stable throughout the static hold window.
            cfg_data_o  <= 16'h00A5;
            cfg_valid_o <= 1'b1;

            // TB4:
            // Both sides are static, so only stable data is needed.
            timing_data_o <= 16'h0137;
        end
    end

endmodule