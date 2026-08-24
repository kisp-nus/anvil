module extern_dram_read_data_source (
    input  logic        clk_i,
    input  logic        rst_ni,
    output logic [63:0] data_o,
    input  logic        ack_i
);

    logic [1:0] counter;
    logic       started;

    always_ff @(posedge clk_i or negedge rst_ni) begin
        if (!rst_ni) begin
            data_o  <= 64'hDEAD_BEEF_0000_0042;
            counter <= 0;
            started <= 1'b0;
        end else begin

            // Do not change data before the first handshake.
            if (!started) begin
                if (ack_i) begin
                    started <= 1'b1;
                    counter <= 0;
                end
            end

            // After handshake, wait for two cycles without ack
            // before presenting the next data value.
            else if (ack_i) begin
                counter <= 0;
            end

            else if (counter == 1) begin
                data_o  <= data_o + 1;
                counter <= 0;

                $display("DRAM data changed to: 0x%h", data_o + 1);
            end

            else begin
                counter <= counter + 1;
            end
        end
    end

endmodule