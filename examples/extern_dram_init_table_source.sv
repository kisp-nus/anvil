module extern_dram_init_table_source (
    input  logic        clk_i,
    input  logic        rst_ni,
    output logic [15:0] data_o
);

    // Counts the number of cycles before publishing
    // the next DRAM timing-table value.
    logic [2:0] counter;

    always_ff @(posedge clk_i or negedge rst_ni) begin
        if (!rst_ni) begin
            // Initial timing-table value.
            data_o  <= 16'h0137;
            counter <= 0;
        end else begin

            // With lifetime = 3 and static_interval = 3,
            // TB4 reaches a legal data-capture boundary every
            // 4 cycles. Changing every 8 cycles keeps the
            // updates aligned with that boundary.
            if (counter == 7) begin
                data_o  <= data_o + 1;
                counter <= 0;

                $display("DRAM timing word changed to: %d", data_o + 1);
            end else begin
                // Hold the current timing word stable.
                counter <= counter + 1;
            end
        end
    end

endmodule
