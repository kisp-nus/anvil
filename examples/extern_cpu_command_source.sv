module extern_cpu_command_source (
    input  logic        clk_i,
    input  logic        rst_ni,
    output logic [15:0] data_o,
    output logic        valid_o
);

    logic [3:0] counter;

    always_ff @(posedge clk_i or negedge rst_ni) begin
        if (!rst_ni) begin
            data_o  <= 16'h00A5;
            valid_o <= 1'b0;
            counter <= 0;
        end else begin
            valid_o <= 1'b0;

            if (counter == 9) begin
                data_o  <= data_o + 1;
                valid_o <= 1'b1;
                counter <= 0;

                $display("CPU issued config command: %d", data_o + 1);
            end else begin
                counter <= counter + 1;
            end
        end
    end

endmodule