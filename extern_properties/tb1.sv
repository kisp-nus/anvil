
module tb1 #(
    parameter int lifetime = 3
) (
    input logic clk_i,
    input logic rst_ni,
    tb1_control_if tb1_control_if,
    data_if tb1_data_if
);
    logic rst_assert_ni;
    typedef enum logic [1:0] {IDLE, WAIT_ACK, HOLD_DATA} state_t;
    state_t state_curr, state_next;
    int counter;
    logic [$bits(tb1_data_if.data)-1:0] tracked_data;

    always_ff @(posedge clk_i or negedge rst_ni) begin
        if (!rst_ni) begin
            state_curr <= IDLE;
            rst_assert_ni <= 1'b0;
            counter <= 0;
        end else begin
            state_curr <= state_next;
            rst_assert_ni <= 1'b1;
            case (state_curr)
                IDLE: begin
                    counter <= 0;
                    tracked_data <= 0;
                    if (tb1_control_if.valid) begin
                        tracked_data <= tb1_data_if.data;
                    end
                end

                WAIT_ACK: begin
                    counter <= 0;
                end

                HOLD_DATA: begin
                    if (counter == lifetime - 1) begin
                        if (tb1_control_if.valid) begin
                            tracked_data <= tb1_data_if.data;
                        end
                        counter <= 0;
                    end else begin
                        counter <= counter + 1;
                    end
                end

                default: begin
                    counter <= 0;
                    tracked_data <= 0;
                end
            endcase
        end
    end

    always_comb begin
        case (state_curr)
            IDLE: begin
                if (tb1_control_if.valid && tb1_control_if.ack) begin
                    state_next = HOLD_DATA;
                end else if (tb1_control_if.valid && !tb1_control_if.ack) begin
                    state_next = WAIT_ACK;
                end else begin
                    state_next = IDLE;
                end
            end

            WAIT_ACK: begin
                if (tb1_control_if.valid && tb1_control_if.ack) begin
                    state_next = HOLD_DATA;
                end else begin
                    state_next = WAIT_ACK;
                end
            end

            HOLD_DATA: begin
                if (counter == lifetime - 1) begin
                    if (tb1_control_if.valid && tb1_control_if.ack) begin
                        state_next = HOLD_DATA;
                    end else if (tb1_control_if.valid && !tb1_control_if.ack) begin
                        state_next = WAIT_ACK;
                    end else begin
                        state_next = IDLE;
                    end
                end else begin
                    state_next = HOLD_DATA;
                end
            end

            default: state_next = IDLE;
        endcase
    end

    property holds_valid_before_handshake;
        @(posedge clk_i) disable iff (!rst_assert_ni) (state_curr == WAIT_ACK) |-> tb1_control_if.valid;
    endproperty

    property holds_data_stable;
        @(posedge clk_i) disable iff (!rst_assert_ni) (state_curr == WAIT_ACK || state_curr == HOLD_DATA) |-> tb1_data_if.data == tracked_data;
    endproperty

    assert property (holds_valid_before_handshake)
        else $error("Assertion Failed: holds_valid_before_handshake");

    assert property (holds_data_stable)
        else $error("Assertion Failed: holds_data_stable");

endmodule

