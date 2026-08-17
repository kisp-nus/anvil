module tb2 #(
    parameter int lifetime = 3,
    parameter int static_interval = 2
) (
    input logic clk_i,
    input logic rst_ni,
    tb2_control_if tb2_control_if,
    data_if tb2_data_if
);
    logic rst_assert_ni;
    typedef enum logic [2:0] {CAPTURE_DATA_AT_START, HOLD_DATA_BEFORE_FIRST_HANSHAKE, CAPTURE_DATA_AT_STATIC_INTERVAL, HOLD_DATA_STATIC_INTERVAL, HOLD_DATA_HANDSHAKE_OR_STATIC_INTERVAL} state_t;
    state_t state_curr, state_next;
    int static_remaining;
    int lifetime_remaining;
    logic[$bits(tb2_data_if.data)-1:0] tracked_data;

    always_ff @(posedge clk_i or negedge rst_ni) begin
        if (!rst_ni) begin
            state_curr <= CAPTURE_DATA_AT_START;
            rst_assert_ni <= 1'b0;
            static_remaining <= 0;
            lifetime_remaining <= 0;
            tracked_data <= 0;
        end else begin
            state_curr <= state_next;
            rst_assert_ni <= 1'b1;
            case (state_curr) 
                CAPTURE_DATA_AT_START: begin
                    tracked_data <= tb2_data_if.data;
                    if (tb2_control_if.ack) begin
                        lifetime_remaining <= lifetime - 1;
                    end
                end

                HOLD_DATA_BEFORE_FIRST_HANSHAKE: begin
                    if (tb2_control_if.ack) begin
                        lifetime_remaining <= lifetime - 1;
                    end
                end

                CAPTURE_DATA_AT_STATIC_INTERVAL: begin
                    tracked_data <= tb2_data_if.data;
                    static_remaining <= static_interval - 1;
                    if (tb2_control_if.ack) begin
                        lifetime_remaining <= lifetime - 1;
                    end
                end

                HOLD_DATA_STATIC_INTERVAL: begin
                    static_remaining <= static_remaining - 1;
                    if (tb2_control_if.ack) begin
                        lifetime_remaining <= lifetime - 1;
                    end
                end

                HOLD_DATA_HANDSHAKE_OR_STATIC_INTERVAL: begin
                    if (static_remaining == 0) begin
                        static_remaining <= static_interval - 1;
                    end else begin
                        static_remaining <= static_remaining - 1;
                    end

                    if (lifetime_remaining > 0) begin
                        lifetime_remaining <= lifetime_remaining - 1;
                    end

                    if (tb2_control_if.ack && static_remaining==0 && lifetime_remaining==0) begin
                        tracked_data <= tb2_data_if.data;
                        lifetime_remaining <= lifetime - 1;
                    end
                end

                default: begin
                    tracked_data <= 0;
                end
            endcase
        end
    end

    always_comb begin
        case (state_curr)
            CAPTURE_DATA_AT_START: begin
                if (tb2_control_if.ack) begin
                    state_next = HOLD_DATA_HANDSHAKE_OR_STATIC_INTERVAL;
                end else begin
                    state_next = HOLD_DATA_BEFORE_FIRST_HANSHAKE;
                end
            end

            HOLD_DATA_BEFORE_FIRST_HANSHAKE: begin
                if (tb2_control_if.ack) begin
                    state_next = HOLD_DATA_HANDSHAKE_OR_STATIC_INTERVAL;
                end else begin
                    state_next = HOLD_DATA_BEFORE_FIRST_HANSHAKE;
                end
            end

            CAPTURE_DATA_AT_STATIC_INTERVAL: begin
                if (tb2_control_if.ack) begin
                    state_next = HOLD_DATA_HANDSHAKE_OR_STATIC_INTERVAL;
                end else begin
                    state_next = HOLD_DATA_STATIC_INTERVAL;
                end
            end

            HOLD_DATA_STATIC_INTERVAL: begin
                if (tb2_control_if.ack) begin
                    state_next = HOLD_DATA_HANDSHAKE_OR_STATIC_INTERVAL;
                end else begin
                    if (static_remaining == 0) begin
                        state_next = CAPTURE_DATA_AT_STATIC_INTERVAL;
                    end else begin
                        state_next = HOLD_DATA_STATIC_INTERVAL;
                    end
                end
            end

            HOLD_DATA_HANDSHAKE_OR_STATIC_INTERVAL: begin
                if (!tb2_control_if.ack && static_remaining == 0 && lifetime_remaining == 0) begin
                    state_next = CAPTURE_DATA_AT_STATIC_INTERVAL;
                end else begin
                    state_next = HOLD_DATA_HANDSHAKE_OR_STATIC_INTERVAL;
                end
            end

            default: state_next = CAPTURE_DATA_AT_START;
        endcase
    end

    property holds_data_stable_syn;
        @(posedge clk_i) disable iff (!rst_assert_ni)
        (state_curr == HOLD_DATA_BEFORE_FIRST_HANSHAKE || state_curr == HOLD_DATA_STATIC_INTERVAL || (state_curr == HOLD_DATA_HANDSHAKE_OR_STATIC_INTERVAL && !(static_remaining == 0 && lifetime_remaining == 0 && tb2_control_if.ack)))
        |-> tb2_data_if.data == tracked_data;
    endproperty


    assert property (holds_data_stable_syn)
        else $error("Assertion Failed: holds_data_stable");

endmodule

