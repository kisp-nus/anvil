
module tb4 #(
    parameter int lifetime = 3,
    parameter int static_interval = 2
) (
    input logic clk_i,
    input logic rst_ni,
    data_if tb4_data_if
);
    logic rst_assert_ni;
    typedef enum logic [0:0] {CAPTURE_DATA, HOLD_DATA_HANDSHAKE_OR_STATIC_INTERVAL} state_t;
    state_t state_curr, state_next;
    int static_remaining;
    int lifetime_remaining;
    logic [$bits(tb4_data_if.data)-1:0] tracked_data;

    always_ff @(posedge clk_i or negedge rst_ni) begin
        if (!rst_ni) begin
            state_curr <= CAPTURE_DATA;
            rst_assert_ni <= 1'b0;
            static_remaining <= 0;
            lifetime_remaining <= 0;
            tracked_data <= 0;
        end else begin
            state_curr <= state_next;
            rst_assert_ni <= 1'b1;
            case (state_curr) 
                CAPTURE_DATA: begin
                    tracked_data <= tb4_data_if.data;
                    lifetime_remaining <= lifetime - 1;
                    static_remaining <= static_interval - 1;
                end

                HOLD_DATA_HANDSHAKE_OR_STATIC_INTERVAL: begin
                    if (static_remaining == 0) begin
                        static_remaining <= static_interval - 1;
                    end else begin
                        static_remaining <= static_remaining - 1;
                    end

                    if (lifetime_remaining == 0) begin
                        lifetime_remaining <= lifetime - 1;
                    end else begin
                        lifetime_remaining <= lifetime_remaining - 1;
                    end
                end
            endcase
        end
    end

    always_comb begin
        case (state_curr)
            CAPTURE_DATA: begin
                state_next = HOLD_DATA_HANDSHAKE_OR_STATIC_INTERVAL;
            end

            HOLD_DATA_HANDSHAKE_OR_STATIC_INTERVAL: begin
                if (static_remaining == 0 && lifetime_remaining == 0) begin
                    state_next = CAPTURE_DATA;
                end else begin
                    state_next = HOLD_DATA_HANDSHAKE_OR_STATIC_INTERVAL;
                end
            end

            default: state_next = CAPTURE_DATA;
        endcase
    end

    property holds_data_stable;
        @(posedge clk_i) disable iff (!rst_assert_ni) (state_curr == HOLD_DATA_HANDSHAKE_OR_STATIC_INTERVAL) |-> tb4_data_if.data == tracked_data;
    endproperty

    assert property (holds_data_stable)
        else $error("Assertion Failed: holds_data_stable");
endmodule

