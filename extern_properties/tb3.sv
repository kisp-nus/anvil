module tb3 #(
    parameter int lifetime = 3,
    parameter int static_interval = 2
) (
    input  logic       clk_i,
    input  logic       rst_ni,
    tb3_control_if tb3_control_if,
    data_if tb3_data_if
);
    logic rst_assert_ni;

    typedef enum logic [1:0] {IDLE, HOLD_STATIC, HOLD_LIFETIME} state_t;
    state_t state_curr, state_next;

    int static_remaining;
    int lifetime_remaining;
    logic [$bits(tb3_data_if.data)-1:0] tracked_data;

    always_ff @(posedge clk_i or negedge rst_ni) begin
        if (!rst_ni) begin
            state_curr <= IDLE;
            rst_assert_ni <= 1'b0;
            static_remaining <= 0;
            lifetime_remaining <= 0;
            tracked_data <= 0;
        end else begin
            state_curr <= state_next;
            rst_assert_ni <= 1'b1;

            case (state_curr)
                IDLE: begin
                    if (tb3_control_if.valid) begin
                        tracked_data <= tb3_data_if.data;
                        static_remaining <= static_interval - 1;
                        lifetime_remaining <= 0;
                    end
                end

                HOLD_STATIC: begin
                    if (static_remaining > 0) begin
                        static_remaining <= static_remaining - 1;
                    end else begin
                        lifetime_remaining <= lifetime - 1;
                    end
                end

                HOLD_LIFETIME: begin
                    if (lifetime_remaining > 0) begin
                        lifetime_remaining <= lifetime_remaining - 1;
                    end else if (tb3_control_if.valid) begin
                        tracked_data <= tb3_data_if.data;
                        static_remaining <= static_interval - 1;
                        lifetime_remaining <= 0;
                    end
                end

                default: begin
                    static_remaining   <= 0;
                    lifetime_remaining <= 0;
                end
            endcase
        end
    end

    always_comb begin
        state_next = state_curr;

        case (state_curr)
            IDLE: begin
                if (tb3_control_if.valid) begin
                    state_next = HOLD_STATIC;
                end else begin
                    state_next = IDLE;
                end
            end

            HOLD_STATIC: begin
                if (static_remaining == 0) begin
                    state_next = HOLD_LIFETIME;
                end else begin
                    state_next = HOLD_STATIC;
                end
            end

            HOLD_LIFETIME: begin
                if (lifetime_remaining == 0) begin
                    if (tb3_control_if.valid) begin
                        state_next = HOLD_STATIC;
                    end else begin
                        state_next = IDLE;
                    end
                end else begin
                    state_next = HOLD_LIFETIME;
                end
            end

            default: begin
                state_next = IDLE;
            end
        endcase
    end

    property holds_data_stable;
        @(posedge clk_i) disable iff (!rst_assert_ni) (state_curr == HOLD_STATIC || state_curr == HOLD_LIFETIME) |-> (tb3_data_if.data == tracked_data);
    endproperty

    assert property (holds_data_stable)
        else $error("data changed during static hold window");

endmodule
