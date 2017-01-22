typedef unsigned char  digital_t;
typedef unsigned short analog_t;
typedef unsigned long  timer_t;
typedef unsigned char  state_t;

enum {
    IN_SIGNAL_goahead,
    IN_SIGNAL_lg,
    IN_SIGNAL_lr,
    SIGNAL_NUM_INPUT
};

enum {
    OUT_SIGNAL_g,
    OUT_SIGNAL_r,
    OUT_GREEN_ok,
    OUT_GREEN_error,
    OUT_RED_error,
    SIGNAL_NUM_OUTPUT
};


enum {
    GREEN_off,
    GREEN_on
};

enum {
    RED_on,
    RED_off
};

typedef struct {
    digital_t input[SIGNAL_NUM_INPUT];
    digital_t output[SIGNAL_NUM_OUTPUT];
    timer_t   clk_GREEN_T1;
    timer_t   clk_RED_T2;
    state_t   GREEN_state;
    state_t   RED_state;
} SIGNAL_ctx_t;

extern int timer_init(timer_t* tp);
extern int timer_start(timer_t* tp);
extern int timer_stop(timer_t* tp);
extern int timer_timeout(timer_t* tp);

void init(SIGNAL_ctx_t* ctx)
{
    ctx->input[IN_SIGNAL_goahead] = 0;
    ctx->input[IN_SIGNAL_lg] = 0;
    ctx->input[IN_SIGNAL_lr] = 0;
    ctx->output[OUT_SIGNAL_g] = 0;
    ctx->output[OUT_SIGNAL_r] = 0;
    timer_init(&ctx->clk_GREEN_T1);
    timer_init(&ctx->clk_RED_T2);
    ctx->GREEN_state = GREEN_off;
    ctx->RED_state = RED_on;
}

void final(SIGNAL_ctx_t* ctx)
{
    timer_stop(&ctx->clk_GREEN_T1);
    timer_stop(&ctx->clk_RED_T2);
}

void machine(SIGNAL_ctx_t* ctx)
{
    digital_t SIGNAL_goahead;
    digital_t SIGNAL_lg;
    digital_t SIGNAL_lr;
    digital_t GREEN_request_on;
    digital_t GREEN_light_control;
    digital_t GREEN_error_in;
    digital_t RED_request_on;
    digital_t RED_light_control;
    digital_t RED_error_in;
    digital_t RED_gok;

    SIGNAL_goahead = ctx->input[IN_SIGNAL_goahead];
    SIGNAL_lg = ctx->input[IN_SIGNAL_lg];
    SIGNAL_lr = ctx->input[IN_SIGNAL_lr];
    GREEN_request_on = ctx->input[IN_SIGNAL_goahead];
    GREEN_light_control = ctx->input[IN_SIGNAL_lg];
    GREEN_error_in = ctx->output[OUT_RED_error];
    switch(ctx->GREEN_state) {
    case GREEN_off:
        if ((GREEN_error_in) || (!(GREEN_request_on))) {
            ctx->GREEN_state = GREEN_on;
            break;
        }
        break;
    case GREEN_on:
        if ((!(GREEN_error_in)) && (GREEN_request_on)) {
            ctx->GREEN_state = GREEN_off;
            timer_start(&ctx->clk_GREEN_T1);
            break;
        }
        break;
    default: break;
    }
    ctx->output[OUT_GREEN_ok] = !(GREEN_light_control) || (GREEN_request_on);
    ctx->output[OUT_GREEN_error] = (!(GREEN_error_in)) && ((timer_timeout(&ctx->clk_GREEN_T1)) && ((!(GREEN_light_control)) && (GREEN_request_on)));
    RED_request_on = !(ctx->input[IN_SIGNAL_goahead]);
    RED_light_control = ctx->input[IN_SIGNAL_lr];
    RED_error_in = ctx->output[OUT_GREEN_error];
    RED_gok = ctx->output[OUT_GREEN_ok];
    switch(ctx->RED_state) {
    case RED_off:
        if ((RED_gok) && (!(RED_request_on))) {
            ctx->RED_state = RED_on;
            timer_start(&ctx->clk_RED_T2);
            break;
        }
        break;
    case RED_on:
        if ((RED_error_in) || (RED_request_on)) {
            ctx->RED_state = RED_off;
            break;
        }
        break;
    default: break;
    }
    ctx->output[OUT_RED_error] = (!(RED_error_in)) && ((timer_timeout(&ctx->clk_RED_T2)) && ((RED_light_control) && ((ctx->RED_state == RED_off))));
    ctx->output[OUT_SIGNAL_g] = (ctx->GREEN_state == GREEN_on);
    ctx->output[OUT_SIGNAL_r] = (ctx->RED_state == RED_on);
}
