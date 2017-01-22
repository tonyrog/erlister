typedef unsigned char  digital_t;
typedef unsigned short analog_t;
typedef unsigned long  timer_t;
typedef unsigned char  state_t;

enum {
    IN_interval_button,
    interval_NUM_INPUT
};

enum {
    OUT_interval_value,
    OUT_springback_value,
    OUT_alternate_value,
    interval_NUM_OUTPUT
};


enum {
    springback_off,
    springback_on
};

enum {
    alternate_low,
    alternate_high
};

typedef struct {
    digital_t input[interval_NUM_INPUT];
    digital_t output[interval_NUM_OUTPUT];
    timer_t   clk_alternate_Th;
    timer_t   clk_alternate_Tl;
    state_t  st_springback;
    state_t  st_alternate;
} interval_ctx_t;

extern int timer_init(timer_t* tp);
extern int timer_start(timer_t* tp);
extern int timer_stop(timer_t* tp);
extern int timer_timeout(timer_t* tp);

void init(interval_ctx_t* ctx)
{
    ctx->input[IN_interval_button] = 0;
    ctx->output[OUT_interval_value] = 0;
    timer_init(&ctx->clk_alternate_Th);
    timer_init(&ctx->clk_alternate_Tl);
    ctx->st_springback = springback_off;
    ctx->st_alternate = alternate_low;
}

void final(interval_ctx_t* ctx)
{
    timer_stop(&ctx->clk_alternate_Th);
    timer_stop(&ctx->clk_alternate_Tl);
}

void machine(interval_ctx_t* ctx)
{
    digital_t interval_button;
    digital_t springback_button;
    digital_t alternate_enable;

    interval_button = ctx->input[IN_interval_button];
    springback_button = ctx->input[IN_interval_button];
    switch(ctx->st_springback) {
    case springback_off:
        if (springback_button) {
            ctx->st_springback = springback_on;
            break;
        }
        break;
    case springback_on:
        if (springback_button) {
            ctx->st_springback = springback_off;
            break;
        }
        break;
    default: break;
    }
    ctx->output[OUT_springback_value] = (ctx->st_springback == springback_on);
    alternate_enable = ctx->output[OUT_springback_value];
    switch(ctx->st_alternate) {
    case alternate_high:
        if ((timer_timeout(&ctx->clk_alternate_Tl)) && (alternate_enable)) {
            ctx->st_alternate = alternate_low;
            timer_start(&ctx->clk_alternate_Th);
            break;
        }
        break;
    case alternate_low:
        if ((timer_timeout(&ctx->clk_alternate_Th)) && (alternate_enable)) {
            ctx->st_alternate = alternate_high;
            timer_start(&ctx->clk_alternate_Tl);
            break;
        }
        break;
    default: break;
    }
    ctx->output[OUT_alternate_value] = (ctx->st_alternate == alternate_high);
    ctx->output[OUT_interval_value] = (ctx->output[OUT_alternate_value]) && (ctx->output[OUT_springback_value]);
}
