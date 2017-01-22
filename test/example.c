typedef unsigned char  digital_t;
typedef unsigned short analog_t;
typedef unsigned long  timer_t;
typedef unsigned char  state_t;

enum {
    IN_example_x1,
    IN_example_x2,
    IN_example_x3,
    example_NUM_INPUT
};

enum {
    OUT_example_z1,
    example_NUM_OUTPUT
};

enum {
    example_state1,
    example_state2,
    example_state3
};

typedef struct {
    digital_t input[example_NUM_INPUT];
    digital_t output[example_NUM_OUTPUT];
    timer_t   clk_example_T;
    state_t   example_state;
} example_ctx_t;

extern int timer_init(timer_t* tp);
extern int timer_start(timer_t* tp);
extern int timer_stop(timer_t* tp);
extern int timer_timeout(timer_t* tp);

void init(example_ctx_t* ctx)
{
    ctx->input[IN_example_x1] = 0;
    ctx->input[IN_example_x2] = 0;
    ctx->input[IN_example_x3] = 0;
    ctx->output[OUT_example_z1] = 0;
    timer_init(&ctx->clk_example_T);
    ctx->example_state = example_state1;
}

void final(example_ctx_t* ctx)
{
    timer_stop(&ctx->clk_example_T);
}

void machine(example_ctx_t* ctx)
{
    digital_t example_x1;
    digital_t example_x2;
    digital_t example_x3;
    digital_t example_y1;

    example_x1 = ctx->input[IN_example_x1];
    example_x2 = ctx->input[IN_example_x2];
    example_x3 = ctx->input[IN_example_x3];
    example_y1 = (example_x2) && (example_x1);
    switch(ctx->example_state) {
    case example_state1:
        if ((timer_timeout(&ctx->clk_example_T)) && (!(example_x2))) {
            ctx->example_state = example_state3;
            break;
        }
        if ((!(example_x3)) && (!(example_y1))) {
            ctx->example_state = example_state2;
            break;
        }
        break;
    case example_state2:
        if (example_y1) {
            ctx->example_state = example_state1;
            break;
        }
        break;
    case example_state3:
        if (example_x3) {
            ctx->example_state = example_state2;
            timer_start(&ctx->clk_example_T);
            break;
        }
        break;
    default: break;
    }
    ctx->output[OUT_example_z1] = (!((ctx->example_state == example_state1))) && (example_y1);
}
