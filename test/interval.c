// C code generated from interval.em
typedef unsigned char  digital_t;
typedef unsigned short analog_t;
typedef unsigned long  timer_t;
typedef unsigned char  state_t;

typedef enum {
  interval_button,
  interval_NUM_INPUT
} interval_input_t;

typedef enum {
  interval_value,
  alternate_value,
  springback_value,
  interval_NUM_OUTPUT
} interval_output_t;

typedef enum {
  alternate_Tl,
  alternate_Th,
  interval_NUM_CLOCKS
} interval_clocks_t;

typedef enum {
  alternate_low,
  alternate_high
} alternate_state_t;

typedef enum {
  springback_off,
  springback_on
} springback_state_t;

typedef enum {
  alternate_state,
  springback_state,
  interval_NUM_MACHINES
} springback_states_t;

digital_t input[interval_NUM_INPUT];
digital_t output[interval_NUM_OUTPUT];
state_t   state[interval_NUM_MACHINES];

extern int timeout(int tm);
extern int start_timer(int tm);

void machine()
{
  digital_t alternate_enable;
  digital_t springback_button;

  alternate_enable = output[springback_value];
  switch(state[alternate_state]) {
    case alternate_high:
      if ((timeout(alternate_Th)) && (alternate_enable)) {
        state[alternate_state] = alternate_low;
        start_timer(Tl);
        break;
      }
      break;
    case alternate_low:
      if ((timeout(alternate_Tl)) && (alternate_enable)) {
        state[alternate_state] = alternate_high;
        start_timer(Th);
        break;
      }
      break;
    default: break;
  }
  output[alternate_value] = (state[alternate_state] == alternate_high);
  springback_button = input[interval_button];
  switch(state[springback_state]) {
    case springback_off:
      if (springback_button) {
        state[springback_state] = springback_on;
        break;
      }
      break;
    case springback_on:
      if (springback_button) {
        state[springback_state] = springback_off;
        break;
      }
      break;
    default: break;
  }
  output[springback_value] = (state[springback_state] == springback_on);
  output[interval_value] = (output[alternate_value]) && (output[springback_value]);
}
