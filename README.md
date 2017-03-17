#Erlister

State machine language heavily inspired by ALISTER.

## The language

Erlister is a language to define state machines and
their behaviour. Two types of state machines can
be specified, atomic machines and composed machines.
Each machine consists of a number of sections and 
they are:

- **in** parameter section
- **def** variable section
- **out** variable section
- **clock** section
- **states** section
- **trans** section

The **in** parameter section is normally a list of **in** parameter names
but an **in** parameter may be defined by a predicate formula that operates
over an external domain of configuration data. This can be used to 
instantiate the machine.

    in x, y, z;
    in do_interval = ALL output
              	        have_interval_hardware(output) ||
                        can_simulate_interval(output);

The **def** variables are a list of variables with corresponding
formula defining the variable. It can be thought of as a
shortcut for the formula. Only input parameters may be used in a the
**def** formula.

    def a = x && y || !z;
    def b = x || y || z;

Timers are introduced with the **clocks** directive. It is a
list of named timers that are used to specify temporal aspects of the
machine.

    clocks on_timer = atime [1-5, 0.1] 2;

The name of the timer declare the range the timer have, from configuration
data, it's minimal step value and the default value.

The **out** section is a list of output assignments. Each
output is given by a formula that may also contain timeout expressions
and state names. When a state name is used in a formula the meaning is
that the machine is in that state.

    out active_light = !off;

States must first be declared by giving a list of all the possible 
state names

    states off, on, activate, on_wait, off_wait;

State transitions are given with the **trans** directive

    trans

    off: off_wait && onoff_released,
         activate && onoff_released;

    activate: off onoff_pressed start(on_timer);
 
    on_wait: actvate onoff_pressed &&!timeout(on_timer);

    on_precharge: on_wait timeout(on_timer) && onoff_releases;

The trans directive should be read something like: to change
state to the state on the right hand side of the colon, the machine
must be in the state given on the left hand side and the formula
must true. When the transition is done optional timers listed are
started.

# Composed machines

When multiple states are needed the sub machines is a handy way
to introduce that. A composed machine do not have a transition
table it self but each submachine may have one.

Sub machines are introduced with the declaration 

    submachines alternate, springback;

The machines are evaluated in this order (may change.)

Each sub machine is like an atomic machine.
The input section uses global input parameters and 
**def** variables from the machine declaration.
The input formulas may also use output values, timeouts and states from 
sibling sub machines.

    machine interval;

    submachines alternate, springback;

    in button;
    out value = springback.value && alternate.value ||
	            (timeout(alternate.Tl) && timeout(alternate.Th);

    submachine springback;
      in button = button;
      out value = on;
      states off, on;
      trans
        off: on  button;
        on:  off button;

    submachine alternate;
      in enable = springback.value;
      out value = high;
      clocks Th = high_time [0-5, 0.2] 2,
             Tl = low_time  [0-5, 0.2] 2;
      states low, high;
      trans
        high: low enable && timeout(Th) start(Tl);
        low: high enable && timeout(Tl) start(Th);

# Types on variables

The following types may be used for **in**, **out**, **def** and
**param** declarations ( boolean is the default ) :

    boolean
    unsigned8
    unsigned16
    unsigned32
    integer8
    integer16
    integer32

and are used like

    unsigned16 in sensor_value;

# General syntax of the formula

The following BNF gives the syntax for a general formula,
note that not all parts are valid in all sections.

    <digit> := "0".."9"
    <letter> := "a".."z"|"A".."Z"|"_"
    <identifier> := <letter>(<letter>|<digit>)*
	<var> := <identifier> | <identifier> '.' <identifier>
    <number> := <digit>+
    <constant> := "true" | "false" | <number>
    <arg> := <number> | <identifier>
	<formula> := <expr>

    <expr> ::= <constant>
              | <var>
              | <identifier> "(" [ <expr> ( "," <expr> )* ] ")"
              | "-" <expr>
              | "!" <expr>
              | "~" <expr>
			  | "(" <expr> ")"
              | <expr> "+" <expr>
              | <expr> "-" <expr>
              | <expr> "*" <expr>
              | <expr> "/" <expr>
              | <expr> "%" <expr>
              | <expr> "<" <expr>
              | <expr> "<=" <expr>
              | <expr> ">=" <expr>
              | <expr> ">" <expr>
              | <expr> "==" <expr>
              | <expr> "!=" <expr>
	          | <expr> "&" <expr>
	          | <expr> "|" <expr>
	          | <expr> "^" <expr>
              | <expr> "&&" <expr>
              | <expr> "||" <expr>
              | <expr> "->" <expr>
              | <expr> "<->" <expr>
              | <expr> ? <expr> : <expr>
              | "ALL" <identifier> <expr>
              | "SOME" <identifier> <expr>
