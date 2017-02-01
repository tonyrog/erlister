
-ifndef(__ERLISTER_HRL__).
-define(__ERLISTER_HRL__, true).

-type type_t() :: boolean|unsigned8|unsigned16|unsigned32|
		  integer8|integer16|integer32.
-type class_t() :: in|out|def.

-record(var,
	{
	  id    :: string(),  %% name of variable
	  line  :: integer(),  %% source line
	  class :: class_t(),
	  type  :: type_t(),
	  expr  %% defining expression
	}).

-record(clock,
	{
	  id    :: string(),   %% id of variable
	  name  :: string(),   %% name of clock
	  line  :: integer(),  %% source line
	  range :: {number(),number(),number()},
	  default :: number()
	}).

-record(state,
	{
	  id :: string(),
	  line :: integer()
	}).

%% atomic machine, submachine or composed machine
-record(machine,
	{
	  name :: string(),   %% name of machine
	  line :: integer(),   %% source line where defined
	  in = [] :: [#var{}],     %% in declarations
	  def = [] :: [#var{}],    %% def declarations
	  out = [] :: [#var{}],    %% out declarations
	  clocks, %% clocks declarations
	  %% machine/atomic machine
	  states, %% state declarations
	  trans,  %% transitions
	  %% composed machine
	  submachines, %% submachines declarations
	  machines = [] :: [#machine{}]   %% list of submachines
	}).

-endif.
