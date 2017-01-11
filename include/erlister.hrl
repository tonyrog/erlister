
-ifndef(__ERLISTER_HRL__).
-define(__ERLISTER_HRL__, true).

%% atomic machine, submachine or composed machine
-record(machine,
	{
	  line,   %% source line where defined
	  name,   %% name of machine
	  in,     %% in declarations
	  def,    %% def declarations
	  out,    %% out declarations
	  clocks, %% clocks declarations
	  %% machine/atomic machine
	  states, %% state declarations
	  trans,  %% transitions
	  %% composed machine
	  submachines, %% submachines declarations
	  machines   %% list of submachines
	}).

-endif.
