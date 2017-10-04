-type neighbour() :: {Name :: atom(), pid()}.

-record(state, {
	  %% name of this vertex
	  name :: atom(),
	  %% status of this vertex
	  status = green :: atom(),
	  %% outlist of this vertex
	  outlist = [] :: [neighbour()],
	  %% inlist of this vertex
	  inlist = [] :: [neighbour()],
	  %% the memory map, default empty
	  memory = #{} :: map()}).

-type state() :: #state{}.
