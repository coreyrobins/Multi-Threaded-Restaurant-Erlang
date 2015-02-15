-module(simulation_event).

% API for starting the logger, printing the events logged so far,
% validating the events (at the end of a simulation),
% and stopping the logger, respectively.
-export([start_logger/0, print/0, flush/0, end_logger/0]).

% API for logging events
-export([simulation_starting/4,simulation_ending/0]).
-export([eater_starting/1, eater_entered_restaurant/1, eater_placed_order/3, 
	 eater_received_order/2, eater_leaving_restaurant/1]).
-export([cook_starting/1, cook_received_order/3, cook_started_food/3, 
	 cook_finished_food/3, cook_completed_order/2, cook_ending/1]).
-export([machine_starting/3, machine_cooking_food/2, machine_done_food/2, 
	 machine_ending/1]).

% Server callbacks
-export([init/0, handle_cast/2, handle_call/2]).

init() -> [].
handle_call(print,Events) ->
    lists:foreach(fun(S) -> io:format("~s~n",[event_to_string(S)]) end, lists:reverse(Events)),
    {ok,Events};
handle_call(flush,Events) ->
    {lists:reverse(Events),[]}.
handle_cast({log,Event}, Events) -> 
	%io:format("EVENTS: ~p~n",[Events]),
    [Event|Events].

% API for starting, stopping, validating, and printing events

start_logger() ->   register(logger,server:start(simulation_event)).
end_logger() ->     unregister(logger).
print() ->          server:call(logger,print), ok.
flush() ->          server:call(logger,flush).

% API for logging particular events

simulation_starting(NEaters,NCooks,NTables,Capacity) ->
    server:cast(logger,{log,{simulation_starting,
			     [NEaters,NCooks,NTables,Capacity]}}).
simulation_ending() ->
    server:cast(logger,{log,{simulation_ending,ok}}).

eater_starting(Eater) -> 
    server:cast(logger,{log,{eater_starting,Eater}}).
eater_entered_restaurant(Eater) -> 
    server:cast(logger,{log,{eater_entered,Eater}}).
eater_placed_order(Eater,Num,Order) -> 
    server:cast(logger,{log,{eater_ordered,[Eater,Num,Order]}}).
eater_received_order(Eater,Num) -> 
    server:cast(logger,{log,{eater_received_order,[Eater,Num]}}).
eater_leaving_restaurant(Eater) -> 
    server:cast(logger,{log,{eater_leaving,Eater}}).

cook_starting(Cook) ->
    server:cast(logger,{log,{cook_starting,Cook}}).
cook_received_order(Cook,Num,Order) ->
    server:cast(logger,{log,{cook_received_order,[Cook,Num,Order]}}).
cook_started_food(Cook,Num,Food) ->
    server:cast(logger,{log,{cook_started_food,[Cook,Num,Food]}}).
cook_finished_food(Cook,Num,Food) ->
    server:cast(logger,{log,{cook_finished_food,[Cook,Num,Food]}}).
cook_completed_order(Cook,Num) ->
    server:cast(logger,{log,{cook_completed_order,[Cook,Num]}}).
cook_ending(Cook) ->
    server:cast(logger,{log,{cook_ending,Cook}}).

machine_starting(Machine,Food,Capacity) ->
    server:cast(logger,{log,{machine_starting,[Machine,Food,Capacity]}}).
machine_cooking_food(Machine,Food) ->
    server:cast(logger,{log,{machine_cooking,[Machine,Food]}}).
machine_done_food(Machine,Food) ->
    server:cast(logger,{log,{machine_done_food,[Machine,Food]}}).
machine_ending(Machine) ->
    server:cast(logger,{log,{machine_ending,Machine}}).

% helper functions

string_of_atom_list([]) -> "";
string_of_atom_list([X]) -> atom_to_list(X);
string_of_atom_list([H|T]) ->
    atom_to_list(H) ++ ", " ++ (string_of_atom_list(T)).

string_format(Format,Params) ->
    lists:flatten(io_lib:format(Format,Params)).

event_to_string(Event) ->
    case Event of
	{simulation_starting,Params} ->
	    string_format("Starting simulation: ~p eaters; ~p cooks; ~p tables; machine capacity ~p.",
				 Params);
	{simulation_ending,ok} -> "Simulation ended.";
	{eater_starting,Eater} ->
	    atom_to_list(Eater) ++ " going to restaurant.";
	{eater_entered,Eater} ->
	    atom_to_list(Eater) ++ " entered restaurant.";
	{eater_ordered,[Eater,Num,Order]} ->
	    string_format("~s placing order ~p: ~s.",
				 [atom_to_list(Eater),
				 Num,
				 string_of_atom_list(Order)]);
	{eater_received_order,[Eater,Num]} ->
	    string_format("~s received order ~p.",
				 [atom_to_list(Eater),
				 Num]);
	{eater_leaving,Eater} ->
	    atom_to_list(Eater) ++ " leaving restaurant.";
	{cook_starting,Cook} ->
	    atom_to_list(Cook) ++ " reporting for work.";
	{cook_received_order,[Cook,Num,Order]} ->
	    string_format("~s starting order ~p: ~s",
				 [atom_to_list(Cook),
				 Num,
				 string_of_atom_list(Order)]);
	{cook_started_food,[Cook,Num,Food]} ->
	    string_format("~s starting food ~s for order ~p.",
				 [atom_to_list(Cook),
				 atom_to_list(Food),
				 Num]);
	{cook_finished_food,[Cook,Num,Food]} ->
	    string_format("~s finished food ~s for order ~p.",
				 [atom_to_list(Cook),
				 atom_to_list(Food),
				 Num]);
	{cook_completed_order,[Cook,Num]} ->
	    string_format("~s completed order ~p.",
				 [atom_to_list(Cook),
				 Num]);
	{cook_ending,Cook} ->
	    atom_to_list(Cook) ++ " going home for the night.";
	{machine_starting,[Machine,Food,Capacity]} ->
	    string_format("~s starting up for making ~s; ~p.",
				 [atom_to_list(Machine),
				  atom_to_list(Food),
				  Capacity]);
	{machine_cooking,[Machine,Food]} ->
	    atom_to_list(Machine) ++ " cooking " ++ atom_to_list(Food) ++ ".";
	{machine_done_food,[Machine,Food]} ->
	    atom_to_list(Machine) ++ " completed " ++ atom_to_list(Food) ++ ".";
	{machine_ending,Machine} ->
	    atom_to_list(Machine) ++ " shutting down.";
        _ -> io:format("unknown event!~n"), "unknown"
    end.
	
