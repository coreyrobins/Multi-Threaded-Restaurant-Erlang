% Corey Robins. 111185399.

-module(simulation).

% Entry point for simulation: will carry out the simulation with the given
% parameters, print the logged events, and validate them.  If validation
% fails, an error is signaled.
-export([start/4]).

% standard simulation parameters for food, machines
%machine_map() ->
%    dict:from_list([{grill, {burger, 500}},
%                    {frier, {fries, 250}},
%                    {soda_fountain, {coke, 100}}]).

%food_map() ->
%    dict:fold(fun(Machine, {Food, _Time}, Dict) -> 
%                      dict:store(Food, Machine, Dict) end,
%              dict:new(),
%              machine_map()).

%foods() ->
%    dict:fetch_keys(food_map()).

% semaphore for restaurant tables
init_tables(NumTables) ->
    register(restaurant, semaphore:make_semaphore(NumTables)), ok.

clear_tables() -> unregister(restaurant), ok.

% MACHINES

% starts up the machines.  Each machine is a process running machine_loop,
% below.
init_machines(MachineCapacity) ->
    % Each machine has a semaphore associated with it, with as many permits as the
    % MachineCapacity.
    %
    % I then log the event that the machine is starting
    %
    % Finally I spawn a new machine process (one process per machine) which runs the machine_loop function

    register(grill_semaphore, semaphore:make_semaphore(MachineCapacity)),
    simulation_event:machine_starting(grill, burger, MachineCapacity),
    spawn(fun() -> machine_loop(grill) end),

    register(frier_semaphore, semaphore:make_semaphore(MachineCapacity)),
    simulation_event:machine_starting(frier, fries, MachineCapacity),
    spawn(fun() -> machine_loop(frier) end),

    register(soda_semaphore, semaphore:make_semaphore(MachineCapacity)),
    simulation_event:machine_starting(soda_fountain, coke, MachineCapacity),
    spawn(fun() -> machine_loop(soda_fountain) end),

    ok. 

% One process for each machine, running this loop.  The Machine argument
% is the atom that names the machine (i.e., grill, frier, etc.).  It
% should receive requests (from Cooks) to cook food.  Once the machine
% has available capacity, it responds to the cook (with a message),
% and then starts cooking.  Once the cooking is done, to sends another
% message to say that it's finished.  It should also accept a message
% telling it to shut down (at the end of the simulation).
machine_loop(Machine) -> 
    % First, I break up the different cases based on the type of machine

    case Machine of
        grill ->

            % For each machine, there is a blockingqueue on which each cook
            % will put its PID, or the message will just be 'cancel', meaning
            % that the machine should log that it's ending, send to the main
            % process that it's terminating, and then exit the loop
            %
            % So at this point, the Machine first tries to take something
            % from its blockingqueue, blocking until it has an element
            %
            % If the message is not 'cancel', then the machine tries to take a permit
            % to start cooking the food, logs that it's started to cook the food,
            % sleep for the specified time, log that it's finished cooking the food,
            % send to the Cook that it has finished cooking the food, add another permit
            % to the semaphore, and then re-enter the loop

            Message = blockingqueue:take(grill_queue),
            case Message of 
                cancel ->
                    simulation_event:machine_ending(Machine),
                    sim_pid ! {Machine, terminate};
                {ok, Cook} ->
                    semaphore:take(grill_semaphore),
                    simulation_event:machine_cooking_food(grill, burger),
                    timer:sleep(500),
                    simulation_event:machine_done_food(grill, burger),
                    Cook ! {Machine, finish},
                    semaphore:put(grill_semaphore),
                    machine_loop(Machine)
            end;
        frier ->
            Message = blockingqueue:take(frier_queue),
            case Message of
                cancel ->
                    simulation_event:machine_ending(Machine),
                    sim_pid ! {Machine, terminate};
                {ok, Cook} ->
                    semaphore:take(frier_semaphore),
                    simulation_event:machine_cooking_food(frier, fries),
                    timer:sleep(250),
                    simulation_event:machine_done_food(frier, fries),
                    Cook ! {Machine, finish},
                    semaphore:put(frier_semaphore),
                    machine_loop(Machine)
            end;
        soda_fountain ->
            Message = blockingqueue:take(soda_queue),
            case Message of 
                cancel ->
                    simulation_event:machine_ending(Machine),
                    sim_pid ! {Machine, terminate};
                {ok, Cook} ->
                    semaphore:take(soda_semaphore),
                    simulation_event:machine_cooking_food(soda_fountain, coke),
                    timer:sleep(100),
                    simulation_event:machine_done_food(soda_fountain, coke),
                    Cook ! {Machine, finish},
                    semaphore:put(soda_semaphore),
                    machine_loop(Machine)
            end
    end,
    ok.

% Tells all the machines to shut down.
stop_machines() -> 
    % I just cancel each of the blockingqueues, 
    % but then wait to get the message back from each machine that it has terminated
    % before the function returns

    blockingqueue:cancel(grill_queue),
    blockingqueue:cancel(frier_queue),
    blockingqueue:cancel(soda_queue),
    receive {grill, terminate} -> ok end,
    receive {frier, terminate} -> ok end,
    receive {soda_fountain, terminate} -> ok end,
    ok.

% COOKS

% Starts up the cooks.  Each cook is a process running cook_loop,
% below.  You may wish to return the list of Pids for the processes
% you created, since that will help with coordinating the simulation
% completion.
init_cooks(NumCooks, Pids, CookNum) ->

    % This function initializes a new Cook process, based on the number of cooks
    % specified in the param
    %
    % It generates its name, logs that the cook is starting, 
    % spawns a new Cook process that executes cook_loop,
    % and then calls init_cooks, adding its recently generated
    % PID to the list of PIDs, and also adds 1 to CookNum
    % so that the program knows how many cooks to generate
    %
    % If it's generated enough cooks, then it returns the list of Pids

    if 
        CookNum < NumCooks ->
            CookName = list_to_atom(string_format("cook~p",[CookNum])),
            simulation_event:cook_starting(CookName),
            Self = self(),
            S = spawn(fun() -> cook_loop(Self, CookName) end),
            init_cooks(NumCooks, [S|Pids], CookNum + 1);
        true ->
            Pids
    end.
    
init_cooks(NumCooks) -> 
    % The first call (from start()) goes to this function,
    % which calls an overloaded version, which initializes an empty list, and 0,
    % and then returns the list of CookPids (see above)
    Pids = init_cooks(NumCooks, [], 0),
    Pids.

% The cook loop takes two arguments: SimPid is the Pid of the main process,
% and Cook is the atom that names the cook.  The cook should wait for
% orders from eaters and then process them.  An order consists of an
% order number (unique to each order), a list of food atoms, and the Pid
% of the requesting eater.  The cook processes the order by finding the
% right machines to cook each food item, submitting the cook requests to
% those machines, and then waiting for all cooking to complete, sending
% confirmation back to the eater when done.  You may wish to use a
% blockingqueue to receive orders from eaters.  In this case, you will
% wait to handle the cancel message when the queue is shut down, 
% sending a message SimPid to indicate the cook has exited.
cook_loop(SimPid,Cook) ->

    % Each cook keeps executing this loop, waiting for an eater to be available to process,
    % processes that eater, and then continues in the loop
    %
    % First, each cook blocks on the 'eaters' blockingqueue until an eater is put in it
    % 
    % The Cook then takes the Message from the take() function call, and if it has taken
    % an eater, then it logs that it has received the order, calls cook_loop_eater() (see below),
    % and then loops again
    % 
    % If it is a cancel message, then it logs that it's ending, and sends the message
    % that it has terminated to the main process

    Message = blockingqueue:take(eaters),
    case Message of 
        {ok, {EaterPid,OrderNum,Order}} ->
            simulation_event:cook_received_order(Cook,OrderNum,Order),
            cook_loop_eater(SimPid,Cook,{EaterPid,OrderNum,Order}),
            cook_loop(SimPid,Cook);
        cancel ->
            simulation_event:cook_ending(Cook),
            SimPid ! {self(), terminate},
            ok
    end,
    ok.

cook_loop_eater(SimPid,Cook,{EaterPid,OrderNum,Order}) ->

    % In this function, the cook logs that it has started each food,
    % and then puts its own PID into each of the 3 queues, thus telling
    % the proper Machine to start up cooking that food, and then send
    % over the message to this Cook that it has finished each food
    %
    % The Cook then calls cook_wait_for_food() (see below)

    simulation_event:cook_started_food(Cook,OrderNum,burger),
    blockingqueue:put(grill_queue,self()),
    simulation_event:cook_started_food(Cook,OrderNum,fries),
    blockingqueue:put(frier_queue,self()),
    simulation_event:cook_started_food(Cook,OrderNum,coke),
    blockingqueue:put(soda_queue,self()),
    cook_wait_for_food(SimPid,Cook,{EaterPid,OrderNum,Order}).

cook_wait_for_food(_SimPid,Cook,{EaterPid,OrderNum,Order}) ->

    % In this method, the Cook calls receive_and_log_finish() (see below),
    % which waits for each of the 3 foods to be finished by the correct Machine
    % (it receives a message sent to it from the Machine), then logs
    % that it has finished the order, and sends the message to the Eater that it was
    % processing that the Cook has finished the order

    lists:foreach(fun(_) -> receive_and_log_finish(Cook,OrderNum) end, Order),
    simulation_event:cook_completed_order(Cook,OrderNum),
    EaterPid ! {self(),OrderNum,complete}.

receive_and_log_finish(Cook,OrderNum) ->

    % This function is called 3 times by each Cook (see above), and simply
    % waits for a message to be sent from the Machine that it has finished cooking the food.
    %
    % Once the Cook receives this message, it logs that it has completed the food,
    % and then just returns

    receive {Machine, finish} ->
        case Machine of
            grill ->
                simulation_event:cook_finished_food(Cook,OrderNum,burger);
            frier ->
                simulation_event:cook_finished_food(Cook,OrderNum,fries);
            soda_fountain ->
                simulation_event:cook_finished_food(Cook,OrderNum,coke)
        end
    end,
    ok.

% Shut down the cooks.  If you are using a blockingqueue to implement
% the order queue, just cancel it.  The cooks will quit upon receiving
% the cancel message from blockingqueue:take().
stop_cooks(CookPids) ->  

    % First, I just cancel the 'eaters' blockingqueue to let each Cook
    % know to shut down
    %
    % However, I also have to wait for each Cook to send a message back
    % that it has finished processing its eater and has terminated.
    % That being said, I wait for each Cook to send a message back saying
    % that it has terminated. I do this by recursively calling the function
    % stop_cooks() until it has received a message from each Cook.

    blockingqueue:cancel(eaters),
    Length = lists:flatlength(CookPids),
    if 
        Length > 0 ->
            receive
                {CookPid, terminate} ->
                    stop_cooks(lists:delete(CookPid,CookPids))
            end;
        true ->
            ok
    end,
    ok.

% EATERS

% Starts up NumEaters eaters, one process for each, where the process
% will run the eat() function below before terminating.  You may wish
% to return the list of Pids for the processes you created, since that
% will help with coordinating the simulation completion.
init_eaters(NumEaters, Pids, EaterNum) ->

    % As explained below, these 2 functions behave in almost the exact
    % same way as init_cooks(), so see the explanation for that 
    % function above to understand how this works.

    if 
        EaterNum < NumEaters ->
            EaterName = list_to_atom(string_format("eater~p",[EaterNum])),
            Order = [burger,fries,coke],
            S = spawn(fun() -> eat(EaterName, Order) end),
            init_eaters(NumEaters, [S|Pids], EaterNum + 1);
        true ->
            Pids
    end.

init_eaters(NumEaters) -> 

    % This function gets called first, and then again calls 
    % an overloaded version of this function. These functions
    % work in almost the exact same way is init_cooks() so I will
    % not go into more detail here...see init_cooks() for more explanation.

    Pids = init_eaters(NumEaters, [], 0),
    Pids.

% Implements an eater, taking the Eater name (an atom) and the food
% order (a list of food atoms).  Eater attempts to get a table (use
% the restaurant semaphore created by init_tables() above), constructs
% an order number (think about using a sequence for this), places
% the order on the order queue, waits for the order to be complete,
% and then leaves the restaurant.
eat(Eater, Order) ->

    % Each eater executes this function.
    %
    % First, the eater logs that it has begun processing (waiting for a table).
    % It then blocks while waiting for the 'restaurant' sempaphore, and there
    % are as many permits available as there are tables available.
    %
    % Once the Eater obtains a permit, it logs that it has entered the restaurant,
    % generates a new OrderNumber from the 'sequence' using get_next(),
    % puts itself onto the 'eaters' blockingqueue, and finally logs the event
    % that it has placed its order.
    %
    % The Eater then waits to receive a message from the Cook that the Cook
    % has completed its order. Once the Eater receives this message, it logs
    % that they have received the order, puts its permit back into the semaphore,
    % logs that it is leaving the restaurant, sends the message that it has finished
    % eating to the main process, and terminates.

    simulation_event:eater_starting(Eater),
    semaphore:take(restaurant),

    simulation_event:eater_entered_restaurant(Eater),
    OrderNum = sequence:get_next(sequence),
    blockingqueue:put(eaters,{self(),OrderNum,Order}),

    simulation_event:eater_placed_order(Eater,OrderNum,Order),

    receive 
        {_From, OrderNum, complete} ->
            simulation_event:eater_received_order(Eater, OrderNum)
    end,

    semaphore:put(restaurant),
    simulation_event:eater_leaving_restaurant(Eater),
    sim_pid ! {self(), eater_done},
    ok.

% Blocks until all processes in EaterPids have terminated (you will
% have to arrange for the eaters to send a message back to the main
% process for this).
wait_for_eaters(EaterPids) ->

    % This function behaves in almost the exact same way as stop_cooks() above.
    % The only difference is that it does not cancel any blockingqueue;
    % it only waits to receive messages from Eaters that they have all finished
    % eating and have left the restaurant.

    Length = lists:flatlength(EaterPids),
    if 
        Length > 0 ->
            receive
                {EaterPid, eater_done} ->
                    wait_for_eaters(lists:delete(EaterPid,EaterPids))
            end;
        true ->
            ok
    end,
    ok.

% simulation entry point

start(NumEaters, NumTables, NumCooks, MachineCapacities) ->
    simulation_event:start_logger(),
    simulation_event:simulation_starting(NumEaters,NumCooks,NumTables,MachineCapacities),
    init_queues_mainpid(), % this function initializes the global vars --> see below
    init_machines(MachineCapacities),
    CookPids = init_cooks(NumCooks), % keep track of CookPids for later
    init_tables(NumTables),
    EaterPids = init_eaters(NumEaters), % keep track of EaterPids for later
    wait_for_eaters(EaterPids),
    stop_cooks(CookPids),
    stop_machines(),
    clear_tables(),
    clear_queues(), % unregister all the other vars
    simulation_event:simulation_ending(),
    simulation_event:print(),
    Events = simulation_event:flush(),
    validate:check(Events),
    simulation_event:end_logger(),
    ok.

% functions you might find useful

%collect_n(0, _F) -> [];
%collect_n(N, F) when N > 0 -> [F(N-1) | collect_n(N-1, F)].

%iter_n(0, _F) -> ok;
%iter_n(N, F) when N > 0 ->
%    F(N-1),
%    iter_n(N-1, F).

string_format(Format,Params) ->
    lists:flatten(io_lib:format(Format,Params)).

init_queues_mainpid() ->
    register(sim_pid,self()), % main process PID
    register(sequence,sequence:make_sequence()), % sequence to generate OrderNum for Eater
    register(eaters, blockingqueue:make_queue()), % communcation for Eaters --> Cooks
    register(grill_queue, blockingqueue:make_queue()), % communication for Cooks --> Grill Machine
    register(frier_queue, blockingqueue:make_queue()), % communication for Cooks --> Frier Machine
    register(soda_queue, blockingqueue:make_queue()), % communication for Cooks --> Soda_Fountain Machine
    ok.

clear_queues() ->

    % This function just unregisters all of the global vars that were
    % registered at some point in the program.

    unregister(sim_pid),
    unregister(sequence),
    unregister(eaters),
    unregister(grill_queue),
    unregister(frier_queue),
    unregister(soda_queue),
    unregister(grill_semaphore),
    unregister(frier_semaphore),
    unregister(soda_semaphore),
    ok.

